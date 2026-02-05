(ns typura.analyzer
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [typura.infer :as infer]
            [typura.context :as ctx]
            [typura.types :as t]
            [typura.stubs :as stubs]))

;; --- Pass 1: Declaration Collection ---

(defn- collect-ast-nodes
  "Walk entire AST tree and collect nodes matching `pred`."
  [ast pred]
  (let [result (transient [])
        walk (fn walk [node]
               (when (map? node)
                 (when (pred node)
                   (conj! result node))
                 ;; Recurse into known child slots
                 (run! walk (:statements node))
                 (when-let [r (:ret node)] (walk r))
                 (when-let [b (:body node)] (walk b))
                 (when-let [t (:then node)] (walk t))
                 (when-let [e (:else node)] (walk e))
                 (when-let [i (:init node)] (walk i))
                 (run! walk (:bindings node))
                 (run! walk (:args node))
                 (run! walk (:methods node))
                 (run! walk (:catches node))
                 (when-let [f (:finally node)] (walk f))
                 (when-let [f (:fn node)] (walk f))
                 (when-let [p (:params node)] (run! walk p))
                 (run! walk (:exprs node))))]
    (walk ast)
    (persistent! result)))

(defn- collect-def-declaration
  "Extract declaration info from a :def node (protocols, annotations, multimethods)."
  [ctx node]
  (let [the-var (:var node)
        var-meta (meta the-var)
        var-sym (infer/var->sym the-var)
        typura-sig (:typura/sig var-meta)
        protocol-var (:protocol var-meta)
        var-val (try (when (and (bound? the-var) (nil? protocol-var))
                       (deref the-var))
                     (catch Exception _ nil))
        is-protocol-def (and (map? var-val) (:sigs var-val) (:on var-val))
        is-multimethod (instance? clojure.lang.MultiFn var-val)]
    (cond
      ;; Protocol definition: register protocol + reverse mapping
      is-protocol-def
      (let [methods-info (into {}
                           (map (fn [[k v]]
                                  [k {:arglists (:arglists v)}])
                                (:sigs var-val)))
            proto-iface (:on-interface var-val)]
        (-> ctx
            (ctx/register-protocol var-sym
              {:methods methods-info :interface proto-iface})
            (ctx/register-interface-mapping proto-iface var-sym)
            (ctx/extend-global var-sym var-val)))

      ;; Protocol method: register stub with protocol symbol as first param
      protocol-var
      (let [proto-name (infer/var->sym protocol-var)
            arglists (:arglists var-meta)
            arity (count (first arglists))
            param-types (into [:cat proto-name]
                              (repeat (dec arity) :any))]
        (ctx/extend-global ctx var-sym (stubs/sig [:=> param-types :any])))

      ;; Annotated defn: register stub from annotation
      (and typura-sig (t/fn-type? typura-sig))
      (ctx/extend-global ctx var-sym (stubs/sig typura-sig))

      ;; Multimethod: register variadic stub
      is-multimethod
      (ctx/extend-global ctx var-sym (stubs/sig [:=> [:cat [:* :any]] :any]))

      ;; Other defs: skip
      :else ctx)))

(defn- collect-deftype-declaration
  "Extract record/type declaration info from a :deftype node."
  [ctx node]
  (let [all-fields (:fields node)
        user-fields (filterv #(not (.startsWith (str (:name %)) "__")) all-fields)
        map-type (into [:map] (mapv (fn [f] [(keyword (:name f)) :any]) user-fields))
        ns-str (namespace (:name node))
        short-name (name (:name node))
        record-sym (symbol ns-str short-name)
        is-record (some #(= clojure.lang.IRecord %) (:interfaces node))
        java-ifaces (set (:interfaces node))
        ;; Resolve protocol symbols via reverse mapping built in sub-pass 1a
        implements (into #{}
                     (keep #(ctx/lookup-protocol-by-interface ctx %))
                     java-ifaces)]
    (if is-record
      (-> ctx
          (ctx/extend-record record-sym
            {:fields (mapv :name user-fields)
             :map-type map-type
             :implements implements
             :java-interfaces java-ifaces})
          (ctx/extend-global
            (symbol ns-str (str "->" short-name))
            (stubs/sig [:=> (into [:cat] (repeat (count user-fields) :any)) record-sym]))
          (ctx/extend-global
            (symbol ns-str (str "map->" short-name))
            (stubs/sig [:=> [:cat [:map-of :keyword :any]] record-sym])))
      ctx)))

(defn- collect-declarations
  "Pass 1: Walk top-level AST and collect type declarations into context.
   Two sub-passes: 1a) protocols/annotations/multimethods, 1b) records/types."
  [ast ctx]
  (let [def-nodes (collect-ast-nodes ast #(= :def (:op %)))
        deftype-nodes (collect-ast-nodes ast #(= :deftype (:op %)))
        ;; Sub-pass 1a: protocols, annotations, multimethods
        ctx' (reduce collect-def-declaration ctx def-nodes)
        ;; Sub-pass 1b: records (needs protocol reverse mapping from 1a)
        ctx'' (reduce collect-deftype-declaration ctx' deftype-nodes)]
    ctx''))

;; --- Entry Point ---

(defn analyze-form
  "Analyze a Clojure form and return its inferred type.
   Two-pass: collect declarations, then infer types."
  [form]
  (let [ast (ana.jvm/analyze+eval form (ana.jvm/empty-env)
              {:handle-evaluation-exception (fn [_] nil)})
        initial-ctx (ctx/make-context)
        ;; Pass 1: collect declarations
        populated-ctx (collect-declarations ast initial-ctx)
        ;; Pass 2: infer types with pre-populated context
        [inferred-type final-ctx] (infer/infer-node ast populated-ctx nil)]
    {:type (ctx/resolve-deep final-ctx inferred-type)
     :diagnostics (ctx/get-diagnostics final-ctx)
     :ctx final-ctx}))
