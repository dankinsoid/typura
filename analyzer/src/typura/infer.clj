(ns typura.infer
  (:require [typura.types :as t]
            [typura.context :as ctx]
            [typura.stubs :as stubs]
            [typura.check :as check]
            [typura.subtype :as sub]))

(defn- simplify-union
  "Subtype-aware union normalization: remove members subsumed by others."
  [t]
  (let [t (t/normalize-union t)]
    (if-not (t/union-type? t)
      t
      (let [members (vec (rest t))
            keep (reduce (fn [acc m]
                           (if (some #(and (not= % m) (sub/subtype? m %)) members)
                             acc
                             (conj acc m)))
                         []
                         members)]
        (t/normalize-union (into [:or] keep))))))

(defn- subtract-type-deep
  "Subtype-aware type subtraction. Removes all union members that are
   subtypes of `to-remove`. E.g. (subtract [:or :int :double :string] :number)
   removes :int and :double since both are subtypes of :number."
  [t to-remove]
  (cond
    (sub/subtype? t to-remove) :nothing
    (= t :any) :any
    (t/union-type? t)
    (let [remaining (remove #(sub/subtype? % to-remove) (rest t))]
      (t/normalize-union (into [:or] remaining)))
    :else t))

(defmulti infer-node
  "Infer the type of an AST node. Returns [type, updated-ctx]."
  (fn [node ctx] (:op node)))

(defmethod infer-node :default [node ctx]
  [:any ctx])

;; --- Literals ---

(defn- const-collection-type
  "Infer a type for a constant collection value."
  [val coll-type]
  (case coll-type
    :vector (if (empty? val)
              [:vector :any]
              (let [elem-types (mapv t/val->type val)
                    unified (simplify-union (into [:or] elem-types))]
                [:vector unified]))
    :set    (if (empty? val)
              [:set :any]
              (let [elem-types (mapv t/val->type val)
                    unified (simplify-union (into [:or] elem-types))]
                [:set unified]))
    :map    (if (empty? val)
              [:map-of :any :any]
              (if (every? keyword? (keys val))
                (into [:map] (mapv (fn [[k v]] [k (t/val->type v)]) val))
                (let [k-types (mapv t/val->type (keys val))
                      v-types (mapv t/val->type (vals val))]
                  [:map-of (simplify-union (into [:or] k-types))
                           (simplify-union (into [:or] v-types))])))
    :any))

(defmethod infer-node :const [node ctx]
  (let [tp (case (:type node)
             :number  (t/val->type (:val node))
             :string  :string
             :keyword :keyword
             :bool    :boolean
             :nil     :nil
             :char    :string
             :regex   :string
             :type    :any
             (:vector :map :set) (const-collection-type (:val node) (:type node))
             :any)]
    [tp ctx]))

;; --- Variables ---

(defmethod infer-node :local [node ctx]
  (let [sym (:name node)
        tp (or (ctx/lookup-binding ctx sym)
               :any)]
    [tp ctx]))

(defmethod infer-node :var [node ctx]
  (let [var-sym (-> node :var symbol)
        global (ctx/lookup-global ctx var-sym)
        tp (cond
             ;; Global is a stub fn → extract its advertised schema
             (fn? global) (or (stubs/stub-schema global) :any)
             ;; Global is a type
             global global
             ;; Fall back to core stubs
             :else (when-let [sf (stubs/lookup-stub var-sym)]
                     (or (stubs/stub-schema sf) :any)))]
    [(or tp :any) ctx]))

(defmethod infer-node :the-var [node ctx]
  (infer-node (assoc node :op :var) ctx))

;; --- Pass-through wrappers ---

(defmethod infer-node :with-meta [node ctx]
  (infer-node (:expr node) ctx))

(defmethod infer-node :do [node ctx]
  (let [ctx' (reduce (fn [c stmt] (second (infer-node stmt c)))
                     ctx
                     (:statements node))]
    (infer-node (:ret node) ctx')))

;; --- Bindings ---

(defmethod infer-node :let [node ctx]
  (let [ctx' (reduce (fn [c binding]
                       (let [[init-type c'] (infer-node (:init binding) c)]
                         (ctx/extend-binding c' (:name binding) init-type)))
                     ctx
                     (:bindings node))]
    (infer-node (:body node) ctx')))

(defmethod infer-node :loop [node ctx]
  ;; For Phase 0, treat loop like let (ignore recur)
  (let [ctx' (reduce (fn [c binding]
                       (let [[init-type c'] (infer-node (:init binding) c)]
                         (ctx/extend-binding c' (:name binding) init-type)))
                     ctx
                     (:bindings node))]
    (infer-node (:body node) ctx')))

;; --- Conditionals ---

(defn- unwrap-test-node
  "Unwrap :do wrappers to get the effective test expression.
   Macros often expand to (do side-effects... expr), so the real
   test is the :ret of the :do node."
  [node]
  (if (= :do (:op node))
    (recur (:ret node))
    node))

(defn- extract-guard-info
  "If test node is (pred? x), return {:sym local-symbol :narrows type} or nil.
   Looks through :do wrappers from macro expansion."
  [test-node]
  (let [test-node (unwrap-test-node test-node)]
    (when (= :invoke (:op test-node))
      (let [fn-node (:fn test-node)
            args (:args test-node)]
        (when (and (= :var (:op fn-node))
                   (= 1 (count args))
                   (= :local (:op (first args))))
          (let [var-sym (-> fn-node :var symbol)
                guard (stubs/lookup-guard var-sym)]
            (when (and guard (= 0 (:arg guard)))
              {:sym (:name (first args))
               :narrows (:narrows guard)})))))))

(defn- extract-test-local
  "If test node is a simple local reference, return its symbol.
   Looks through :do wrappers from macro expansion."
  [test-node]
  (let [test-node (unwrap-test-node test-node)]
    (when (= :local (:op test-node))
      (:name test-node))))

(defmethod infer-node :if [node ctx]
  (let [test-node (:test node)
        [_ ctx'] (infer-node test-node ctx)
        guard (extract-guard-info test-node)
        test-local (extract-test-local test-node)
        ;; Build narrowed contexts for then/else branches
        then-ctx (cond
                   ;; Guard predicate: narrow to guard type in then-branch
                   guard
                   (ctx/extend-binding ctx' (:sym guard) (:narrows guard))
                   ;; Truthiness: remove nil from local's type in then-branch
                   test-local
                   (let [orig (or (ctx/lookup-binding ctx' test-local) :any)]
                     (ctx/extend-binding ctx' test-local (t/remove-falsy orig)))
                   :else ctx')
        else-ctx (cond
                   ;; Guard predicate: subtract guard type in else-branch
                   guard
                   (let [orig (or (ctx/lookup-binding ctx' (:sym guard)) :any)]
                     (ctx/extend-binding ctx' (:sym guard)
                                         (subtract-type-deep orig (:narrows guard))))
                   :else ctx')
        [then-type _] (infer-node (:then node) then-ctx)
        [else-type _] (if (:else node)
                         (infer-node (:else node) else-ctx)
                         [:nil else-ctx])
        result-type (if (= then-type else-type)
                      then-type
                      (simplify-union [:or then-type else-type]))]
    [result-type ctx']))

;; --- Function invocation ---

(defn- infer-args
  "Infer types of argument nodes. Returns [arg-types-vec, updated-ctx]."
  [args ctx]
  (reduce (fn [[types c] arg]
            (let [[tp c'] (infer-node arg c)]
              [(conj types tp) c']))
          [[] ctx]
          args))

(defmethod infer-node :invoke [node ctx]
  (let [fn-node (:fn node)
        [arg-types ctx'] (infer-args (:args node) ctx)
        var-sym (when (= :var (:op fn-node))
                  (-> fn-node :var symbol))
        ;; Look up stub fn: from globals (inline annotation) or core stubs
        stub-fn (when var-sym
                  (let [g (ctx/lookup-global ctx' var-sym)]
                    (if (fn? g)
                      g
                      (stubs/lookup-stub var-sym))))
        ;; Fall back to fn type from globals or local binding
        fn-type (when-not stub-fn
                  (case (:op fn-node)
                    :var (let [g (ctx/lookup-global ctx' var-sym)]
                           (when (t/fn-type? g) g))
                    :local (let [t (ctx/lookup-binding ctx' (:name fn-node))]
                             (when (and t (t/fn-type? t)) t))
                    nil))]
    (cond
      stub-fn (stub-fn arg-types (:args node) ctx')
      (and fn-type (t/fn-type? fn-type))
      (check/apply-fn-type ctx' fn-type arg-types (:args node))
      :else [:any ctx'])))

(defmethod infer-node :static-call [node ctx]
  (let [[arg-types ctx'] (infer-args (:args node) ctx)
        stub-fn (stubs/lookup-static (:class node) (:method node))]
    (cond
      (fn? stub-fn) (stub-fn arg-types (:args node) ctx')
      (t/fn-type? stub-fn) (check/apply-fn-type ctx' stub-fn arg-types (:args node))
      :else [(or (t/tag->type (:tag node)) :any) ctx'])))

(defmethod infer-node :keyword-invoke [node ctx]
  (let [kw-node (:keyword node)
        target-node (:target node)
        [target-type ctx'] (infer-node target-node ctx)
        kw (:val kw-node)]
    (if-let [vt (check/map-get-type target-type kw)]
      [vt ctx']
      [:any ctx'])))

(defmethod infer-node :instance-call [node ctx]
  (let [[_ ctx'] (infer-args (:args node) ctx)
        tag (:tag node)]
    [(or (when (class? tag) (t/class->type tag))
         (t/tag->type tag)
         :any)
     ctx']))

;; --- Functions ---

(defmethod infer-node :fn [node ctx]
  (let [method (first (:methods node))
        params (:params method)
        ;; Fresh tvars for each param
        param-tvars (mapv (fn [_] (t/fresh-tvar)) params)
        ;; Bind params
        ctx' (reduce (fn [c [param tvar]]
                       (ctx/extend-binding c (:name param) tvar))
                     ctx
                     (map vector params param-tvars))
        ;; Infer body
        [body-type ctx''] (infer-node (:body method) ctx')
        ;; Resolve tvars
        resolved-params (mapv #(ctx/resolve-type ctx'' %) param-tvars)
        resolved-ret (ctx/resolve-type ctx'' body-type)]
    [[:=> (into [:cat] resolved-params) resolved-ret]
     ctx'']))

(defmethod infer-node :fn-method [node ctx]
  ;; Handled by :fn, but just in case
  (infer-node (:body node) ctx))

;; --- Definitions ---

(defmethod infer-node :def [node ctx]
  (let [;; Check for inline annotation {:typura/sig [:=> ...]}
        var-meta (meta (:var node))
        typura-sig (:typura/sig var-meta)
        ;; Infer body (even with annotation, for internal error checking)
        [init-type ctx'] (if (:init node)
                           (infer-node (:init node) ctx)
                           [:any ctx])
        var-sym (-> node :var symbol)
        ;; Annotation overrides inferred type: store stub fn in globals
        global-val (if (and typura-sig (t/fn-type? typura-sig))
                     (stubs/sig typura-sig)
                     init-type)
        ctx'' (ctx/extend-global ctx' var-sym global-val)]
    [(or typura-sig init-type) ctx'']))

;; --- Other nodes (Phase 0 stubs) ---

(defmethod infer-node :try [node ctx]
  (infer-node (:body node) ctx))

(defmethod infer-node :throw [node ctx]
  [:any ctx])

(defmethod infer-node :new [node ctx]
  (let [[_ ctx'] (infer-args (:args node) ctx)
        cls (:class node)]
    [(or (when (class? cls) (t/class->type cls)) :any) ctx']))

(defmethod infer-node :quote [node ctx]
  [:any ctx])

(defmethod infer-node :set! [node ctx]
  [:any ctx])

(defmethod infer-node :recur [node ctx]
  [:any ctx])

(defmethod infer-node :map [node ctx]
  (let [[key-types ctx'] (infer-args (:keys node) ctx)
        [val-types ctx''] (infer-args (:vals node) ctx')
        key-nodes (:keys node)]
    (if (empty? key-types)
      [[:map-of :any :any] ctx'']
      (if (every? #(and (= :const (:op %))
                        (= :keyword (:type %)))
                  key-nodes)
        ;; All keyword keys → structural map type
        (let [entries (mapv (fn [kn vt] [(:val kn) vt])
                            key-nodes val-types)]
          [(into [:map] entries) ctx''])
        ;; Mixed keys → homogeneous map-of type
        (let [k-union (simplify-union (into [:or] key-types))
              v-union (simplify-union (into [:or] val-types))]
          [[:map-of k-union v-union] ctx''])))))

(defmethod infer-node :vector [node ctx]
  (let [[elem-types ctx'] (infer-args (:items node) ctx)]
    (if (empty? elem-types)
      [[:vector :any] ctx']
      (let [unified (simplify-union (into [:or] elem-types))]
        [[:vector unified] ctx']))))

(defmethod infer-node :set [node ctx]
  (let [[elem-types ctx'] (infer-args (:items node) ctx)]
    (if (empty? elem-types)
      [[:set :any] ctx']
      (let [unified (simplify-union (into [:or] elem-types))]
        [[:set unified] ctx']))))
