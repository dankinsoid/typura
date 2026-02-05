(ns typura.infer
  (:require [typura.types :as t]
            [typura.context :as ctx]
            [typura.stubs :as stubs]
            [typura.check :as check]
            [typura.subtype :as sub]))

(defn var->sym
  "Convert a Var to a clean qualified symbol.
   (symbol (str var)) produces '#'ns/name with a #' prefix — this avoids that."
  [^clojure.lang.Var v]
  (symbol (str (.name (.ns v))) (str (.sym v))))

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
  "Infer the type of an AST node. Returns [type, updated-ctx].
   `expected` is nil (synthesis) or a type (checking mode)."
  (fn [node ctx expected] (:op node)))

(defmethod infer-node :default [node ctx _expected]
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

(defmethod infer-node :const [node ctx _expected]
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

(defmethod infer-node :local [node ctx _expected]
  (let [sym (:name node)
        tp (or (ctx/lookup-binding ctx sym)
               :any)]
    [tp ctx]))

(defmethod infer-node :var [node ctx _expected]
  (let [var-sym (var->sym (:var node))
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

(defmethod infer-node :the-var [node ctx expected]
  (infer-node (assoc node :op :var) ctx expected))

;; --- Pass-through wrappers ---

(defmethod infer-node :with-meta [node ctx expected]
  (infer-node (:expr node) ctx expected))

(defmethod infer-node :do [node ctx expected]
  (let [ctx' (reduce (fn [c stmt] (second (infer-node stmt c nil)))
                     ctx
                     (:statements node))]
    (infer-node (:ret node) ctx' expected)))

;; --- Bindings ---

(defmethod infer-node :let [node ctx expected]
  (let [ctx' (reduce (fn [c binding]
                       (let [[init-type c'] (infer-node (:init binding) c nil)]
                         (ctx/extend-binding c' (:name binding) init-type)))
                     ctx
                     (:bindings node))]
    (infer-node (:body node) ctx' expected)))

(defmethod infer-node :loop [node ctx expected]
  ;; For Phase 0, treat loop like let (ignore recur)
  (let [ctx' (reduce (fn [c binding]
                       (let [[init-type c'] (infer-node (:init binding) c nil)]
                         (ctx/extend-binding c' (:name binding) init-type)))
                     ctx
                     (:bindings node))]
    (infer-node (:body node) ctx' expected)))

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
          (let [var-sym (var->sym (:var fn-node))
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

(defmethod infer-node :if [node ctx expected]
  (let [test-node (:test node)
        [_ ctx'] (infer-node test-node ctx nil)
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
        [then-type _] (infer-node (:then node) then-ctx expected)
        [else-type _] (if (:else node)
                         (infer-node (:else node) else-ctx expected)
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
            (let [[tp c'] (infer-node arg c nil)]
              [(conj types tp) c']))
          [[] ctx]
          args))

(defn- infer-args-with-expected
  "Infer types of argument nodes with expected types pushed down.
   `expected-types` is a seq of types parallel to args (may be shorter)."
  [args expected-types ctx]
  (reduce (fn [[types c] [arg exp]]
            (let [[tp c'] (infer-node arg c exp)]
              [(conj types tp) c']))
          [[] ctx]
          (map vector args (concat expected-types (repeat nil)))))

(defn- extract-expected-arg-types
  "Extract expected argument types from a fn schema for bidirectional pushdown.
   Expands variadic tails to match the actual arg count."
  [schema n-args]
  (when schema
    (let [cat (second schema)
          raw-params (vec (rest cat))
          last-p (peek raw-params)]
      (if (and last-p (t/repeat-type? last-p))
        (let [fixed (pop raw-params)
              var-elem (second last-p)]
          (into fixed (repeat (max 0 (- n-args (count fixed))) var-elem)))
        raw-params))))

(defmethod infer-node :invoke [node ctx _expected]
  (let [fn-node (:fn node)
        var-sym (when (= :var (:op fn-node))
                  (var->sym (:var fn-node)))
        ;; Look up stub fn: from globals (inline annotation) or core stubs
        stub-fn (when var-sym
                  (let [g (ctx/lookup-global ctx var-sym)]
                    (if (fn? g)
                      g
                      (stubs/lookup-stub var-sym))))
        ;; Fall back to fn type from globals or local binding
        fn-type (when-not stub-fn
                  (case (:op fn-node)
                    :var (let [g (ctx/lookup-global ctx var-sym)]
                           (when (t/fn-type? g) g))
                    :local (let [t (ctx/lookup-binding ctx (:name fn-node))]
                             (when (and t (t/fn-type? t)) t))
                    nil))
        ;; Extract expected arg types for bidirectional pushdown
        schema (or (when stub-fn (stubs/stub-schema stub-fn))
                   (when (t/fn-type? fn-type) fn-type))
        expected-arg-types (extract-expected-arg-types schema (count (:args node)))
        ;; Infer args with expected types pushed down
        [arg-types ctx'] (if expected-arg-types
                           (infer-args-with-expected (:args node) expected-arg-types ctx)
                           (infer-args (:args node) ctx))]
    (cond
      stub-fn (stub-fn arg-types (:args node) ctx')
      (and fn-type (t/fn-type? fn-type))
      (check/apply-fn-type ctx' fn-type arg-types (:args node))
      :else [:any ctx'])))

(defmethod infer-node :static-call [node ctx _expected]
  (let [stub-fn (stubs/lookup-static (:class node) (:method node))
        schema (when (fn? stub-fn) (stubs/stub-schema stub-fn))
        expected-arg-types (extract-expected-arg-types schema (count (:args node)))
        [arg-types ctx'] (if expected-arg-types
                           (infer-args-with-expected (:args node) expected-arg-types ctx)
                           (infer-args (:args node) ctx))]
    (cond
      (fn? stub-fn) (stub-fn arg-types (:args node) ctx')
      (t/fn-type? stub-fn) (check/apply-fn-type ctx' stub-fn arg-types (:args node))
      :else [(or (t/tag->type (:tag node)) :any) ctx'])))

(defmethod infer-node :keyword-invoke [node ctx _expected]
  (let [kw-node (:keyword node)
        target-node (:target node)
        [target-type ctx'] (infer-node target-node ctx nil)
        kw (:val kw-node)]
    (if-let [vt (check/map-get-type target-type kw ctx')]
      [vt ctx']
      [:any ctx'])))

(defmethod infer-node :instance-call [node ctx _expected]
  (let [[_ ctx'] (infer-args (:args node) ctx)
        tag (:tag node)]
    [(or (when (class? tag) (t/class->type tag))
         (t/tag->type tag)
         :any)
     ctx']))

;; --- Functions ---

(defmethod infer-node :fn [node ctx expected]
  (let [method (first (:methods node))
        params (:params method)
        ;; Decompose expected fn type if present and arity matches
        [expected-params expected-ret]
        (when (and expected (t/fn-type? expected))
          (let [param-types (vec (rest (second expected)))]
            (when (= (count param-types) (count params))
              [param-types (nth expected 2)])))
        ;; Bind params: concrete from expected, or fresh tvars for inference
        param-types (or expected-params
                        (mapv (fn [_] (t/fresh-tvar)) params))
        ctx' (reduce (fn [c [param tp]]
                       (ctx/extend-binding c (:name param) tp))
                     ctx
                     (map vector params param-types))
        ;; Infer body, pushing expected return type
        [body-type ctx''] (infer-node (:body method) ctx' expected-ret)
        ;; Resolve tvars (no-op for concrete types from expected)
        resolved-params (mapv #(ctx/resolve-type ctx'' %) param-types)
        resolved-ret (ctx/resolve-type ctx'' body-type)]
    [[:=> (into [:cat] resolved-params) resolved-ret]
     ctx'']))

(defmethod infer-node :fn-method [node ctx expected]
  ;; Handled by :fn, but just in case
  (infer-node (:body node) ctx expected))

;; --- Definitions ---

(defmethod infer-node :def [node ctx _expected]
  (let [the-var (:var node)
        var-meta (meta the-var)
        var-sym (var->sym the-var)
        typura-sig (:typura/sig var-meta)
        ;; Protocol method detection: var has :protocol in metadata
        protocol-var (:protocol var-meta)
        ;; Safe deref for protocol def / multimethod detection
        var-val (try (when (and (bound? the-var) (nil? protocol-var))
                       (deref the-var))
                     (catch Exception _ nil))
        is-protocol-def (and (map? var-val) (:sigs var-val) (:on var-val))
        is-multimethod (instance? clojure.lang.MultiFn var-val)
        ;; Push annotation as expected to :fn child for bidirectional inference
        fn-expected (when (and typura-sig (t/fn-type? typura-sig))
                      typura-sig)
        ;; Infer init (skip for protocol method defs — init is dispatch machinery)
        [init-type ctx'] (if (and (:init node) (nil? protocol-var))
                           (infer-node (:init node) ctx fn-expected)
                           [:any ctx])
        ;; Check return type mismatch when annotated
        ctx'' (if (and fn-expected (t/fn-type? init-type))
                (let [declared-ret (nth typura-sig 2)
                      actual-ret (nth init-type 2)]
                  (if (or (= actual-ret :any)
                          (= actual-ret declared-ret)
                          (sub/subtype? actual-ret declared-ret))
                    ctx'
                    (ctx/emit-diagnostic ctx'
                      {:level :error
                       :code :return-type-mismatch
                       :message (str "Return type " actual-ret
                                     " doesn't satisfy declared " declared-ret)
                       :loc (check/node->loc node)
                       :expected declared-ret
                       :actual actual-ret})))
                ctx')
        ;; Existing stub in globals (e.g., factory stub from :deftype)
        existing-global (ctx/lookup-global ctx'' var-sym)
        ;; Build global-val with protocol/multimethod awareness
        global-val (cond
                     ;; Explicit annotation always wins
                     (and typura-sig (t/fn-type? typura-sig))
                     (stubs/sig typura-sig)
                     ;; Protocol method: stub with protocol symbol as first param
                     protocol-var
                     (let [proto-name (var->sym protocol-var)
                           arglists (:arglists var-meta)
                           arity (count (first arglists))
                           param-types (into [:cat proto-name]
                                             (repeat (dec arity) :any))]
                       (stubs/sig [:=> param-types :any]))
                     ;; Multimethod: variadic any→any stub
                     is-multimethod
                     (stubs/sig [:=> [:cat [:* :any]] :any])
                     ;; Preserve existing stub (e.g., factory from :deftype)
                     (fn? existing-global)
                     existing-global
                     ;; Default: inferred type
                     :else init-type)
        ;; Register protocol definition in context
        ctx''' (if is-protocol-def
                 (let [methods-info (into {}
                                     (map (fn [[k v]]
                                            [k {:arglists (:arglists v)}])
                                          (:sigs var-val)))
                       proto-iface (:on-interface var-val)]
                   (-> (ctx/extend-global ctx'' var-sym global-val)
                       (ctx/register-protocol var-sym
                         {:methods methods-info
                          :interface proto-iface})
                       (ctx/register-interface-mapping proto-iface var-sym)))
                 (ctx/extend-global ctx'' var-sym global-val))]
    [(or typura-sig init-type) ctx''']))

;; --- Protocols, Records, Multimethods (Phase 4) ---

(defmethod infer-node :deftype [node ctx _expected]
  ;; Registration done in Pass 1 (collect-declarations). Here we only walk
  ;; method bodies for diagnostics.
  (let [ctx' (reduce (fn [c method-node]
                       (let [mc (reduce (fn [mc p]
                                          (ctx/extend-binding mc (:name p) :any))
                                        c (:params method-node))
                             [_ mc'] (infer-node (:body method-node) mc nil)]
                         (assoc c :diagnostics (:diagnostics mc'))))
                     ctx (:methods node))]
    [:any ctx']))

(defmethod infer-node :method [node ctx expected]
  (infer-node (:body node) ctx expected))

(defmethod infer-node :protocol-invoke [node ctx _expected]
  (let [proto-fn-node (:protocol-fn node)
        the-var (when (= :var (:op proto-fn-node)) (:var proto-fn-node))
        var-sym (when the-var (var->sym the-var))
        ;; Look up stub from globals, core stubs, or derive from protocol metadata
        stub-fn (when var-sym
                  (or (let [g (ctx/lookup-global ctx var-sym)]
                        (when (fn? g) g))
                      (stubs/lookup-stub var-sym)
                      (when-let [proto-var (:protocol (meta the-var))]
                        (let [proto-name (var->sym proto-var)
                              arity (count (first (:arglists (meta the-var))))
                              params (into [:cat proto-name]
                                           (repeat (dec arity) :any))]
                          (stubs/sig [:=> params :any])))))
        [target-type ctx'] (infer-node (:target node) ctx nil)
        [arg-types ctx''] (infer-args (:args node) ctx')
        all-arg-types (into [target-type] arg-types)
        all-arg-nodes (into [(:target node)] (:args node))]
    (if (fn? stub-fn)
      (stub-fn all-arg-types all-arg-nodes ctx'')
      [:any ctx''])))

;; --- Other nodes (Phase 0 stubs) ---

(defmethod infer-node :try [node ctx expected]
  (infer-node (:body node) ctx expected))

(defmethod infer-node :throw [_node ctx _expected]
  [:any ctx])

(defmethod infer-node :new [node ctx _expected]
  (let [[_ ctx'] (infer-args (:args node) ctx)
        cls (:class node)]
    [(or (when (class? cls) (t/class->type cls)) :any) ctx']))

(defmethod infer-node :quote [_node ctx _expected]
  [:any ctx])

(defmethod infer-node :set! [_node ctx _expected]
  [:any ctx])

(defmethod infer-node :recur [_node ctx _expected]
  [:any ctx])

(defmethod infer-node :map [node ctx _expected]
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

(defmethod infer-node :vector [node ctx _expected]
  (let [[elem-types ctx'] (infer-args (:items node) ctx)]
    (if (empty? elem-types)
      [[:vector :any] ctx']
      (let [unified (simplify-union (into [:or] elem-types))]
        [[:vector unified] ctx']))))

(defmethod infer-node :set [node ctx _expected]
  (let [[elem-types ctx'] (infer-args (:items node) ctx)]
    (if (empty? elem-types)
      [[:set :any] ctx']
      (let [unified (simplify-union (into [:or] elem-types))]
        [[:set unified] ctx']))))
