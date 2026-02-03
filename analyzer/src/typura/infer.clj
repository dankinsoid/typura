(ns typura.infer
  (:require [typura.types :as t]
            [typura.context :as ctx]
            [typura.stubs :as stubs]
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

(defn- node->loc
  "Extract source location from an AST node."
  [node]
  (let [env (:env node)]
    (when (and (:line env) (:column env))
      {:line (:line env) :col (:column env)})))

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
        tp (or (ctx/lookup-global ctx var-sym)
               (stubs/lookup-stub var-sym)
               :any)]
    [tp ctx]))

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

(defn- parse-params
  "Split [:cat ...] into [fixed-params variadic-element-type-or-nil].
   Variadic tail is the last element if it's [:* T] or [:+ T]."
  [cat-schema]
  (let [params (vec (rest cat-schema)) ; skip :cat
        last-p (peek params)]
    (if (and last-p (t/repeat-type? last-p))
      [(pop params) (second last-p)]
      [params nil])))

(defn- apply-fn-type
  "Apply a function type to argument types. Constrains args and returns result type.
   `arg-nodes` are AST nodes used for diagnostic locations."
  [ctx fn-type arg-types arg-nodes]
  (let [[fixed-params variadic-type] (parse-params (second fn-type))
        return-type (nth fn-type 2)
        n-fixed (count fixed-params)
        n-args (count arg-types)]
    (cond
      (< n-args n-fixed)
      [return-type ctx]

      (and (> n-args n-fixed) (nil? variadic-type))
      [return-type ctx]

      :else
      (let [;; Constrain fixed params, collecting type-mismatch diagnostics
            ctx' (reduce
                   (fn [c [arg-t param-t arg-node]]
                     (let [result (ctx/constrain c arg-t param-t)]
                       (or result
                           (ctx/emit-diagnostic c
                             {:level :error :code :type-mismatch
                              :message (str "Expected " param-t ", got " arg-t)
                              :loc (node->loc arg-node)
                              :expected param-t :actual arg-t}))))
                   ctx
                   (map vector arg-types fixed-params arg-nodes))
            ;; Constrain variadic args
            ctx'' (if variadic-type
                    (reduce (fn [c [arg-t arg-node]]
                              (let [result (ctx/constrain c arg-t variadic-type)]
                                (or result
                                    (ctx/emit-diagnostic c
                                      {:level :error :code :type-mismatch
                                       :message (str "Expected " variadic-type ", got " arg-t)
                                       :loc (node->loc arg-node)
                                       :expected variadic-type :actual arg-t}))))
                            ctx'
                            (map vector (drop n-fixed arg-types) (drop n-fixed arg-nodes)))
                    ctx')]
        [return-type ctx'']))))

(defn- map-get-type
  "Extract value type from a map type for a given keyword key.
   Handles unions by checking each member."
  [coll-type key-val]
  (cond
    (and (t/map-type? coll-type) (keyword? key-val))
    (some (fn [entry] (when (= key-val (first entry)) (second entry)))
          (rest coll-type))

    (t/map-of-type? coll-type)
    (nth coll-type 2)

    ;; Union: try each member, return first match
    (t/union-type? coll-type)
    (some #(map-get-type % key-val) (rest coll-type))

    :else nil))

(defn- coll-nth-type
  "Extract element type from a vector type."
  [coll-type]
  (when (t/vector-type? coll-type)
    (second coll-type)))

(defn- special-invoke
  "Handle functions whose return type depends on argument values/types.
   Returns [type ctx] or nil to fall through to standard logic."
  [var-sym arg-types arg-nodes ctx]
  (case var-sym
    clojure.core/get
    (let [coll-type (first arg-types)
          key-node (second arg-nodes)
          key-val (when (and key-node (= :const (:op key-node)))
                    (:val key-node))]
      (when-let [vt (map-get-type coll-type key-val)]
        [vt ctx]))

    clojure.core/nth
    (when-let [et (coll-nth-type (first arg-types))]
      [et ctx])

    ;; default — not special
    nil))

(defmethod infer-node :invoke [node ctx]
  (let [fn-node (:fn node)
        [arg-types ctx'] (infer-args (:args node) ctx)
        ;; Try special-case dispatch first
        var-sym (when (= :var (:op fn-node))
                  (-> fn-node :var symbol))
        special (when var-sym
                  (special-invoke var-sym arg-types (:args node) ctx'))
        ;; Resolve the function type
        fn-type (when-not special
                  (case (:op fn-node)
                    :var (or (ctx/lookup-global ctx' var-sym)
                             (stubs/lookup-stub var-sym))
                    :local (let [t (ctx/lookup-binding ctx' (:name fn-node))]
                             (when (and t (t/fn-type? t)) t))
                    nil))]
    (cond
      special special
      (and fn-type (t/fn-type? fn-type))
      (apply-fn-type ctx' fn-type arg-types (:args node))
      :else [:any ctx'])))

(defn- special-static-call
  "Handle RT.get / RT.nth static calls from destructuring expansion.
   Returns [type ctx] or nil."
  [class method arg-types arg-nodes ctx]
  (let [class-name (if (class? class) (.getName ^Class class) (str class))
        method-name (str method)]
    (case [class-name method-name]
      ["clojure.lang.RT" "get"]
      (let [coll-type (first arg-types)
            key-node (second arg-nodes)
            key-val (when (and key-node (= :const (:op key-node)))
                      (:val key-node))]
        (when-let [vt (map-get-type coll-type key-val)]
          [vt ctx]))

      ["clojure.lang.RT" "nth"]
      (when-let [et (coll-nth-type (first arg-types))]
        [et ctx])

      ;; default
      nil)))

(defmethod infer-node :static-call [node ctx]
  (let [[arg-types ctx'] (infer-args (:args node) ctx)
        special (special-static-call (:class node) (:method node)
                                     arg-types (:args node) ctx')
        fn-type (when-not special
                  (stubs/lookup-static (:class node) (:method node)))]
    (cond
      special special
      (and fn-type (t/fn-type? fn-type))
      (apply-fn-type ctx' fn-type arg-types (:args node))
      :else [(or (t/tag->type (:tag node)) :any) ctx'])))

(defmethod infer-node :keyword-invoke [node ctx]
  (let [kw-node (:keyword node)
        target-node (:target node)
        [target-type ctx'] (infer-node target-node ctx)
        kw (:val kw-node)]
    (if-let [vt (map-get-type target-type kw)]
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
  (let [[init-type ctx'] (if (:init node)
                           (infer-node (:init node) ctx)
                           [:any ctx])
        var-sym (-> node :var symbol)
        ctx'' (ctx/extend-global ctx' var-sym init-type)]
    [init-type ctx'']))

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
