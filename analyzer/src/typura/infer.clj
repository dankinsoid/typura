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

(defmulti infer-node
  "Infer the type of an AST node. Returns [type, updated-ctx]."
  (fn [node ctx] (:op node)))

(defmethod infer-node :default [node ctx]
  [:any ctx])

;; --- Literals ---

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
  "Apply a function type to argument types. Constrains args and returns result type."
  [ctx fn-type arg-types]
  (let [[fixed-params variadic-type] (parse-params (second fn-type))
        return-type (nth fn-type 2)
        n-fixed (count fixed-params)
        n-args (count arg-types)]
    (cond
      ;; Not enough args for fixed params
      (< n-args n-fixed)
      [return-type ctx]

      ;; More args than fixed but no variadic — arity mismatch
      (and (> n-args n-fixed) (nil? variadic-type))
      [return-type ctx]

      :else
      (let [;; Constrain fixed params
            ctx' (reduce (fn [c [arg-t param-t]]
                           (if c (ctx/constrain c arg-t param-t) nil))
                         ctx
                         (map vector arg-types fixed-params))
            ;; Constrain variadic args
            ctx'' (if (and ctx' variadic-type)
                    (reduce (fn [c arg-t]
                              (if c (ctx/constrain c arg-t variadic-type) nil))
                            ctx'
                            (drop n-fixed arg-types))
                    ctx')]
        [return-type (or ctx'' ctx)]))))

(defmethod infer-node :invoke [node ctx]
  (let [fn-node (:fn node)
        [arg-types ctx'] (infer-args (:args node) ctx)
        ;; Resolve the function type
        fn-type (when (= :var (:op fn-node))
                  (let [var-sym (-> fn-node :var symbol)]
                    (or (ctx/lookup-global ctx' var-sym)
                        (stubs/lookup-stub var-sym))))]
    (if (and fn-type (t/fn-type? fn-type))
      (apply-fn-type ctx' fn-type arg-types)
      ;; Unknown function or non-fn type
      [:any ctx'])))

(defmethod infer-node :static-call [node ctx]
  (let [[arg-types ctx'] (infer-args (:args node) ctx)
        fn-type (stubs/lookup-static (:class node) (:method node))]
    (if (and fn-type (t/fn-type? fn-type))
      (apply-fn-type ctx' fn-type arg-types)
      ;; Unknown static call — try JVM tag
      [(or (t/tag->type (:tag node)) :any) ctx'])))

(defmethod infer-node :instance-call [node ctx]
  ;; Phase 0: use JVM tag if available
  (let [[_ ctx'] (infer-args (:args node) ctx)]
    [(or (t/tag->type (:tag node)) :any) ctx']))

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
  [:any ctx])

(defmethod infer-node :quote [node ctx]
  [:any ctx])

(defmethod infer-node :set! [node ctx]
  [:any ctx])

(defmethod infer-node :recur [node ctx]
  [:any ctx])

(defmethod infer-node :map [node ctx]
  ;; Map literal — for Phase 0, return :any
  [:any ctx])

(defmethod infer-node :vector [node ctx]
  [:any ctx])

(defmethod infer-node :set [node ctx]
  [:any ctx])
