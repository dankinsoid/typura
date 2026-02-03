(ns typura.infer
  (:require [typura.types :as t]
            [typura.context :as ctx]
            [typura.stubs :as stubs]))

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

(defmethod infer-node :if [node ctx]
  (let [[_ ctx'] (infer-node (:test node) ctx)
        [then-type ctx''] (infer-node (:then node) ctx')
        [else-type ctx'''] (if (:else node)
                             (infer-node (:else node) ctx'')
                             [:nil ctx''])]
    (if (= then-type else-type)
      [then-type ctx''']
      [[:or then-type else-type] ctx'''])))

;; --- Function invocation ---

(defn- infer-args
  "Infer types of argument nodes. Returns [arg-types-vec, updated-ctx]."
  [args ctx]
  (reduce (fn [[types c] arg]
            (let [[tp c'] (infer-node arg c)]
              [(conj types tp) c']))
          [[] ctx]
          args))

(defn- apply-fn-type
  "Apply a function type to argument types. Constrains args and returns result type."
  [ctx fn-type arg-types]
  (let [param-types (rest (second fn-type)) ; skip :cat
        return-type (nth fn-type 2)]
    (if (not= (count param-types) (count arg-types))
      ;; Arity mismatch — degrade to :any
      [return-type ctx]
      (let [ctx' (reduce (fn [c [arg-t param-t]]
                           (if c
                             (ctx/constrain c arg-t param-t)
                             nil))
                         ctx
                         (map vector arg-types param-types))]
        [return-type (or ctx' ctx)]))))

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
