(ns typura.context
  (:require [typura.types :as t]
            [typura.subtype :as sub]))

(defn make-context []
  {:bindings    {}
   :globals     {}
   :subst       {}
   :diagnostics []})

(defn emit-diagnostic
  "Append a diagnostic to the context's accumulator."
  [ctx diagnostic]
  (update ctx :diagnostics conj diagnostic))

(defn get-diagnostics [ctx]
  (:diagnostics ctx))

(defn extend-binding [ctx sym type]
  (assoc-in ctx [:bindings sym] type))

(defn extend-bindings [ctx bindings-map]
  (update ctx :bindings merge bindings-map))

(defn lookup-binding [ctx sym]
  (get-in ctx [:bindings sym]))

(defn extend-global [ctx var-sym type]
  (assoc-in ctx [:globals var-sym] type))

(defn lookup-global [ctx var-sym]
  (get-in ctx [:globals var-sym]))

(defn load-stubs [ctx stubs]
  (reduce-kv (fn [c var-sym stub]
               (extend-global c var-sym (:sig stub)))
             ctx
             stubs))

(defn resolve-type
  "Follow tvar substitution chains to a concrete type."
  [ctx t]
  (if (t/tvar? t)
    (let [id (t/tvar-id t)]
      (if-let [resolved (get-in ctx [:subst id])]
        (recur ctx resolved)
        t))
    t))

(defn resolve-deep
  "Resolve all tvars within a type structure."
  [ctx t]
  (let [t (resolve-type ctx t)]
    (cond
      (t/tvar? t) t
      (keyword? t) t
      (and (vector? t) (= :=> (first t)))
      (let [ret (nth t 2)]
        [:=> (resolve-deep ctx (second t))
         (resolve-deep ctx ret)])
      (and (vector? t) (= :or (first t)))
      (into [:or] (map #(resolve-deep ctx %) (rest t)))
      (and (vector? t) (= :cat (first t)))
      (into [:cat] (map #(resolve-deep ctx %) (rest t)))
      (and (vector? t) (t/repeat-type? t))
      [(first t) (resolve-deep ctx (second t))]
      :else t)))

(defn constrain
  "Constrain a type to be a subtype of the expected type.
   If t is a tvar, bind it. If concrete, check subtype.
   Returns updated ctx or nil on failure."
  [ctx t expected]
  (let [t (resolve-type ctx t)
        expected (resolve-type ctx expected)]
    (cond
      (= t expected) ctx
      (= expected :any) ctx
      (t/tvar? t) (assoc-in ctx [:subst (t/tvar-id t)] expected)
      (t/tvar? expected) (assoc-in ctx [:subst (t/tvar-id expected)] t)
      (sub/subtype? t expected) ctx
      :else nil)))
