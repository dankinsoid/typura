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
      (and (vector? t) (= :vector (first t)))
      [:vector (resolve-deep ctx (second t))]
      (and (vector? t) (= :set (first t)))
      [:set (resolve-deep ctx (second t))]
      (and (vector? t) (= :map (first t)))
      (into [:map] (mapv (fn [entry] [(first entry) (resolve-deep ctx (second entry))]) (rest t)))
      (and (vector? t) (= :map-of (first t)))
      [:map-of (resolve-deep ctx (nth t 1)) (resolve-deep ctx (nth t 2))]
      :else t)))

(defn- bind-tvar
  "Bind or narrow a tvar. If already bound, narrows to more specific type.
   Returns updated ctx, or nil on incompatible constraint."
  [ctx tvar-id new-type]
  (let [existing (get-in ctx [:subst tvar-id])]
    (cond
      (nil? existing)
      (assoc-in ctx [:subst tvar-id] new-type)

      (= existing new-type)
      ctx

      (sub/subtype? new-type existing)
      (assoc-in ctx [:subst tvar-id] new-type)

      (sub/subtype? existing new-type)
      ctx

      :else nil)))

(defn constrain
  "Constrain a type to be a subtype of the expected type.
   If t is a tvar, bind/narrow it. If concrete, check subtype.
   Returns updated ctx or nil on failure."
  [ctx t expected]
  (let [resolved-t (resolve-type ctx t)
        resolved-exp (resolve-type ctx expected)]
    (cond
      (= resolved-t resolved-exp) ctx
      (= resolved-exp :any) ctx
      (t/tvar? t)        (bind-tvar ctx (t/tvar-id t) resolved-exp)
      (t/tvar? expected) (bind-tvar ctx (t/tvar-id expected) resolved-t)
      (sub/subtype? resolved-t resolved-exp) ctx
      :else nil)))
