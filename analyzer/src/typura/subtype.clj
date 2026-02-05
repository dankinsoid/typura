(ns typura.subtype
  (:require [typura.types :as t]))

(defn subtype?
  "Is `sub` a subtype of `super`?"
  [sub super]
  (cond
    (= sub super)
    true

    (= super :any)
    true

    ;; sub is a union — all members must be subtypes of super
    (t/union-type? sub)
    (every? #(subtype? % super) (rest sub))

    ;; super is a union — sub must be subtype of at least one member
    (t/union-type? super)
    (some #(subtype? sub %) (rest super))

    ;; keyword satisfies Java interface
    (and (keyword? sub) (t/java-type? super))
    (contains? (get t/interface-satisfaction sub) super)

    ;; primitive hierarchy (transitive)
    (and (keyword? sub) (keyword? super))
    (let [parents (get t/primitive-parents sub)]
      (or (contains? parents super)
          (some #(subtype? % super) parents)))

    ;; map structural subtyping: sub has all keys of super with compatible value types
    (and (t/map-type? sub) (t/map-type? super))
    (let [sub-entries (rest sub)
          super-entries (rest super)]
      (every? (fn [super-entry]
                (let [super-key (first super-entry)
                      super-val (last super-entry)]
                  (some (fn [sub-entry]
                          (and (= (first sub-entry) super-key)
                               (subtype? (last sub-entry) super-val)))
                        sub-entries)))
              super-entries))

    ;; function subtyping: contravariant params, covariant return
    (and (t/fn-type? sub) (t/fn-type? super))
    (let [sub-params (rest (second sub))
          super-params (rest (second super))
          sub-ret (nth sub 2)
          super-ret (nth super 2)]
      (and (= (count sub-params) (count super-params))
           (every? true? (map subtype? super-params sub-params))
           (subtype? sub-ret super-ret)))

    ;; vector covariant: [:vector :int] <: [:vector :number]
    (and (t/vector-type? sub) (t/vector-type? super))
    (subtype? (second sub) (second super))

    ;; set covariant: [:set :int] <: [:set :number]
    (and (t/set-type? sub) (t/set-type? super))
    (subtype? (second sub) (second super))

    ;; map-of covariant: [:map-of K1 V1] <: [:map-of K2 V2]
    (and (t/map-of-type? sub) (t/map-of-type? super))
    (and (subtype? (nth sub 1) (nth super 1))
         (subtype? (nth sub 2) (nth super 2)))

    ;; structural map <: map-of: all entries must fit
    (and (t/map-type? sub) (t/map-of-type? super))
    (let [k-super (nth super 1)
          v-super (nth super 2)]
      (every? (fn [entry]
                (and (subtype? (if (keyword? (first entry)) :keyword :any) k-super)
                     (subtype? (second entry) v-super)))
              (rest sub)))

    ;; collection type satisfies Java interface
    (and (vector? sub) (t/java-type? super))
    (let [tag (first sub)]
      (contains? (get t/interface-satisfaction tag) super))

    :else false))
