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

    :else false))
