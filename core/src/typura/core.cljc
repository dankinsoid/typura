(ns typura.core
  (:require [malli.core :as m]
            [malli.registry :as mr]))

;; Core library for type definitions and utilities
(defmacro defschema
  ([name args schema]
   `(defn ~name ~args ~schema))
  ([name schema]
   `(def ~name ~schema)))

(defn is?
  "Check if the value is of the specified schema"
  [schema value]
  (m/validate schema value))

(defn as
  "Assert that the value conforms to the schema, throwing an exception if it does not."
  [schema value]
  (if (is? schema value)
    value
    (throw (ex-info "Value does not conform to schema" {:schema schema :value value}))))

(defschema Fixed-Vector [size Item]
  [:vector
   {:min size :max size}
   Item])

(defschema Matrix [Item size0 & rest-sizes]
  [:vector
   {:min size0 :max size0}
   (if (seq rest-sizes)
     (apply Matrix Item rest-sizes)
     Item)])

;; (defn some-fn
;;   {:types [T]
;;    :where (extends? Object T)}
  
;;   [object T
;;    :_ currency :keyword := :USD
;;    :count number :int := 0
;;    :tuple [first second] [:tuple :string :string]
;;    & rest :any]
;;   object)

;; (defn hmhm [number :int #(-> (or 4) (min (max 4)))
;;             name :string :or 3
;;             [first second] :vector :or [0 1]]
;;   count)

(-> nil (or 3))

(defn parse-args [args]
  (if (seq args)
    (cond
      (symbol? (first args)) []
      (keyword? (first args)) []
      :else (throw (ex-info "Invalid argument type" {:args args})))
    []))

;; (defn hmhm [[count :int]
;;             [name :string]]
;;   count)

;; (hmm :count 10 :name "example")

;; (defn convert
;;   [:_ amount :number
;;    :_ from-currency :keyword
;;    :to to-currency :keyword]
;;   object)

;; (convert 20 :USD :to :EUR)

(defschema User
  [:map
   [:id :string]
   [:name :string]
   [:email :string]
   [:age :int]
   [:location [:tuple :double :double]]])

(is? (Matrix :int 3 3) [[0 0 7] [0 0 0] [0 0 0]])

(macroexpand-1 '(defn eee {:doc "doc"} [hm] hm))
