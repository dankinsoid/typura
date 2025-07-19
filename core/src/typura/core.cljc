(ns typura.core
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(deftype SchemaWrapper [schema]
  m/Schema
  (-validator [_] (m/-validator schema))
  (-explainer [_ path] (m/-explainer schema path))
  (-parser [_] (m/-parser schema))
  (-unparser [_] (m/-unparser schema))
  (-transformer [_ transformer method options]
    (m/-transformer schema transformer method options))
  (-walk [_ walker path options] (m/-walk schema walker path options))
  (-properties [_] (m/-properties schema))
  (-options [_] (m/-options schema))
  (-children [_] (m/-children schema))
  (-parent [_] (m/-parent schema))
  (-form [_] (m/-form schema))
  
  clojure.lang.IFn
  (invoke [_ x] x))

(defn schema-wrapper [schema]
  (SchemaWrapper. (m/schema schema)))

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
  (into [:tuple] (repeat size Item)))

(defschema Matrix [Item size0 & rest-sizes]
  [:vector
   {:min size0 :max size0}
   (if (seq rest-sizes)
     (apply Matrix Item rest-sizes)
     Item)])

(defschema User 
  [:map 
   [:id :string]
   [:name :string]
   [:email :string]
   [:age :int]
   [:location [:tuple :double :double]]])


(defn test []
  (m/validate User [123.456 789.012]))
