(ns typura.stubs
  (:require [typura.check :as check]))

(defn sig
  "Create a stub function from a Malli fn schema [:=> [:cat ...] ret].
   Returns (fn [arg-types arg-nodes ctx] -> [ret-type ctx']).
   The schema is stored in metadata for type extraction."
  [schema]
  (with-meta
    (fn [arg-types arg-nodes ctx]
      (check/apply-fn-type ctx schema arg-types arg-nodes))
    {:schema schema}))

(defn sig+guard
  "Like `sig` but attaches a guard for flow narrowing in `if` branches."
  [schema guard]
  (with-meta (sig schema) {:guard guard}))

;; --- Resolvers for context-dependent stubs ---

(defn- resolve-get
  "Resolver for `get` / `RT.get`: extracts value type from typed maps."
  [arg-types arg-nodes ctx]
  (let [coll-type (first arg-types)
        key-node (second arg-nodes)
        key-val (when (and key-node (= :const (:op key-node)))
                  (:val key-node))]
    (if-let [vt (check/map-get-type coll-type key-val)]
      [vt ctx]
      [:any ctx])))

(defn- resolve-nth
  "Resolver for `nth` / `RT.nth`: extracts element type from vectors."
  [arg-types arg-nodes ctx]
  (if-let [et (check/coll-nth-type (first arg-types))]
    [et ctx]
    [:any ctx]))

;; --- Core stubs ---

(def core-stubs
  {;; Arithmetic — variadic: (+ x y & more)
   'clojure.core/+       (sig [:=> [:cat [:* :number]] :number])
   'clojure.core/-       (sig [:=> [:cat :number [:* :number]] :number])
   'clojure.core/*       (sig [:=> [:cat [:* :number]] :number])
   'clojure.core//       (sig [:=> [:cat :number [:* :number]] :number])
   'clojure.core/inc     (sig [:=> [:cat :number] :number])
   'clojure.core/dec     (sig [:=> [:cat :number] :number])

   ;; Strings — variadic: (str & args)
   'clojure.core/str     (sig [:=> [:cat [:* :any]] :string])

   ;; Collections
   'clojure.core/count   (sig [:=> [:cat :any] :int])
   'clojure.core/conj    (sig [:=> [:cat :any [:+ :any]] :any])
   'clojure.core/assoc   (sig [:=> [:cat :any :any :any [:* :any]] :any])
   'clojure.core/get     resolve-get
   'clojure.core/first   (sig [:=> [:cat :any] :any])
   'clojure.core/rest    (sig [:=> [:cat :any] :any])
   'clojure.core/nth     resolve-nth

   ;; IO — variadic: (println & args)
   'clojure.core/println (sig [:=> [:cat [:* :any]] :nil])

   ;; Type predicates — with guards for flow narrowing
   'clojure.core/int?     (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :int})
   'clojure.core/integer? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :int})
   'clojure.core/double?  (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :double})
   'clojure.core/number?  (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :number})
   'clojure.core/string?  (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :string})
   'clojure.core/keyword? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :keyword})
   'clojure.core/symbol?  (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :symbol})
   'clojure.core/boolean? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :boolean})
   'clojure.core/nil?     (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :nil})
   'clojure.core/map?     (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows [:map-of :any :any]})
   'clojure.core/vector?  (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows [:vector :any]})
   'clojure.core/set?     (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows [:set :any]})
   'clojure.core/seq?     (sig [:=> [:cat :any] :boolean])
   'clojure.core/some?    (sig [:=> [:cat :any] :boolean])
   'clojure.core/true?    (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :boolean})
   'clojure.core/false?   (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :boolean})})

;; tools.analyzer.jvm inlines arithmetic to static calls on Numbers.
;; Keys are [class-name-string method-name-string] for reliable lookup.
(def static-call-types
  {["clojure.lang.Numbers" "add"]             (sig [:=> [:cat :number :number] :number])
   ["clojure.lang.Numbers" "minus"]           (sig [:=> [:cat :number :number] :number])
   ["clojure.lang.Numbers" "multiply"]        (sig [:=> [:cat :number :number] :number])
   ["clojure.lang.Numbers" "divide"]          (sig [:=> [:cat :number :number] :number])
   ["clojure.lang.Numbers" "inc"]             (sig [:=> [:cat :number] :number])
   ["clojure.lang.Numbers" "dec"]             (sig [:=> [:cat :number] :number])
   ["clojure.lang.Numbers" "unchecked_inc"]   (sig [:=> [:cat :number] :number])
   ["clojure.lang.Numbers" "unchecked_dec"]   (sig [:=> [:cat :number] :number])
   ["clojure.lang.Numbers" "addP"]            (sig [:=> [:cat :number :number] :number])
   ["clojure.lang.Numbers" "minusP"]          (sig [:=> [:cat :number :number] :number])
   ["clojure.lang.Numbers" "multiplyP"]       (sig [:=> [:cat :number :number] :number])
   ["clojure.lang.Numbers" "incP"]            (sig [:=> [:cat :number] :number])
   ["clojure.lang.Numbers" "decP"]            (sig [:=> [:cat :number] :number])

   ;; Bitwise operations
   ["clojure.lang.Numbers" "and"]              (sig [:=> [:cat :int :int] :int])
   ["clojure.lang.Numbers" "or"]               (sig [:=> [:cat :int :int] :int])
   ["clojure.lang.Numbers" "xor"]              (sig [:=> [:cat :int :int] :int])

   ;; Destructuring support — RT methods and PersistentArrayMap factory
   ["clojure.lang.RT" "get"]                    resolve-get
   ["clojure.lang.RT" "nth"]                    resolve-nth
   ["clojure.lang.PersistentArrayMap" "createAsIfByAssoc"] (sig [:=> [:cat :any] [:map-of :any :any]])})

(defn stub-schema
  "Extract the advertised schema from a stub fn, if available."
  [stub-fn]
  (:schema (meta stub-fn)))

(defn lookup-stub
  "Look up a core stub function by var symbol."
  [var-sym]
  (get core-stubs var-sym))

(defn lookup-guard
  "Look up guard metadata for a core stub."
  [var-sym]
  (when-let [stub-fn (get core-stubs var-sym)]
    (:guard (meta stub-fn))))

(defn lookup-static
  "Look up a static-call stub function by class and method."
  [class method]
  (let [class-name (if (class? class) (.getName ^Class class) (str class))
        method-name (str method)]
    (get static-call-types [class-name method-name])))
