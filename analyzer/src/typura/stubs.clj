(ns typura.stubs)

(def core-stubs
  {;; Arithmetic — variadic: (+ x y & more)
   'clojure.core/+       {:sig [:=> [:cat [:* :number]] :number]}
   'clojure.core/-       {:sig [:=> [:cat :number [:* :number]] :number]}
   'clojure.core/*       {:sig [:=> [:cat [:* :number]] :number]}
   'clojure.core//       {:sig [:=> [:cat :number [:* :number]] :number]}
   'clojure.core/inc     {:sig [:=> [:cat :number] :number]}
   'clojure.core/dec     {:sig [:=> [:cat :number] :number]}

   ;; Strings — variadic: (str & args)
   'clojure.core/str     {:sig [:=> [:cat [:* :any]] :string]}

   ;; Collections
   'clojure.core/count   {:sig [:=> [:cat :any] :int]}
   'clojure.core/conj    {:sig [:=> [:cat :any [:+ :any]] :any]}
   'clojure.core/assoc   {:sig [:=> [:cat :any :any :any [:* :any]] :any]}
   'clojure.core/get     {:sig [:=> [:cat :any :any] :any]}
   'clojure.core/first   {:sig [:=> [:cat :any] :any]}
   'clojure.core/rest    {:sig [:=> [:cat :any] :any]}
   'clojure.core/nth     {:sig [:=> [:cat :any :int] :any]}

   ;; IO — variadic: (println & args)
   'clojure.core/println {:sig [:=> [:cat [:* :any]] :nil]}

   ;; Type predicates — with guards for flow narrowing
   'clojure.core/int?     {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :int}}
   'clojure.core/integer? {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :int}}
   'clojure.core/double?  {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :double}}
   'clojure.core/number?  {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :number}}
   'clojure.core/string?  {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :string}}
   'clojure.core/keyword? {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :keyword}}
   'clojure.core/symbol?  {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :symbol}}
   'clojure.core/boolean? {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :boolean}}
   'clojure.core/nil?     {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :nil}}
   'clojure.core/map?     {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows [:map-of :any :any]}}
   'clojure.core/vector?  {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows [:vector :any]}}
   'clojure.core/set?     {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows [:set :any]}}
   'clojure.core/seq?     {:sig [:=> [:cat :any] :boolean]}
   'clojure.core/some?    {:sig [:=> [:cat :any] :boolean]}
   'clojure.core/true?    {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :boolean}}
   'clojure.core/false?   {:sig [:=> [:cat :any] :boolean] :guard {:arg 0 :narrows :boolean}}})

;; tools.analyzer.jvm inlines arithmetic to static calls on Numbers.
;; Keys are [class-name-string method-name-string] for reliable lookup
;; since the AST provides java.lang.Class and clojure.lang.Symbol.
(def static-call-types
  {["clojure.lang.Numbers" "add"]             {:sig [:=> [:cat :number :number] :number]}
   ["clojure.lang.Numbers" "minus"]           {:sig [:=> [:cat :number :number] :number]}
   ["clojure.lang.Numbers" "multiply"]        {:sig [:=> [:cat :number :number] :number]}
   ["clojure.lang.Numbers" "divide"]          {:sig [:=> [:cat :number :number] :number]}
   ["clojure.lang.Numbers" "inc"]             {:sig [:=> [:cat :number] :number]}
   ["clojure.lang.Numbers" "dec"]             {:sig [:=> [:cat :number] :number]}
   ["clojure.lang.Numbers" "unchecked_inc"]   {:sig [:=> [:cat :number] :number]}
   ["clojure.lang.Numbers" "unchecked_dec"]   {:sig [:=> [:cat :number] :number]}
   ["clojure.lang.Numbers" "addP"]            {:sig [:=> [:cat :number :number] :number]}
   ["clojure.lang.Numbers" "minusP"]          {:sig [:=> [:cat :number :number] :number]}
   ["clojure.lang.Numbers" "multiplyP"]       {:sig [:=> [:cat :number :number] :number]}
   ["clojure.lang.Numbers" "incP"]            {:sig [:=> [:cat :number] :number]}
   ["clojure.lang.Numbers" "decP"]            {:sig [:=> [:cat :number] :number]}

   ;; Destructuring support — RT methods and PersistentArrayMap factory
   ["clojure.lang.RT" "get"]                    {:sig [:=> [:cat :any :any] :any]}
   ["clojure.lang.RT" "nth"]                    {:sig [:=> [:cat :any :int] :any]}
   ["clojure.lang.PersistentArrayMap" "createAsIfByAssoc"] {:sig [:=> [:cat :any] [:map-of :any :any]]}})

(defn lookup-stub [var-sym]
  (get-in core-stubs [var-sym :sig]))

(defn lookup-guard [var-sym]
  (get-in core-stubs [var-sym :guard]))

(defn lookup-static [class method]
  (let [class-name (if (class? class) (.getName ^Class class) (str class))
        method-name (str method)]
    (get-in static-call-types [[class-name method-name] :sig])))
