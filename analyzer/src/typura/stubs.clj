(ns typura.stubs)

(def core-stubs
  {;; Arithmetic
   'clojure.core/+       {:sig [:=> [:cat :number :number] :number]}
   'clojure.core/-       {:sig [:=> [:cat :number :number] :number]}
   'clojure.core/*       {:sig [:=> [:cat :number :number] :number]}
   'clojure.core//       {:sig [:=> [:cat :number :number] :number]}
   'clojure.core/inc     {:sig [:=> [:cat :number] :number]}
   'clojure.core/dec     {:sig [:=> [:cat :number] :number]}

   ;; Strings
   'clojure.core/str     {:sig [:=> [:cat :any] :string]}

   ;; Collections
   'clojure.core/count   {:sig [:=> [:cat :any] :int]}
   'clojure.core/conj    {:sig [:=> [:cat :any :any] :any]}
   'clojure.core/assoc   {:sig [:=> [:cat :any :any :any] :any]}
   'clojure.core/get     {:sig [:=> [:cat :any :any] :any]}
   'clojure.core/first   {:sig [:=> [:cat :any] :any]}
   'clojure.core/rest    {:sig [:=> [:cat :any] :any]}
   'clojure.core/nth     {:sig [:=> [:cat :any :int] :any]}

   ;; IO
   'clojure.core/println {:sig [:=> [:cat :any] :nil]}})

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
   ["clojure.lang.Numbers" "decP"]            {:sig [:=> [:cat :number] :number]}})

(defn lookup-stub [var-sym]
  (get-in core-stubs [var-sym :sig]))

(defn lookup-static [class method]
  (let [class-name (if (class? class) (.getName ^Class class) (str class))
        method-name (str method)]
    (get-in static-call-types [[class-name method-name] :sig])))
