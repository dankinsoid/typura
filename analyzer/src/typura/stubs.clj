(ns typura.stubs
  (:require [typura.check :as check]
            [typura.context :as ctx]
            [typura.types :as t])
  (:import [clojure.lang ILookup Indexed IPersistentSet ITransientSet]
           [java.util Map]))

;; =============================================================================
;; Unified Stub Registry
;; =============================================================================

(defonce ^:private registry (atom {}))

(defn register!
  "Register a stub. Key formats:
   - [:var 'clojure.core/+]           — var/function call
   - [:static \"ClassName\" \"method\"] — static method call
   - [:ifn \"ClassName\" arity]        — IFn.invoke on type
   - [:method \"ClassName\" \"method\"] — instance method call"
  [key stub-fn]
  (swap! registry assoc key stub-fn)
  nil)

(defn lookup
  "Look up a stub by key."
  [key]
  (get @registry key))

(defn register-var! [var-sym stub-fn]
  (register! [:var var-sym] stub-fn))

(defn register-static! [class-name method-name stub-fn]
  (register! [:static class-name method-name] stub-fn))

(defn register-ifn! [class-name arity stub-fn]
  (register! [:ifn class-name arity] stub-fn))

(defn register-method! [class-name method-name stub-fn]
  (register! [:method class-name method-name] stub-fn))

;; Convenience lookups
(defn lookup-var [var-sym]
  (lookup [:var var-sym]))

(defn lookup-static [class method]
  (let [class-name (if (class? class) (.getName ^Class class) (str class))
        method-name (str method)]
    (lookup [:static class-name method-name])))

(defn lookup-ifn [class-or-type arity]
  (let [class-name (cond
                     (class? class-or-type) (.getName ^Class class-or-type)
                     (string? class-or-type) class-or-type
                     :else (str class-or-type))]
    (lookup [:ifn class-name arity])))

(defn lookup-method [class method]
  (let [class-name (if (class? class) (.getName ^Class class) (str class))
        method-name (str method)]
    (lookup [:method class-name method-name])))

(defn type->ifn-class
  "Map a type to its IFn-implementing class name, or nil if unknown."
  [t]
  (cond
    (= :keyword t) "clojure.lang.Keyword"
    (and (vector? t) (= :map (first t))) "clojure.lang.APersistentMap"
    (and (vector? t) (= :map-of (first t))) "clojure.lang.APersistentMap"
    (and (vector? t) (= :vector (first t))) "clojure.lang.APersistentVector"
    (and (vector? t) (= :set (first t))) "clojure.lang.APersistentSet"
    :else nil))

(defn lookup-ifn-for-type
  "Look up IFn stub for a type and arity."
  [t arity]
  (when-let [cls (type->ifn-class t)]
    (lookup-ifn cls arity)))

;; =============================================================================
;; Stub Helpers
;; =============================================================================

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
  (with-meta (sig schema) {:schema schema :guard guard}))

;; =============================================================================
;; Context-dependent resolvers
;; =============================================================================

;; Union of types that RT.get supports (plus nil handled specially)
(def ^:private gettable-type
  [:or :nil ILookup Map IPersistentSet ITransientSet])

(defn- resolve-get
  "Resolver for `get` / `RT.get`: extracts value type from typed maps.
   Constrains coll to gettable types if it's a tvar."
  [arg-types arg-nodes ctx]
  (let [coll-type (first arg-types)
        key-node (second arg-nodes)
        key-val (when (and key-node (= :const (:op key-node)))
                  (:val key-node))
        ctx' (if (t/tvar? coll-type)
               (or (ctx/constrain ctx coll-type gettable-type) ctx)
               ctx)]
    (if-let [vt (check/map-get-type coll-type key-val ctx')]
      [vt ctx']
      [:any ctx'])))

(defn- resolve-nth
  "Resolver for `nth` / `RT.nth`: extracts element type from vectors.
   Constrains coll to Indexed if it's a tvar."
  [arg-types _arg-nodes ctx]
  (let [coll-type (first arg-types)
        ctx' (if (t/tvar? coll-type)
               (or (ctx/constrain ctx coll-type Indexed) ctx)
               ctx)]
    (if-let [et (check/coll-nth-type coll-type)]
      [et ctx']
      [:any ctx'])))

(defn- resolve-keyword-invoke
  "Resolver for keyword invocation `(:key m)`.
   arg-types: [keyword-type target-type]
   arg-nodes: [keyword-node target-node]
   Constrains target to gettable types if it's a tvar."
  [arg-types arg-nodes ctx]
  (let [[_kw-type target-type] arg-types
        kw-node (first arg-nodes)
        ctx' (if (t/tvar? target-type)
               (or (ctx/constrain ctx target-type gettable-type) ctx)
               ctx)]
    (if (= :const (:op kw-node))
      (let [kw (:val kw-node)]
        (if-let [vt (check/map-get-type target-type kw ctx')]
          [vt ctx']
          [:any ctx']))
      [:any ctx'])))

(defn- resolve-map-invoke
  "Resolver for map invocation `(m :key)`.
   arg-types: [map-type key-type]
   arg-nodes: [map-node key-node]"
  [arg-types arg-nodes ctx]
  (let [[map-type _key-type] arg-types
        key-node (second arg-nodes)]
    (if (= :const (:op key-node))
      (let [key-val (:val key-node)]
        (if-let [vt (check/map-get-type map-type key-val ctx)]
          [vt ctx]
          [:any ctx]))
      [:any ctx])))

(defn- resolve-vector-invoke
  "Resolver for vector invocation `(v idx)`.
   Returns element type of the vector.
   Constrains vec to Indexed if it's a tvar."
  [arg-types _arg-nodes ctx]
  (let [[vec-type _idx-type] arg-types
        ctx' (if (t/tvar? vec-type)
               (or (ctx/constrain ctx vec-type Indexed) ctx)
               ctx)]
    (if-let [et (check/coll-nth-type vec-type)]
      [et ctx']
      [:any ctx'])))

(defn- resolve-set-invoke
  "Resolver for set invocation `(s val)`.
   Returns the element type or nil."
  [arg-types _arg-nodes ctx]
  (let [[set-type _val-type] arg-types
        elem-type (when (and (vector? set-type) (= :set (first set-type)))
                    (second set-type))]
    [(if elem-type [:or elem-type :nil] :any) ctx]))

;; =============================================================================
;; Stub metadata utilities
;; =============================================================================

(defn stub-schema
  "Extract the advertised schema from a stub fn, if available."
  [stub-fn]
  (:schema (meta stub-fn)))

(defn stub-guard
  "Extract guard metadata from a stub fn, if available."
  [stub-fn]
  (:guard (meta stub-fn)))

(defn lookup-guard
  "Look up guard metadata for a var stub."
  [var-sym]
  (when-let [stub-fn (lookup-var var-sym)]
    (stub-guard stub-fn)))

;; =============================================================================
;; Core stubs registration
;; =============================================================================

(defn- register-core-stubs! []
  ;; Arithmetic — variadic
  (register-var! 'clojure.core/+ (sig [:=> [:cat [:* :number]] :number]))
  (register-var! 'clojure.core/- (sig [:=> [:cat :number [:* :number]] :number]))
  (register-var! 'clojure.core/* (sig [:=> [:cat [:* :number]] :number]))
  (register-var! 'clojure.core// (sig [:=> [:cat :number [:* :number]] :number]))
  (register-var! 'clojure.core/inc (sig [:=> [:cat :number] :number]))
  (register-var! 'clojure.core/dec (sig [:=> [:cat :number] :number]))

  ;; Strings
  (register-var! 'clojure.core/str (sig [:=> [:cat [:* :any]] :string]))

  ;; Collections
  (register-var! 'clojure.core/count (sig [:=> [:cat :any] :int]))
  (register-var! 'clojure.core/conj (sig [:=> [:cat :any [:+ :any]] :any]))
  (register-var! 'clojure.core/assoc (sig [:=> [:cat :any :any :any [:* :any]] :any]))
  (register-var! 'clojure.core/get resolve-get)
  (register-var! 'clojure.core/first (sig [:=> [:cat :any] :any]))
  (register-var! 'clojure.core/rest (sig [:=> [:cat :any] :any]))
  (register-var! 'clojure.core/nth resolve-nth)

  ;; IO
  (register-var! 'clojure.core/println (sig [:=> [:cat [:* :any]] :nil]))

  ;; Type predicates with guards
  (register-var! 'clojure.core/int? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :int}))
  (register-var! 'clojure.core/integer? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :int}))
  (register-var! 'clojure.core/double? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :double}))
  (register-var! 'clojure.core/number? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :number}))
  (register-var! 'clojure.core/string? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :string}))
  (register-var! 'clojure.core/keyword? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :keyword}))
  (register-var! 'clojure.core/symbol? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :symbol}))
  (register-var! 'clojure.core/boolean? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :boolean}))
  (register-var! 'clojure.core/nil? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :nil}))
  (register-var! 'clojure.core/map? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows [:map-of :any :any]}))
  (register-var! 'clojure.core/vector? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows [:vector :any]}))
  (register-var! 'clojure.core/set? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows [:set :any]}))
  (register-var! 'clojure.core/seq? (sig [:=> [:cat :any] :boolean]))
  (register-var! 'clojure.core/some? (sig [:=> [:cat :any] :boolean]))
  (register-var! 'clojure.core/true? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :boolean}))
  (register-var! 'clojure.core/false? (sig+guard [:=> [:cat :any] :boolean] {:arg 0 :narrows :boolean})))

(defn- register-static-stubs! []
  ;; Numbers — tools.analyzer.jvm inlines arithmetic to static calls
  (register-static! "clojure.lang.Numbers" "add" (sig [:=> [:cat :number :number] :number]))
  (register-static! "clojure.lang.Numbers" "minus" (sig [:=> [:cat :number :number] :number]))
  (register-static! "clojure.lang.Numbers" "multiply" (sig [:=> [:cat :number :number] :number]))
  (register-static! "clojure.lang.Numbers" "divide" (sig [:=> [:cat :number :number] :number]))
  (register-static! "clojure.lang.Numbers" "inc" (sig [:=> [:cat :number] :number]))
  (register-static! "clojure.lang.Numbers" "dec" (sig [:=> [:cat :number] :number]))
  (register-static! "clojure.lang.Numbers" "unchecked_inc" (sig [:=> [:cat :number] :number]))
  (register-static! "clojure.lang.Numbers" "unchecked_dec" (sig [:=> [:cat :number] :number]))
  (register-static! "clojure.lang.Numbers" "addP" (sig [:=> [:cat :number :number] :number]))
  (register-static! "clojure.lang.Numbers" "minusP" (sig [:=> [:cat :number :number] :number]))
  (register-static! "clojure.lang.Numbers" "multiplyP" (sig [:=> [:cat :number :number] :number]))
  (register-static! "clojure.lang.Numbers" "incP" (sig [:=> [:cat :number] :number]))
  (register-static! "clojure.lang.Numbers" "decP" (sig [:=> [:cat :number] :number]))

  ;; Bitwise
  (register-static! "clojure.lang.Numbers" "and" (sig [:=> [:cat :int :int] :int]))
  (register-static! "clojure.lang.Numbers" "or" (sig [:=> [:cat :int :int] :int]))
  (register-static! "clojure.lang.Numbers" "xor" (sig [:=> [:cat :int :int] :int]))

  ;; Destructuring support
  (register-static! "clojure.lang.RT" "get" resolve-get)
  (register-static! "clojure.lang.RT" "nth" resolve-nth)
  (register-static! "clojure.lang.PersistentArrayMap" "createAsIfByAssoc"
                    (sig [:=> [:cat :any] [:map-of :any :any]])))

(defn- register-ifn-stubs! []
  ;; Keyword as IFn: (:key m) or (:key m default)
  (register-ifn! "clojure.lang.Keyword" 1 resolve-keyword-invoke)
  (register-ifn! "clojure.lang.Keyword" 2 resolve-keyword-invoke)

  ;; Map as IFn: (m :key) or (m :key default)
  (register-ifn! "clojure.lang.APersistentMap" 1 resolve-map-invoke)
  (register-ifn! "clojure.lang.APersistentMap" 2 resolve-map-invoke)
  (register-ifn! "clojure.lang.PersistentArrayMap" 1 resolve-map-invoke)
  (register-ifn! "clojure.lang.PersistentArrayMap" 2 resolve-map-invoke)
  (register-ifn! "clojure.lang.PersistentHashMap" 1 resolve-map-invoke)
  (register-ifn! "clojure.lang.PersistentHashMap" 2 resolve-map-invoke)

  ;; Vector as IFn: (v idx)
  (register-ifn! "clojure.lang.APersistentVector" 1 resolve-vector-invoke)
  (register-ifn! "clojure.lang.PersistentVector" 1 resolve-vector-invoke)

  ;; Set as IFn: (s val)
  (register-ifn! "clojure.lang.APersistentSet" 1 resolve-set-invoke)
  (register-ifn! "clojure.lang.PersistentHashSet" 1 resolve-set-invoke))

(defn init!
  "Initialize all built-in stubs. Call once at startup."
  []
  (register-core-stubs!)
  (register-static-stubs!)
  (register-ifn-stubs!))

;; Auto-init on namespace load
(init!)
