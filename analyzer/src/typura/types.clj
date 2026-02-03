(ns typura.types)

;; Primitive type hierarchy: child -> set of direct parents
(def primitive-parents
  {:int     #{:number}
   :double  #{:number}
   :number  #{:any}
   :string  #{:any}
   :keyword #{:any}
   :boolean #{:any}
   :nil     #{:any}
   :symbol  #{:any}
   :any     #{}})

(def ^:private tvar-counter (atom 0))

(defn fresh-tvar
  "Create a fresh type variable."
  []
  [:tvar (swap! tvar-counter inc)])

(defn tvar? [t]
  (and (vector? t) (= :tvar (first t))))

(defn tvar-id [t]
  (second t))

(defn fn-type? [t]
  (and (vector? t) (= :=> (first t))))

(defn union-type? [t]
  (and (vector? t) (= :or (first t))))

(defn map-type? [t]
  (and (vector? t) (= :map (first t))))

(defn val->type
  "Map a JVM value to a Malli type keyword."
  [val]
  (cond
    (nil? val)                     :nil
    (instance? Boolean val)        :boolean
    (instance? Long val)           :int
    (instance? Integer val)        :int
    (instance? Double val)         :double
    (instance? Float val)          :double
    (instance? Number val)         :number
    (instance? String val)         :string
    (keyword? val)                 :keyword
    (symbol? val)                  :symbol
    :else                          :any))

(defn tag->type
  "Map a tools.analyzer JVM tag to a Malli type."
  [tag]
  (case tag
    long    :int
    int     :int
    double  :double
    float   :double
    boolean :boolean
    void    :nil
    nil))
