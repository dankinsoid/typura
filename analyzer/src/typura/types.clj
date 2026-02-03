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

(defn repeat-type?
  "[:* type] or [:+ type] — variadic tail in :cat."
  [t]
  (and (vector? t) (#{:* :+} (first t))))

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

(defn normalize-union
  "Flatten nested :or, deduplicate members, unwrap singletons.
   Removes :nothing (bottom type) from unions."
  [t]
  (if-not (union-type? t)
    t
    (let [members (->> (rest t)
                       (mapcat (fn [m]
                                 (if (union-type? m)
                                   (rest (normalize-union m))
                                   [m])))
                       (remove #{:nothing})
                       distinct
                       vec)]
      (case (count members)
        0 :nothing
        1 (first members)
        (into [:or] members)))))

(defn subtract-type
  "Remove `to-remove` from type `t`. For else-branch narrowing.
   Only meaningful on unions; `:any` stays `:any`."
  [t to-remove]
  (cond
    (= t to-remove) :nothing
    (= t :any) :any
    (union-type? t)
    (let [remaining (remove (fn [m] (= m to-remove)) (rest t))]
      (normalize-union (into [:or] remaining)))
    :else t))

(defn remove-falsy
  "Remove :nil and :boolean from a union (for truthiness narrowing in then-branch).
   On non-union types: if type is :nil return :nothing, otherwise leave as-is."
  [t]
  (cond
    (= t :nil) :nothing
    (union-type? t)
    (let [remaining (remove #{:nil} (rest t))]
      (normalize-union (into [:or] remaining)))
    :else t))

(defn add-falsy
  "Add :nil to a type (for else-branch of truthiness narrowing)."
  [t]
  (cond
    (= t :nil) :nil
    (union-type? t)
    (if (some #{:nil} (rest t))
      t
      (normalize-union (into [:or] (conj (vec (rest t)) :nil))))
    :else (normalize-union [:or t :nil])))

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
