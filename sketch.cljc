

(defschema Address
  {:id :string
   :tags [:set :keyword]
   :address
   {:street :string
    :city :string
    :zip :int
    :lonlat [:tuple :double :double]}})


{:id "123" :tags #{:home :work} :address {:street "123 Main St" :city "Anytown" :zip 12345 :lonlat [123.456 789.012]}}

(deftype Schema [schema]
  Object
  (toString [_] (str schema))

  IEquiv
  (-equiv [this other]
    (compare schema (.-schema other)))

  IHash
  (-hash [this]
    (hash schema))

  IFn
  (-invoke [_ value] value))

(def Address (Schema
              {:id :string
               :tags [:set :keyword]
               :address
               {:street :string
                :city :string
                :zip :int
                :lonlat [:tuple :double :double]}}))

(Address
 {:id "123"
  :tags #{:home :work}
  :address {:street "123 Main St"
            :city "Anytown"
            :zip 12345
            :lonlat [123.456 789.012]}})

(as Address {:id "123" :tags #{:home :work} :address {:street "123 Main St" :city "Anytown" :zip 12345 :lonlat [123.456 789.012]}})

(defschema Array [Element]
  [:vector Element])

(defn Array [Element] (Schema [:vector Element]))

(if (is? :int 0)
  (println "int")
  (println "not int"))

(if (some? nil)
  (println "some")
  (println "not some"))

(if (nil? nil)
  (println "nil")
  (println "not nil"))

(is? :int 0)

((Array :int) [1 2 3])

(defn FixedArray [size Element]
  (Schema (concat [:tuple] (repeat size Element))))

(FixedArray 3 :int)
[:fixed-array 3 :int]

(defschema Maybe [Value]
  [:enum Value nil])

(defschema Result [Success Failure]
  [:enum Success Failure])

(defschema Status
  [:enum :ok :error])

(defschema TypeKey [Root]
  [:enum (all-keys Root)])

(defschema KeyPath [Root Value]
  [:map
   [:key (TypeKey Root)]
   [:root (shema Root)]
   [:value (shema Value)]])

{:type :address, :value nil}

(def ^Address addr)

(defn get-address [&args]
  args)

(defn [T] get-address
  [city Address
   zip :int
   array [:maybe [:vector T]]
   lonlat [:tuple :double :double]])
