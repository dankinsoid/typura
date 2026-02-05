(ns typura.subtype-test
  (:require [clojure.test :refer [deftest is testing]]
            [typura.subtype :as sut])
  (:import [clojure.lang IFn ILookup Indexed Seqable Associative Counted]))

(deftest identity-subtype
  (is (sut/subtype? :int :int))
  (is (sut/subtype? :string :string))
  (is (sut/subtype? :any :any)))

(deftest any-is-top
  (is (sut/subtype? :int :any))
  (is (sut/subtype? :string :any))
  (is (sut/subtype? :nil :any))
  (is (sut/subtype? :boolean :any))
  (is (sut/subtype? [:or :int :string] :any)))

(deftest primitive-hierarchy
  (testing "direct parents"
    (is (sut/subtype? :int :number))
    (is (sut/subtype? :double :number)))
  (testing "transitive"
    (is (sut/subtype? :int :any))
    (is (sut/subtype? :double :any)))
  (testing "not subtypes"
    (is (not (sut/subtype? :string :number)))
    (is (not (sut/subtype? :number :int)))
    (is (not (sut/subtype? :boolean :number)))))

(deftest union-subtyping
  (testing "value is subtype of union containing it"
    (is (sut/subtype? :int [:or :int :string]))
    (is (sut/subtype? :string [:or :int :string])))
  (testing "value not in union"
    (is (not (sut/subtype? :boolean [:or :int :string]))))
  (testing "union is subtype of super if all members are"
    (is (sut/subtype? [:or :int :double] :number))
    (is (not (sut/subtype? [:or :int :string] :number))))
  (testing "nil in union"
    (is (sut/subtype? :nil [:or :int :nil]))))

(deftest map-structural-subtyping
  (testing "sub has all keys of super"
    (is (sut/subtype? [:map [:a :int] [:b :string]]
                      [:map [:a :int]])))
  (testing "sub missing key"
    (is (not (sut/subtype? [:map [:a :int]]
                           [:map [:a :int] [:b :string]]))))
  (testing "value type mismatch"
    (is (not (sut/subtype? [:map [:a :string]]
                           [:map [:a :int]])))))

(deftest fn-subtyping
  (testing "contravariant params, covariant return"
    ;; fn accepting :any and returning :int is subtype of fn accepting :int and returning :number
    (is (sut/subtype? [:=> [:cat :any] :int]
                      [:=> [:cat :int] :number])))
  (testing "arity mismatch"
    (is (not (sut/subtype? [:=> [:cat :int] :int]
                           [:=> [:cat :int :int] :int])))))

;; --- Phase 3: Collection & Capability Subtyping ---

(deftest vector-subtyping
  (testing "covariant element"
    (is (sut/subtype? [:vector :int] [:vector :number]))
    (is (not (sut/subtype? [:vector :number] [:vector :int]))))
  (testing "identity"
    (is (sut/subtype? [:vector :string] [:vector :string])))
  (testing "to :any"
    (is (sut/subtype? [:vector :int] :any))))

(deftest set-subtyping
  (testing "covariant element"
    (is (sut/subtype? [:set :int] [:set :number])))
  (testing "identity"
    (is (sut/subtype? [:set :keyword] [:set :keyword]))))

(deftest map-of-subtyping
  (testing "covariant"
    (is (sut/subtype? [:map-of :keyword :int] [:map-of :keyword :number])))
  (testing "key mismatch"
    (is (not (sut/subtype? [:map-of :string :int] [:map-of :keyword :int])))))

(deftest structural-map-to-map-of
  (testing "keyword map fits map-of"
    (is (sut/subtype? [:map [:a :int] [:b :string]]
                      [:map-of :keyword :any])))
  (testing "value type must fit"
    (is (not (sut/subtype? [:map [:a :int] [:b :string]]
                           [:map-of :keyword :int])))))

(deftest interface-subtyping
  (testing "vector satisfies interfaces"
    (is (sut/subtype? [:vector :int] IFn))
    (is (sut/subtype? [:vector :int] Indexed))
    (is (sut/subtype? [:vector :int] Seqable))
    (is (sut/subtype? [:vector :int] Counted)))
  (testing "map satisfies interfaces"
    (is (sut/subtype? [:map [:a :int]] ILookup))
    (is (sut/subtype? [:map [:a :int]] Associative))
    (is (not (sut/subtype? [:map [:a :int]] IFn))))
  (testing "set satisfies interfaces"
    (is (sut/subtype? [:set :int] IFn))
    (is (sut/subtype? [:set :int] Counted))
    (is (not (sut/subtype? [:set :int] Indexed))))
  (testing "keyword satisfies interfaces"
    (is (sut/subtype? :keyword IFn))
    (is (not (sut/subtype? :keyword ILookup)))
    (is (not (sut/subtype? :keyword Indexed)))))
