(ns typura.subtype-test
  (:require [clojure.test :refer [deftest is testing]]
            [typura.subtype :as sut]))

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
