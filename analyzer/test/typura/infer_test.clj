(ns typura.infer-test
  (:require [clojure.test :refer [deftest is testing]]
            [typura.analyzer :as sut]))

(deftest const-inference
  (testing "integer literal"
    (is (= :int (:type (sut/analyze-form '1)))))
  (testing "double literal"
    (is (= :double (:type (sut/analyze-form '1.5)))))
  (testing "string literal"
    (is (= :string (:type (sut/analyze-form '"hello")))))
  (testing "keyword literal"
    (is (= :keyword (:type (sut/analyze-form ':foo)))))
  (testing "boolean literal"
    (is (= :boolean (:type (sut/analyze-form 'true)))))
  (testing "nil literal"
    (is (= :nil (:type (sut/analyze-form 'nil))))))

(deftest arithmetic-inference
  (testing "addition"
    (is (= :number (:type (sut/analyze-form '(+ 1 2))))))
  (testing "nested arithmetic"
    (is (= :number (:type (sut/analyze-form '(+ (* 2 3) (- 4 1))))))))

(deftest let-inference
  (testing "simple let"
    (is (= :int (:type (sut/analyze-form '(let [x 1] x))))))
  (testing "let with arithmetic"
    (is (= :number (:type (sut/analyze-form '(let [x 1] (+ x 2)))))))
  (testing "nested let"
    (is (= :number (:type (sut/analyze-form '(let [x 1] (let [y 2] (+ x y)))))))))

(deftest if-inference
  (testing "same branch types"
    (is (= :int (:type (sut/analyze-form '(if true 1 2))))))
  (testing "different branch types"
    (is (= [:or :int :string] (:type (sut/analyze-form '(if true 1 "x")))))))

(deftest fn-inference
  (testing "fn with arithmetic body — the MVP target"
    (is (= [:=> [:cat :number :number] :number]
           (:type (sut/analyze-form '(fn [a b] (+ a b))))))))

(deftest defn-inference
  (testing "defn infers function type"
    (is (= [:=> [:cat :number :number] :number]
           (:type (sut/analyze-form '(defn add [a b] (+ a b))))))))

(deftest str-inference
  (testing "str returns string"
    (is (= :string (:type (sut/analyze-form '(str 42)))))))

(deftest inc-dec-inference
  (testing "inc"
    (is (= :number (:type (sut/analyze-form '(inc 1))))))
  (testing "dec"
    (is (= :number (:type (sut/analyze-form '(dec 1)))))))
