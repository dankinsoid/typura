(ns typura.context-test
  (:require [clojure.test :refer [deftest is testing]]
            [typura.types :as t]
            [typura.context :as sut]))

(deftest make-context-test
  (let [ctx (sut/make-context)]
    (is (= {} (:bindings ctx)))
    (is (= {} (:globals ctx)))
    (is (= {} (:subst ctx)))))

(deftest bindings-test
  (let [ctx (-> (sut/make-context)
                (sut/extend-binding 'x :int)
                (sut/extend-binding 'y :string))]
    (is (= :int (sut/lookup-binding ctx 'x)))
    (is (= :string (sut/lookup-binding ctx 'y)))
    (is (nil? (sut/lookup-binding ctx 'z)))))

(deftest globals-test
  (let [ctx (-> (sut/make-context)
                (sut/extend-global 'clojure.core/+ [:=> [:cat :number :number] :number]))]
    (is (= [:=> [:cat :number :number] :number]
           (sut/lookup-global ctx 'clojure.core/+)))))

(deftest resolve-type-test
  (testing "non-tvar passes through"
    (is (= :int (sut/resolve-type {:subst {}} :int))))
  (testing "tvar resolves through substitution"
    (let [ctx {:subst {1 :number}}]
      (is (= :number (sut/resolve-type ctx [:tvar 1])))))
  (testing "tvar chain resolution"
    (let [ctx {:subst {1 [:tvar 2], 2 :string}}]
      (is (= :string (sut/resolve-type ctx [:tvar 1])))))
  (testing "unresolved tvar stays"
    (is (= [:tvar 99] (sut/resolve-type {:subst {}} [:tvar 99])))))

(deftest constrain-test
  (testing "tvar gets bound"
    (let [tv (t/fresh-tvar)
          ctx (sut/constrain (sut/make-context) tv :number)]
      (is (some? ctx))
      (is (= :number (sut/resolve-type ctx tv)))))
  (testing "compatible concrete types"
    (let [ctx (sut/constrain (sut/make-context) :int :number)]
      (is (some? ctx))))
  (testing "incompatible types return nil"
    (is (nil? (sut/constrain (sut/make-context) :string :number)))))

(deftest constrain-narrowing-test
  (testing "tvar narrowing: :number then :int narrows to :int"
    (let [tv (t/fresh-tvar)
          ctx (-> (sut/make-context)
                  (sut/constrain tv :number)
                  (sut/constrain tv :int))]
      (is (some? ctx))
      (is (= :int (sut/resolve-type ctx tv)))))
  (testing "tvar narrowing: :int then :number keeps :int"
    (let [tv (t/fresh-tvar)
          ctx (-> (sut/make-context)
                  (sut/constrain tv :int)
                  (sut/constrain tv :number))]
      (is (some? ctx))
      (is (= :int (sut/resolve-type ctx tv)))))
  (testing "incompatible constraints return nil"
    (let [tv (t/fresh-tvar)
          ctx (sut/constrain (sut/make-context) tv :number)]
      (is (nil? (sut/constrain ctx tv :string)))))
  (testing "same constraint twice is idempotent"
    (let [tv (t/fresh-tvar)
          ctx (-> (sut/make-context)
                  (sut/constrain tv :int)
                  (sut/constrain tv :int))]
      (is (some? ctx))
      (is (= :int (sut/resolve-type ctx tv))))))

(deftest resolve-deep-test
  (let [tv1 (t/fresh-tvar)
        tv2 (t/fresh-tvar)
        ctx (-> (sut/make-context)
                (assoc-in [:subst (t/tvar-id tv1)] :number)
                (assoc-in [:subst (t/tvar-id tv2)] :string))]
    (is (= [:=> [:cat :number :string] :number]
           (sut/resolve-deep ctx [:=> [:cat tv1 tv2] tv1])))))
