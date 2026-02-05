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

(deftest variadic-inference
  (testing "str with multiple args"
    (is (= :string (:type (sut/analyze-form '(str "a" "b" "c"))))))
  (testing "str with no args"
    (is (= :string (:type (sut/analyze-form '(str))))))
  (testing "println with multiple args"
    (is (= :nil (:type (sut/analyze-form '(println 1 2 3))))))
  (testing "println with no args"
    (is (= :nil (:type (sut/analyze-form '(println)))))))

(deftest str-inference
  (testing "str returns string"
    (is (= :string (:type (sut/analyze-form '(str 42)))))))

(deftest inc-dec-inference
  (testing "inc"
    (is (= :number (:type (sut/analyze-form '(inc 1))))))
  (testing "dec"
    (is (= :number (:type (sut/analyze-form '(dec 1)))))))

;; --- Phase 1: Flow Analysis ---

(deftest guard-narrowing
  (testing "int? narrows to :int in then-branch"
    (is (= :number
           (:type (sut/analyze-form
                   '(let [x 1]
                      (if (int? x) (+ x 1) 0)))))))
  (testing "string? narrows to :string in then-branch"
    (is (= :string
           (:type (sut/analyze-form
                   '(let [x "hello"]
                      (if (string? x) (str x " world") ""))))))))

(deftest predicate-return-type
  (testing "int? returns boolean"
    (is (= :boolean (:type (sut/analyze-form '(int? 42))))))
  (testing "string? returns boolean"
    (is (= :boolean (:type (sut/analyze-form '(string? "x")))))))

(deftest else-branch-subtraction
  (testing "guard subtracts type in else-branch — return x to see narrowed type"
    ;; x is [:or :int :string], int? narrows then to :int,
    ;; else-branch subtracts :int → x is :string
    ;; both branches return :string → result is :string
    (is (= :string
           (:type (sut/analyze-form
                   '(let [x (if true 1 "hello")]
                      (if (int? x)
                        "was int"
                        x)))))))
  (testing "subtype-aware subtraction: number? removes int from union"
    ;; x is [:or :int :string], number? narrows then to :number,
    ;; else subtracts :number (removes :int as subtype) → x is :string
    (is (= :string
           (:type (sut/analyze-form
                   '(let [x (if true 1 "hello")]
                      (if (number? x)
                        "was number"
                        x)))))))
  (testing "guard on non-union leaves else as-is"
    (is (= :number
           (:type (sut/analyze-form
                   '(let [x 42]
                      (if (int? x)
                        (+ x 1)
                        (+ x 2)))))))))

(deftest nested-guard-do-wrapper
  (testing "guard detected through do wrapper (when macro expansion)"
    ;; (when pred body) expands to (if pred (do body) nil)
    ;; but the test itself can also be wrapped in do by macros
    (is (= :number
           (:type (sut/analyze-form
                   '(let [x 1]
                      (if (do (int? x)) (+ x 1) 0))))))))

(deftest truthiness-narrowing
  (testing "if with local test removes nil in then-branch — return x"
    ;; x is [:or :int :nil], truthiness removes :nil in then → x is :int
    ;; else returns 0 (:int), so result is :int
    (is (= :int
           (:type (sut/analyze-form
                   '(let [x (if true 1 nil)]
                      (if x x 0)))))))
  (testing "nil-only type: then-branch unreachable, result is else type"
    (is (= :string
           (:type (sut/analyze-form
                   '(let [x nil]
                      (if x x "was nil"))))))))

(deftest union-normalization
  (testing "same branch types produce single type, not union"
    (is (= :int (:type (sut/analyze-form '(if true 1 2))))))
  (testing "different branch types produce union"
    (is (= [:or :int :string]
           (:type (sut/analyze-form '(if true 1 "x")))))))

;; --- Phase 2: Diagnostics ---

(deftest type-mismatch-diagnostic
  (testing "string arg where number expected"
    (let [result (sut/analyze-form '(+ "hello" 1))
          diags (:diagnostics result)]
      (is (= 1 (count diags)))
      (is (= :type-mismatch (:code (first diags))))
      (is (= :error (:level (first diags))))
      (is (= :number (:expected (first diags))))
      (is (= :string (:actual (first diags))))))
  (testing "multiple mismatches reported"
    (let [result (sut/analyze-form '(+ "a" "b"))
          diags (:diagnostics result)]
      (is (= 2 (count diags)))
      (is (every? #(= :type-mismatch (:code %)) diags))))
  (testing "no diagnostic on valid call"
    (let [result (sut/analyze-form '(+ 1 2))]
      (is (empty? (:diagnostics result))))))

;; --- Phase 3: Collections ---

(deftest vector-literal-inference
  (testing "const vector of ints"
    (is (= [:vector :int] (:type (sut/analyze-form '[1 2 3])))))
  (testing "const vector of mixed types"
    (is (= [:vector [:or :int :string]]
           (:type (sut/analyze-form '[1 "a"])))))
  (testing "non-const vector"
    (is (= [:vector :number]
           (:type (sut/analyze-form '[(+ 1 2) (+ 3 4)])))))
  (testing "empty vector"
    ;; empty literal []; tools.analyzer may fold to const
    (is (= [:vector :any] (:type (sut/analyze-form '[]))))))

(deftest map-literal-inference
  (testing "const keyword map"
    (is (= [:map [:a :int] [:b :string]]
           (:type (sut/analyze-form '{:a 1 :b "hi"})))))
  (testing "non-const keyword map"
    (is (= [:map [:a :number] [:b :string]]
           (:type (sut/analyze-form '{:a (+ 1 2) :b (str "x")})))))
  (testing "empty map"
    (is (= [:map-of :any :any] (:type (sut/analyze-form '{}))))))

(deftest set-literal-inference
  (testing "const set of ints"
    (is (= [:set :int] (:type (sut/analyze-form '#{1 2 3})))))
  (testing "non-const set"
    (is (= [:set :number]
           (:type (sut/analyze-form '#{(+ 1 2) (+ 3 4)}))))))

(deftest keyword-invoke-inference
  (testing "keyword access on typed map"
    (is (= :int (:type (sut/analyze-form '(:a {:a 1 :b "hi"}))))))
  (testing "keyword access on let-bound map"
    (is (= :string
           (:type (sut/analyze-form '(let [m {:x 1 :y "hello"}] (:y m)))))))
  (testing "unknown key returns :any"
    (is (= :any (:type (sut/analyze-form '(:z {:a 1})))))))

(deftest get-inference
  (testing "get on keyword map with const key"
    (is (= :int (:type (sut/analyze-form '(get {:a 1 :b "hi"} :a))))))
  (testing "get unknown key"
    (is (= :any (:type (sut/analyze-form '(get {:a 1} :z)))))))

(deftest nth-inference
  (testing "nth on vector"
    (is (= :int (:type (sut/analyze-form '(nth [1 2 3] 0)))))))

(deftest destructuring-inference
  ;; Map destructuring deferred — see CLAUDE.md Phase 3 note.
  ;; The macro expansion `(if (seq? m) ... m)` loses map type info.
  (testing "sequential destructuring via macro expansion"
    (is (= :int
           (:type (sut/analyze-form
                   '(let [[a b] [1 2]] a)))))))

;; --- Phase 6: Functional Stubs & Inline Annotations ---

(deftest inline-annotation
  (testing "defn with :typura/sig overrides inferred type"
    ;; Define an annotated function, then call it
    (let [result (sut/analyze-form
                   '(do (defn ^{:typura/sig [:=> [:cat :int :string] :number]}
                          my-fn [a b] (+ a 1))
                        (my-fn 1 "hello")))]
      (is (= :number (:type result)))
      (is (empty? (:diagnostics result)))))
  (testing "defn annotation produces type-mismatch on wrong args"
    (let [result (sut/analyze-form
                   '(do (defn ^{:typura/sig [:=> [:cat :int :string] :number]}
                          my-fn2 [a b] (+ a 1))
                        (my-fn2 "wrong" 42)))
          diags (:diagnostics result)]
      (is (= 2 (count diags)))
      (is (every? #(= :type-mismatch (:code %)) diags)))))

;; --- Phase 2b: Bidirectional Inference & Constraint Accumulation ---

(deftest tvar-constraint-narrowing
  (testing "two constraints narrow: + gives :number, bit-and narrows to :int"
    (is (= [:=> [:cat :int] :int]
           (:type (sut/analyze-form
                   '(fn [x] (do (+ x 1) (bit-and x 3))))))))
  (testing "same constraint twice is stable"
    (is (= [:=> [:cat :number] :number]
           (:type (sut/analyze-form
                   '(fn [x] (do (+ x 1) (inc x)))))))))

(deftest declared-type-no-narrowing
  (testing "annotated :number param + bit-and produces type-mismatch"
    (let [result (sut/analyze-form
                   '(defn ^{:typura/sig [:=> [:cat :number] :int]}
                      f [x] (bit-and x 3)))
          diags (:diagnostics result)]
      (is (= 1 (count diags)))
      (is (= :type-mismatch (:code (first diags))))
      (is (= :int (:expected (first diags))))
      (is (= :number (:actual (first diags)))))))

(deftest return-type-mismatch-diagnostic
  (testing "defn body return type conflicts with annotation"
    (let [result (sut/analyze-form
                   '(defn ^{:typura/sig [:=> [:cat :int] :string]}
                      bad-fn [x] (+ x 1)))
          diags (:diagnostics result)]
      (is (= 1 (count diags)))
      (is (= :return-type-mismatch (:code (first diags))))
      (is (= :string (:expected (first diags))))
      (is (= :number (:actual (first diags))))))
  (testing "no diagnostic when return type matches"
    (let [result (sut/analyze-form
                   '(defn ^{:typura/sig [:=> [:cat :int] :number]}
                      good-fn [x] (+ x 1)))]
      (is (empty? (:diagnostics result)))))
  (testing "no diagnostic when body returns :any"
    (let [result (sut/analyze-form
                   '(defn ^{:typura/sig [:=> [:cat :int] :string]}
                      unknown-fn [x] (first [x])))]
      (is (empty? (:diagnostics result))))))

(deftest bidir-fn-arg-pushdown
  (testing "fn arg receives expected param types from annotated caller"
    (let [result (sut/analyze-form
                   '(do (defn ^{:typura/sig [:=> [:cat [:=> [:cat :int] :string]] :string]}
                          apply-fn [f] (f 1))
                        (apply-fn (fn [x] (str x)))))]
      (is (empty? (:diagnostics result))))))

(deftest bidir-if-branches
  (testing "expected type pushed through if branches"
    (let [result (sut/analyze-form
                   '(defn ^{:typura/sig [:=> [:cat :boolean] :string]}
                      to-str [flag]
                      (if flag "yes" "no")))]
      (is (empty? (:diagnostics result))))))

