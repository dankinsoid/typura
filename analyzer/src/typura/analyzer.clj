(ns typura.analyzer
  (:require
   [malli.core :as m]
   [clojure.tools.analyzer :as ana]
   [clojure.tools.analyzer.passes :as ana.passes]
   [clojure.tools.analyzer.ast :as ast]
   [clojure.tools.analyzer.env :as ana.env]
   [clojure.tools.analyzer.jvm :as ana.jvm]
   [cljs.analyzer :as ana.js]
   [clojure.edn :as edn]))

(defn compile-ast [code]
  (let [ast (ana.jvm/analyze code)]
    ast))

(compile-ast '(let [x 1] (let [y 4] (+ x y))))

{:children [:bindings :body]
 :bindings [{:children [:init]
             :init {:op :const
                    :env {:context :ctx/expr, :locals {}, :ns 'typura.analyzer, :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj", :column 15, :line 16}
                    :type :number
                    :literal? true
                    :val 1
                    :form 1
                    :o-tag 'long
                    :tag 'long}
             :name 'x__#0
             :op :binding
             :env {:context :ctx/expr, :locals {}, :ns 'typura.analyzer, :column 15, :line 16, :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj"}
             :o-tag 'long
             :form 'x
             :tag 'long
             :atom #atom[{:tag 'long}]
             :local :let}]
 :op :let
 :env {:context :ctx/expr
       :locals {}
       :ns 'typura.analyzer
       :column 15
       :line 16
       :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj"}
 :o-tag 'long
 :top-level true
 :form '(let* [x 1] (let [y 4] (+ x y)))
 :tag 'long
 :body {:children [:bindings :body]
        :body? true
        :bindings [{:children [:init]
                    :init {:op :const
                           :env {:context :ctx/expr
                                 :locals {'x {:op :binding
                                              :name 'x
                                              :init {:op :const
                                                     :env {:context :ctx/expr
                                                           :locals {}
                                                           :ns 'typura.analyzer}
                                                     :type :number
                                                     :literal? true
                                                     :val 1
                                                     :form 1}
                                              :form 'x
                                              :local :let
                                              :children [:init]}}
                                 :ns 'typura.analyzer
                                 :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj"
                                 :column 26
                                 :line 16}
                           :type :number
                           :literal? true
                           :val 4
                           :form 4
                           :o-tag 'long
                           :tag 'long}
                           :name 'y__#0
                           :op :binding
                           :env {:context :ctx/expr
                                 :locals {'x {:op :binding
                                              :name 'x
                                              :init {:op :const
                                                     :env {:context :ctx/expr
                                                           :locals {}
                                                           :ns 'typura.analyzer}
                                                     :type :number
                                                     :literal? true
                                                     :val 1
                                                     :form 1}
                                              :form 'x
                                              :local :let
                                              :children [:init]}}
                                 :ns 'typura.analyzer
                                 :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj"
                                 :column 26
                                 :line 16}
                           :o-tag 'long
                           :form 'y
                           :tag 'long
                           :atom #atom[{:tag 'long}]
                           :local :let}]
                           :op :let
                           :env {:context :ctx/expr
                                 :locals {'x {:op :binding
                                              :name 'x
                                              :init {:op :const
                                                     :env {:context :ctx/expr
                                                           :locals {}
                                                           :ns 'typura.analyzer}
                                                     :type :number
                                                     :literal? true
                                                     :val 1
                                                     :form 1}
                                              :form 'x
                                              :local :let
                                              :children [:init]}}
                                 :ns 'typura.analyzer
                                 :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj"
                                 :column 26
                                 :line 16}
                           :o-tag 'long
                           :form '(let* [y 4] (+ x y))
                           :tag 'long
                           :body {:args [{:children []
                                          :name 'x__#0
                                          :op :local
                                          :env {:context :ctx/expr
                                                :locals {'x {:op :binding
                                                             :name 'x
                                                             :init {:op :const, :env {:context :ctx/expr, :locals {}, :ns typura.analyzer}, :type :number, :literal? true, :val 1, :form 1}
                                                             :form 'x
                                                             :local :let
                                                             :children [:init]}
                                                         'y {:op :binding
                                                             :name 'y
                                                             :init {:op :const, :env {:context :ctx/expr, :locals {'x {:op :binding, :name 'x, :init {:op :const, :env {:context :ctx/expr, :locals {}, :ns typura.analyzer}, :type :number, :literal? true, :val 1, :form 1}, :form 'x, :local :let, :children [:init]}}, :ns 'typura.analyzer}, :type :number, :literal? true, :val 4, :form 4}
                                                             :form 'y
                                                             :local :let
                                                             :children [:init]}}
                                                :ns 'typura.analyzer
                                                :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj"
                                                :column 37
                                                :line 16}
                                          :o-tag 'long
                                          :form 'x
                                          :tag 'long
                                          :atom #atom[{:tag 'long}]
                                          :local :let
                                          :assignable? false} 
                                          {:children []
                                           :name 'y__#0
                                           :op :local
                                           :env {:context :ctx/expr, :locals {'x {:op :binding, :name 'x, :init {:op :const, :env {:context :ctx/expr, :locals {}, :ns 'typura.analyzer}, :type :number, :literal? true, :val 1, :form 1}, :form 'x, :local :let, :children [:init]},'y {:op :binding, :name y, :init {:op :const, :env {:context :ctx/expr, :locals {x {:op :binding, :name x, :init {:op :const, :env {:context :ctx/expr, :locals {}, :ns typura.analyzer}, :type :number, :literal? true, :val 1, :form 1}, :form x, :local :let, :children [:init]}}, :ns typura.analyzer}, :type :number, :literal? true, :val 4, :form 4}, :form y, :local :let, :children [:init]}}, :ns typura.analyzer, :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj", :column 37, :line 16}, :o-tag long, :form y, :tag long, :atom #atom[{:tag long}], :local :let, :assignable? false}], :children [:args], :body? true, :method add, :op :static-call, :env {:context :ctx/expr, :locals {x {:op :binding, :name x, :init {:op :const, :env {:context :ctx/expr, :locals {}, :ns typura.analyzer}, :type :number, :literal? true, :val 1, :form 1}, :form x, :local :let, :children [:init]}, y {:op :binding, :name y, :init {:op :const, :env {:context :ctx/expr, :locals {x {:op :binding, :name x, :init {:op :const, :env {:context :ctx/expr, :locals {}, :ns typura.analyzer}, :type :number, :literal? true, :val 1, :form 1}, :form x, :local :let, :children [:init]}}, :ns typura.analyzer}, :type :number, :literal? true, :val 4, :form 4}, :form y, :local :let, :children [:init]}}, :ns typura.analyzer, :file "/Users/danil/Code/typura/analyzer/src/typura/analyzer.clj", :column 37, :line 16}, :o-tag long, :class clojure.lang.Numbers, :form (. clojure.lang.Numbers (add x y)), :tag long, :validated? true, :raw-forms ((do (+ x y)) (+ x y))}, :raw-forms ((do (let [y 4] (+ x y))) (let [y 4] (+ x y)))}, :raw-forms ((let [x 1] (let [y 4] (+ x y))))}
