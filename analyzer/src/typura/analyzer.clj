(ns typura.analyzer
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [typura.infer :as infer]
            [typura.context :as ctx]))

(defn analyze-form
  "Analyze a Clojure form and return its inferred type."
  [form]
  (let [ast (ana.jvm/analyze form)
        initial-ctx (ctx/make-context)
        [inferred-type final-ctx] (infer/infer-node ast initial-ctx)]
    {:type (ctx/resolve-deep final-ctx inferred-type)
     :diagnostics (ctx/get-diagnostics final-ctx)
     :ctx final-ctx}))
