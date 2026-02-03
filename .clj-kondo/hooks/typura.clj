(ns hooks.typura
  (:require [clj-kondo.hooks-api :as api]))

(defn defschema [{:keys [node]}]
  (let [args (rest (:children node))
        [name-node & rest-args] args]
    (when (api/token-node? name-node)
      (let [;; Handle optional arguments - count determines the form:
            ;; 1 arg: schema only, 2 args: args + schema
            [args-node schema-node] (if (= 2 (count rest-args))
                                     [(first rest-args) (second rest-args)]
                                     [nil (first rest-args)])]
        (if args-node
          ;; Multi-arity: transform to (defn name args schema)
          {:node (api/list-node
                  [(api/token-node 'defn)
                   name-node
                   args-node
                   schema-node])}
          ;; Single arity: transform to (def name schema)
          {:node (api/list-node
                  [(api/token-node 'def)
                   name-node
                   schema-node])})))))