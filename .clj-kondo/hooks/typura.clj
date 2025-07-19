(ns hooks.typura
  (:require [clj-kondo.hooks-api :as api]))

(defn defschema [{:keys [node]}]
  (let [args (rest (:children node))
        [name-node & rest-args] args]
    (when (api/token-node? name-node)
      (let [name (api/sexpr name-node)
            ;; Handle optional arguments - count determines the form:
            ;; 1 arg: schema only, 2 args: args + schema
            [args-node schema-node] (if (= 2 (count rest-args))
                                     [(first rest-args) (second rest-args)]
                                     [nil (first rest-args)])
            ;; Create a def form for the schema
            def-node (api/list-node
                      [(api/token-node 'def)
                       name-node
                       schema-node])
            ;; Create a defn form for the constructor function
            constructor-name (symbol (str "->" name))
            constructor-node (api/list-node
                             [(api/token-node 'defn)
                              (api/token-node constructor-name)
                              (or args-node (api/vector-node []))
                              (api/token-node 'nil)])]
        {:node (api/list-node
                [(api/token-node 'do)
                 def-node
                 constructor-node])}))))