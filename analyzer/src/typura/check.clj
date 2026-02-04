(ns typura.check
  "Shared type-checking utilities used by both stubs and inference."
  (:require [typura.types :as t]
            [typura.context :as ctx]
            [typura.subtype :as sub]))

(defn node->loc
  "Extract source location from an AST node."
  [node]
  (let [env (:env node)]
    (when (and (:line env) (:column env))
      {:line (:line env) :col (:column env)})))

(defn- parse-params
  "Split [:cat ...] into [fixed-params variadic-element-type-or-nil].
   Variadic tail is the last element if it's [:* T] or [:+ T]."
  [cat-schema]
  (let [params (vec (rest cat-schema)) ; skip :cat
        last-p (peek params)]
    (if (and last-p (t/repeat-type? last-p))
      [(pop params) (second last-p)]
      [params nil])))

(defn apply-fn-type
  "Apply a function type to argument types. Constrains args and returns result type.
   `arg-nodes` are AST nodes used for diagnostic locations."
  [ctx fn-type arg-types arg-nodes]
  (let [[fixed-params variadic-type] (parse-params (second fn-type))
        return-type (nth fn-type 2)
        n-fixed (count fixed-params)
        n-args (count arg-types)]
    (cond
      (< n-args n-fixed)
      [return-type ctx]

      (and (> n-args n-fixed) (nil? variadic-type))
      [return-type ctx]

      :else
      (let [;; Constrain fixed params, collecting type-mismatch diagnostics
            ctx' (reduce
                   (fn [c [arg-t param-t arg-node]]
                     (let [result (ctx/constrain c arg-t param-t)]
                       (or result
                           (ctx/emit-diagnostic c
                             {:level :error :code :type-mismatch
                              :message (str "Expected " param-t ", got " arg-t)
                              :loc (node->loc arg-node)
                              :expected param-t :actual arg-t}))))
                   ctx
                   (map vector arg-types fixed-params arg-nodes))
            ;; Constrain variadic args
            ctx'' (if variadic-type
                    (reduce (fn [c [arg-t arg-node]]
                              (let [result (ctx/constrain c arg-t variadic-type)]
                                (or result
                                    (ctx/emit-diagnostic c
                                      {:level :error :code :type-mismatch
                                       :message (str "Expected " variadic-type ", got " arg-t)
                                       :loc (node->loc arg-node)
                                       :expected variadic-type :actual arg-t}))))
                            ctx'
                            (map vector (drop n-fixed arg-types) (drop n-fixed arg-nodes)))
                    ctx')]
        [return-type ctx'']))))

(defn map-get-type
  "Extract value type from a map type for a given keyword key.
   Handles unions by checking each member."
  [coll-type key-val]
  (cond
    (and (t/map-type? coll-type) (keyword? key-val))
    (some (fn [entry] (when (= key-val (first entry)) (second entry)))
          (rest coll-type))

    (t/map-of-type? coll-type)
    (nth coll-type 2)

    ;; Union: try each member, return first match
    (t/union-type? coll-type)
    (some #(map-get-type % key-val) (rest coll-type))

    :else nil))

(defn coll-nth-type
  "Extract element type from a vector type."
  [coll-type]
  (when (t/vector-type? coll-type)
    (second coll-type)))
