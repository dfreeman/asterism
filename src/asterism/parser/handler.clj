(ns asterism.parser.handler
  (:require [asterism.parser.protocols :as p]))

(defn default-leaf-handler [x] x)

(defn default-node-handler [lhs rhs children]
  {:type lhs :children children})

(defn elide-terms [pred? handler]
  (fn [lhs rhs children]
    (let [pred? #(try (pred? (p/token-type %)) (catch Exception e))]
      (handler lhs rhs (filter (comp not pred?) children)))))

(defn collapse-prods [pred? handler]
  (fn [lhs rhs children]
    (if (try (pred? lhs) (catch Exception e))
      children
      (handler lhs rhs children))))