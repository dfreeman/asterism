(ns asterism.parser
  (require [asterism.parser [generator :as g]]))

(defn default-leaf-handler [x] x)

(defn default-node-handler [lhs rhs children]
  {:type lhs :children children})

(defn elide-terms [pred? handler]
  (fn [lhs rhs children]
    (let [pred? #(try (pred? (:token-type %)) (catch Exception e))]
      (handler lhs rhs (filter (comp not pred?) children)))))

(defn collapse-prods [pred? handler]
  (fn [lhs rhs children]
    (if (try (pred? lhs) (catch Exception e))
      children
      (handler lhs rhs children))))

(defn parser
  "Creates a parser for the given productions, using the given options."
  [opts & prods]
  (let [[opts prods] (if (map? opts) [opts prods] [{} (cons opts prods)])
        {:keys [node-handler leaf-handler start whitespace terminals]
         :or {node-handler default-node-handler
              leaf-handler default-leaf-handler
              start (first prods)
              whitespace #"\s*"
              terminals {}}} opts
        grammar (g/make-grammar start terminals prods)]
    (g/make-parser grammar whitespace leaf-handler node-handler)))