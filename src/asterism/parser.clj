(ns asterism.parser
  (require [asterism.parser [generator :refer [make-parser]]
                            [grammar :refer [make-grammar]]]))

(def default-leaf-handler identity)

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
        {:keys [node-handler leaf-handler disambiguators start whitespace terminals]
         :or {node-handler default-node-handler
              leaf-handler default-leaf-handler
              start (first prods)
              whitespace #"\s*"
              terminals {}}} opts
        grammar (make-grammar start terminals prods)]
    (make-parser grammar whitespace leaf-handler node-handler)))