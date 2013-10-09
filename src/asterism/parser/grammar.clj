(ns asterism.parser.grammar
  (:require [asterism.parser.scanner :as s]
            [asterism.util :as u]
            [slingshot.slingshot :refer [throw+]]))

;;;;;;;; Terminal Processing ;;;;;;;;;

(defn make-terminal [[id definition]]
  (let [invalid-term {:type ::invalid-terminal :id id :definition definition}]
    (cond 
      (map? definition)
        (if (satisfies? s/Matcher (:matcher definition))
          (assoc definition :id id)
          (throw+ (assoc invalid-term :msg "Invalid matcher")))
      (satisfies? s/Matcher definition)
        {:id id :matcher definition}
      :else
        (throw+ invalid-term))))

(defn- generate-id [matcher]
  (let [type (.toLowerCase (.getSimpleName (type matcher)))]
    (keyword (str type "-" matcher))))

(defn- matcher-key [literal]
  (if (instance? java.util.regex.Pattern literal)
    {:pattern (str literal)}
    literal))

(defn- terminal-lookup [raw-terminals]
  (let [terms (set (map make-terminal raw-terminals))
        by-id (into {} (map #(vector (:id %) %) terms))
        by-matcher (->> terms
                     (map #(vector (matcher-key (:matcher %)) %))
                     (into {}))]
    (merge by-id by-matcher)))

(defn- process-terminals [nonterminals explicits prods]
  (let [initial (into explicits {:asterism/empty ""})
        terminals (atom (terminal-lookup initial))
        prods (into {} (for [[lhs prod] prods]
                [lhs
                (set (for [alternative prod]
                  (vec (for [element alternative]
                    (if (contains? nonterminals element)
                      element
                      (let [matcher (matcher-key element)
                            id (generate-id element)]
                        (if (contains? @terminals matcher)
                          (let [result (:id (get @terminals matcher))]
                            (if (instance? clojure.lang.IMeta result)
                              (with-meta result (meta element))
                              result))
                          (do 
                            (swap! terminals assoc
                              matcher (make-terminal [id element]))
                            id))))))))]))
        terminals (->> @terminals
                    (map (fn [[matcher term]] [(:id term) term]))
                    (into {}))]
    [terminals prods]))


;;;;;; Production Normalization ;;;;;;

(defn normalize
  "Takes an arbitrary grammar RHS fragment and returns a single set
  of vectors, each containing only elements of (T u NT)"
  [rhs]
  (letfn [(append-all [lhs-vecs rhs-vecs]
            (set (for [lhs lhs-vecs
                       rhs rhs-vecs]
                    (vec (concat lhs rhs)))))

          (normalize-vec [v]
            (->> v
              (map #(normalize %))
              (vec)
              (reduce append-all #{[]})
              u/set-flatten))

          (normalize-set [s]
            (->> s
              (map #(normalize %))
              (set)
              u/set-flatten))

          (normalize-node [x] #{[x]})]

      (condp apply [rhs]
        set? (normalize-set rhs)
        vector? (normalize-vec rhs)
        (normalize-node rhs))))


;;;;;;;;;;;;;;;; Build ;;;;;;;;;;;;;;;

(defn make-grammar [start explicit-terminals prods]
  (let [prods (->> prods
                (apply hash-map)
                (#(assoc % :asterism/start start))
                (reduce-kv #(assoc %1 %2 (normalize %3)) {}))
        nonterminals (set (keys prods))
        [terminals prods] (process-terminals 
                            nonterminals 
                            explicit-terminals
                            prods)]
    {:start :asterism/start
     :terminals terminals
     :nonterminals nonterminals
     :productions prods}))
