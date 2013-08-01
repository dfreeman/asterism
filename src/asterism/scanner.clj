(ns asterism.scanner
  (:require [asterism.util :as util]
            [clojure.set :as set]))

;;;;;;;;;;;;;; Matching ;;;;;;;;;;;;;;

(defprotocol Matchable
  (matches? [this input offset]
    "Tests whether this object matches the given input starting
    at the given offset. If it does not match, returns nil. If it
    does, returns a map containing at least the following keys:
      :consumed - the number of characters consumed from the input
      :lexeme - the value for the token to be produced
    Additional keys may be defined by some implementations. For
    instance, any internal groups in a Pattern will have their
    matches in a :groups vector."))

(extend-protocol Matchable
  nil
  (matches? [this _ _] nil)

  java.lang.String
  (matches? [this input offset]
    (when (.regionMatches this 0 input offset (.length this))
      {:consumed (.length this)
       :lexeme this}))

  java.util.regex.Pattern
  (matches? [this input offset]
    (let [matcher (.matcher this input)]
      (.region matcher offset (.length input))
      (when (.lookingAt matcher)
        {:consumed (- (.end matcher) offset)
         :lexeme (.group matcher)
         :groups (util/vec-for [i (range 1 (inc (.groupCount matcher)))]
                   (.group matcher i))})))

  clojure.lang.IFn
  (matches? [this input offset]
    (this input offset)))

;;;;;;;;; Token Construction ;;;;;;;;;

(defn make-token [id terminal offset match]
  (let [{:keys [lexeme consumed]} match
        metadata (dissoc match :lexeme :consumed)]
    (with-meta
      {:type id
       :lexeme lexeme}
      (assoc metadata
        :terminal terminal
        :start offset
        :length consumed))))

;;;;;;;;; Terminal Dominance ;;;;;;;;;

(defn dominates? [terms a-id b-id]
  (let [a-term (get terms a-id)
        b-term (get terms b-id)
        a-classes (into #{a-id} (:classes a-term))
        b-classes (into #{b-id} (:classes b-term))
        a-dominates (set (:dominates a-term))
        b-submits-to (set (:submits-to b-term))]
    (or (not (empty? (set/intersection a-classes b-submits-to)))
        (not (empty? (set/intersection b-classes a-dominates))))))

(defn- full-dominance-set [dom-map ids]
  (apply set/union
    ids
    (map #(get dom-map %) ids)))

(defn- dominance-map
  "Produces a map from terminal ids to the set of terminal
  ids by which that those terminals are directly or indirectly
  dominated"
  [terminals]
  (let [term-ids (keys terminals)
        ; Map each id to the set of ids that directly dominate it
        initial-doms 
          (->> term-ids
            (util/set-map 
              (fn [id]
                (->> term-ids
                  (util/set-filter #(dominates? terminals % id))
                  (vector id))))
            (into {}))]
    ; Keep rolling in indirect dominators until reaching a fixed point
    (util/fixed-point
      initial-doms
      (fn [doms]
        (util/map-map
          (fn [id dominators] (full-dominance-set doms dominators))
          doms)))))

;;;;;;;;; Processing Helpers ;;;;;;;;;

(defn- find-maximal [tokens]
  (let [consumed-groups (group-by #(:length (meta %)) tokens)
        [length group] (last (sort consumed-groups))]
    group))

;;;;;;;;;;;;;;; Public ;;;;;;;;;;;;;;;

(defn scanner [input terminals]
  {:input input
   :terminals terminals
   :dominance-map (dominance-map terminals)})

(defn scan [{:keys [input terminals dominance-map]} offset valid-lookahead]
  (if (>= offset (count input))
    ; If all input is consumed, EOF
    #{[offset {:type :asterism/eof}]}
    ; Otherwise, expand the search to include any dominating terminals...
    (let [valid-lookahead (full-dominance-set dominance-map valid-lookahead)
          matched-tokens
            (->> (for [id valid-lookahead]
                   ; attempt to match each one...
                   (let [terminal (get terminals id)
                         matcher (:matcher terminal)]
                     (when-let [match (matches? matcher input offset)]
                       (make-token id terminal offset match))))
                 (filter identity)
                 ; but only keep the ones that consumed the most.
                 find-maximal)
          matched-types (util/set-map :type matched-tokens)]
      (->> matched-tokens
        ; Filter out any tokens that were dominated by other matches
        (filter
          (fn [token]
            (let [dominators (get dominance-map (:type token))]
              (not-any? dominators matched-types))))
        ; Tag each with the new offset and return
        (util/set-map 
          (fn [token]
            [(+ offset (:length (meta token))) token]))))))