(ns asterism.parser.scanner
  (:require [asterism.parser.protocols :as parser]
            [asterism.util :as util]
            [clojure.set :as set]))

;;;;;;;;;;;;;;; Models ;;;;;;;;;;;;;;;

(defrecord Token [type lexeme source-info]
  parser/IToken
  (token-type [this] type)
  (lexeme [this] lexeme)
  (source-info [this] source-info))

;;;;;;;;;;;;;; Matching ;;;;;;;;;;;;;;

(extend-protocol parser/IMatcher
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

(defn make-token [type offset match]
  (let [{:keys [lexeme consumed]} match
        source-info (dissoc match :lexeme :consumed)]
    (->Token type lexeme
      (assoc source-info
        :start offset
        :length consumed))))

;;;;;;;;; Terminal Dominance ;;;;;;;;;

(defn dominates? [terms a-id b-id]
  (let [a-term (get terms a-id)
        b-term (get terms b-id)
        a-classes (into #{a-id} (parser/classes a-term))
        b-classes (into #{b-id} (parser/classes b-term))
        a-dominates (set (parser/dominates a-term))
        b-submits-to (set (parser/submits-to b-term))]
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
  (let [consumed-groups (group-by #(:length (parser/source-info %)) tokens)
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
    #{[offset (make-token :asterism/eof offset {})]}
    ; Otherwise, expand the search to include any dominating terminals...
    (let [valid-lookahead (full-dominance-set dominance-map valid-lookahead)
          matched-tokens
            (->> (for [id valid-lookahead]
                   ; attempt to match each one...
                   (when-let [terminal (get terminals id)]
                     (let [matcher (parser/matcher terminal)]
                       (when-let [match (parser/matches? matcher input offset)]
                         (when [(and (= (:consumed match) 0)
                                     (not= id :asterism/empty))]
                           (make-token id offset match))))))
                 (filter identity)
                 ; but only keep the ones that consumed the most.
                 find-maximal)
          matched-types (util/set-map parser/token-type matched-tokens)]
      (->> matched-tokens
        ; Filter out any tokens that were dominated by other matches
        (filter
          (fn [token]
            (let [dominators (get dominance-map (parser/token-type token))]
              (not-any? dominators matched-types))))
        ; Tag each with the new offset and return
        (util/set-map 
          (fn [token]
            [(+ offset (:length (parser/source-info token))) token]))))))