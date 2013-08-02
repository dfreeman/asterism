(ns asterism.parser.scanner
  (:require [asterism.parser.protocols :as parser]
            [asterism.util :as util]
            [clojure.set :as set]))

;;;;;;;;;;;;;; Matching ;;;;;;;;;;;;;;

(defn- make-token [type lexeme source-info & {:as supplements}]
    {:token-type type
     :lexeme lexeme
     :source-info (into source-info supplements)})

(extend-protocol parser/IToken
  clojure.lang.IPersistentMap
  (token-type [this] (:token-type this))
  (lexeme [this] (:lexeme this))
  (source-info [this] (:source-info this)))

(extend-protocol parser/IMatcher
  nil
  (matches? [this _ _ _] nil)

  java.lang.String
  (matches? [this input offset type source-info]
    (when (.regionMatches this 0 input offset (.length this))
      [(.length this) (make-token type this source-info
                        :length (.length this))]))

  java.util.regex.Pattern
  (matches? [this input offset type source-info]
    (let [matcher (.matcher this input)]
      (.region matcher offset (.length input))
      (when (.lookingAt matcher)
        (let [consumed (- (.end matcher) offset)
              groups (inc (.groupCount matcher))]
          [consumed (make-token type (.group matcher) source-info
                      :length consumed
                      :groups (util/vec-for [i (range 1 groups)]
                                (.group matcher i)))]))))

  clojure.lang.IFn
  (matches? [this input offset type source-info]
    (this input offset type source-info)))

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

(defn- find-matches [terminals input offset valid-lookahead]
  (->> 
    (for [id valid-lookahead]
      (when-let [terminal (get terminals id)]
        (let [matcher (parser/matcher terminal)
              source-info {:start offset}]
          (when-let [[consumed token]
                       (parser/matches? matcher input offset id source-info)]
            (when (or (not= consumed 0) (= id :asterism/empty))
              token)))))
    (filter identity)))

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
            (->> valid-lookahead
                 ; attempt to match each one... 
                 (find-matches terminals input offset)
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