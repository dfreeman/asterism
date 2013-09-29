(ns asterism.parser.scanner
  (:require [asterism.util :as u]
            [clojure.set :as set]
            [slingshot.slingshot :refer [throw+]]))

;;;;;;;;;;;;;; Matching ;;;;;;;;;;;;;;

(defprotocol IMatcher
  "Matchers provide a means for the scanner to consume tokens in an input stream.
  Strings and Pattern instances are both matchers that behave in the intuitive way;
  functions as matchers are treated as their own implementation of `matches?`"
  (matches? [this input offset]
    "Tests whether this object matches the given input starting at the given
    offset. If it does not match, returns nil. If it does, returns a map with
    the following keys:
      :lexeme (required) the string that was consumed
      :consumed (optional) the number of characters of input consumed, if other than the
        length of the lexeme
      :info (optional) any additional source information that should be attached to the
        token produced (e.g. a Pattern's match groups)"))

(extend-protocol IMatcher
  nil
  (matches? [this _ _] nil)

  java.lang.String
  (matches? [this input offset]
    (when (.regionMatches this 0 input offset (.length this))
      {:lexeme this}))

  java.util.regex.Pattern
  (matches? [this input offset]
    (let [matcher (.matcher this input)]
      (.region matcher offset (.length input))
      (when (.lookingAt matcher)
        (let [groups (inc (.groupCount matcher))]
          {:lexeme (.group matcher)
           :info {:groups (mapv #(.group matcher %) (range 1 groups))}}))))

  clojure.lang.IFn
  (matches? [this input offset]
    (try
      (this input offset)
      (catch Exception e
        (throw+ {:type ::matcher-fail
                 :msg "Exception applying function as matcher"
                 :cause e})))))

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
            (map
              (fn [id]
                (->> term-ids
                  (filter #(dominates? terminals % id))
                  (set)
                  (vector id))))
            (into {}))]
    ; Keep rolling in indirect dominators until reaching a fixed point
    (u/fixed-point
      initial-doms
      (fn [doms]
        (reduce-kv #(assoc %1 %2 (full-dominance-set doms %3)) {} doms)))))

;;;;;;;;; Processing Helpers ;;;;;;;;;

(defn- find-maximal [tokens]
  (let [consumed-groups (group-by #(:length (:source-info %)) tokens)
        [length group] (last (sort consumed-groups))]
    group))

(defn- find-matches [terminals input offset valid-lookahead]
  (->> 
    (for [id valid-lookahead]
      (when-let [terminal (get terminals id)]
        (let [matcher (:matcher terminal)
              {:keys [lexeme consumed info]} (matches? matcher input offset)
              consumed (or consumed (count lexeme))
              source-info (merge {:start offset :length consumed} info)]
          (when (and lexeme
                (or (not= consumed 0) (= id :asterism/empty)))
              {:token-type id
               :lexeme lexeme
               :source-info source-info}))))
    (filter identity)))

;;;;;;;;;;;;;;; Public ;;;;;;;;;;;;;;;

(defn scanner [input whitespace terminals]
  {:input input
   :terminals terminals
   :whitespace whitespace
   :dominance-map (dominance-map terminals)})

(defn scan [{:keys [input whitespace terminals dominance-map]}
            offset valid-lookahead]
  (let [{:keys [lexeme]} (matches? whitespace input offset)
        offset (+ offset (count lexeme))]
    (if (>= offset (count input))
      ; If all input is consumed, EOF
      #{[offset {:token-type :asterism/eof :lexeme ""}]}
      ; Otherwise, expand the search to include any dominating terminals...
      (let [valid-lookahead (full-dominance-set dominance-map valid-lookahead)
            matched-tokens
              (->> valid-lookahead
                   ; attempt to match each one... 
                   (find-matches terminals input offset)
                   ; but only keep the ones that consumed the most.
                   find-maximal)
            matched-types (set (map :token-type matched-tokens))]
        (->> matched-tokens
          ; Filter out any tokens that were dominated by other matches
          (filter
            (fn [token]
              (let [dominators (get dominance-map (:token-type token))]
                (not-any? dominators matched-types))))
          ; Tag each with the new offset and return
          (map (fn [token] [(+ offset (:length (:source-info token))) token]))
          (set))))))