(ns asterism.scanner
  (:require [asterism.util :as util]))

;;;;;;;;;;;;;;;; Public ;;;;;;;;;;;;;;

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

(defn- make-token [id terminal offset match]
  (let [{:keys [lexeme consumed]} match
        metadata (dissoc match :lexeme :consumed)]
    {:type id
     :lexeme lexeme
     :meta (assoc metadata
                 :start offset
                 :length consumed)}))

(defn scan [[input offset] valid-lookahead state]
  (if (>= offset (count input))
    [[input offset] {:type :asterism/eof}]
    (let [matches (for [[id terminal] valid-lookahead]
                    (when-let [match (matches? (:matcher terminal) input offset)]
                      (make-token id terminal offset match)))
          matches (filter identity matches)]
      (case (count matches)
        0 (throw (Exception. "not enough!"))
        1 (let [match (first matches)
                length (get-in match [:meta :length])]
            [[input (+ offset length)] match])
        (throw (Exception. "too many!"))))))