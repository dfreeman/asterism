(ns asterism.parser.generator
  "A canonical LR(1) parser generator with a couple of tweaks. Namely, it is built to work with
  a context-aware scanner, and has a basic notion of operator associativity and precedence."
  (:require [asterism.util :as u]
            [asterism.parser.scanner :as s]
            [clojure.set :as set]
            [clojure.tools.logging :as log]
            [slingshot.slingshot :refer [throw+]]))

;;;;;;;;; FIRST(x) Generation ;;;;;;;;

(defn collapse-first
  "Given a set of known firsts, collapses down the given sequence
  to determine the valid set of expectable next token types"
  [firsts sequence]
  (loop [sequence sequence
         current-set #{}]
    (let [node (first sequence)
          node-firsts (get firsts node)]
      (if (or (not (contains? node-firsts :asterism/empty))
              (empty? (rest sequence)))
        (set/union current-set node-firsts)
        (recur (rest sequence)
               (set/union
                 current-set
                 (set/difference node-firsts #{:asterism/empty})))))))

(defn generate-first-sets
  "Returns a map from each terminal and non-terminal in the given grammar
  to the set of possible leftmost terminals for each."
  [{:keys [terminals nonterminals productions]}]
  (let [first-sets (atom (-> {:asterism/empty #{:asterism/empty}
                              :asterism/eof #{:asterism/eof}}
                             (into (map #(vector % #{%}) (keys terminals)))
                             (into (map #(vector % #{}) nonterminals))))]
    (u/fixed-point
      @first-sets
      (fn [start-value]
        (doseq [[lhs rhs-set] productions
                rhs rhs-set]
          (let [new-set (collapse-first @first-sets rhs)]
            (swap! first-sets update-in [lhs] #(set/union % new-set))))
        @first-sets))))

;;;;;;;;;;;; CC Generation ;;;;;;;;;;;

(defn closure
  "Generates a closed state set from that set's core,
  iteratively including any items implied by those
  already in the set."
  [firsts grammar core]
  (let [{:keys [productions nonterminals]} grammar]
    (loop [cc #{}
           to-check core]
      (let [cc' (into cc to-check)
            to-check' 
              (apply set/union
                (pmap 
                  (fn [[lhs rhs pos la]]
                    (when-let [nxt (nth rhs pos nil)]
                      (if (contains? nonterminals nxt)
                        (let [rst (concat (drop (inc pos) rhs) [la])
                              prods (get productions nxt)]
                          (set
                            (for [rhs prods
                                  new-la (collapse-first firsts rst)]
                              [nxt rhs 0 new-la]))))))
                  to-check))]
        (if (empty? (set/difference to-check' cc'))
          cc'
          (recur cc' to-check'))))))

(defn cc0
  "Generates the initial set in the canonical collection
  for the given grammar."
  [firsts grammar]
  (let [start (:start grammar)
        start-prods (start (:productions grammar))]
    (->> start-prods
      (map (fn [rhs] [start rhs 0 :asterism/eof]))
      (set)
      (closure firsts grammar))))

; LR(1) item: [lhs rhs position lookahead]
(defn goto
  "Generates all possible LR(1) items that could result
  from recognizing `x` from the given state"
  [state x firsts grammar]
  (closure
    firsts
    grammar
    (reduce
      (fn [acc [lhs rhs pos la]]
        (if (= (nth rhs pos nil) x)
          (conj acc [lhs rhs (inc pos) la])
          acc))
      #{} state)))

(defn cc [cc0 firsts grammar]
  (loop [cc #{}
         to-check [cc0]]
    (if (empty? to-check)
      cc
      (recur
        (set/union cc to-check)
        (->>
          (for [state to-check
                [lhs rhs pos la] state]
            (if (< pos (count rhs))
              (let [node (nth rhs pos)
                    next-state (goto state node firsts grammar)]
                (when-not (or (= #{nil} next-state)
                              (contains? cc next-state)
                              (contains? to-check next-state))
                  next-state))))
          (remove nil?)
          set)))))

;;;;;;; LR(1) Table Generation ;;;;;;;

(defn build-tables
  "Builds action and goto tables for the given grammar"
  [cc0 firsts grammar]
  (let [cc (cc cc0 firsts grammar)
        action-table (atom {})
        goto-table (atom {})
        terminals (:terminals grammar)
        update-action-table!
          (fn [state la value]
            (let [existing (get-in @action-table [state la])
                  la-term (get terminals la)]
              (cond
                (or (nil? existing) (:operator la-term))
                  (swap! action-table update-in [state la] set/union #{value})
                (not= existing #{value})
                  (throw+ {:type ::table-conflict
                           :state state
                           :lookahead la
                           :values (set/union existing #{value})}))))]
    (doseq [cc-i cc]
      (doseq [[lhs rhs pos la] cc-i]
        (cond ; shift
              (< pos (count rhs))
                (let [nxt (get rhs pos)]
                  (when-not (contains? (:nonterminals grammar) nxt)
                    (update-action-table! cc-i nxt
                      [:shift (goto cc-i nxt firsts grammar)])))
              ; reduce
              (and (= pos (count rhs))
                   (not= lhs (:start grammar)))
                (update-action-table! cc-i la [:reduce lhs rhs])
              ; accept
              (and (= pos (count rhs))
                   (= lhs (:start grammar))
                   (= la :asterism/eof))
                (update-action-table! cc-i :asterism/eof [:accept])))
      (doseq [n (:nonterminals grammar)]
        (let [cc-j (goto cc-i n firsts grammar)]
          (when-not (empty? cc-j)
            (swap! goto-table assoc-in [cc-i n] cc-j)))))
    {:action-table @action-table
     :goto-table @goto-table}))

(defn resolve-action
  "Resolves which table action should be followed when two are available"
  [actions prev-token prev-op next-token next-op]
  (if (= (count actions) 1)
    (first actions)
    (let [shifts (filter #(= (first %) :shift) actions)
          reduces (filter #(= (first %) :reduce) actions)]
      (cond
        (not= (count shifts) (count reduces) 1)
          (throw+ {:type ::shift-reduce-ambiguity
                   :msg "Pretty sure this can only resolve shift-reduce conflicts"
                   :actions actions})
        (or (nil? prev-op) (nil? next-op))
          (throw+ {:type ::ambiguous-parse
                   :msg "You should really fix that"})
        :else
          (let [[shift-action] shifts
                [reduce-action] reduces
                lprec (s/precedence prev-op prev-token)
                rprec (s/precedence next-op next-token)]
            (cond
              (< lprec rprec) reduce-action
              (> lprec rprec) shift-action
              :else (case (s/associativity next-op next-token)
                      :right shift-action
                      :left reduce-action
                      (throw+ {:type ::nonassociative-operator
                               :msg "Found a sequence of nonassociative operators"
                               :ops [prev-op next-op]}))))))))

;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;

(defn valid-lookaheads [terminals state]
  (->> state
    (map
      (fn [[_ rhs pos la]]
        (if (= pos (count rhs))
          la
          (nth rhs pos))))
    (filter terminals)
    set
    u/set-flatten))

; stack elements are [state payload token?]
(defn make-parser [grammar whitespace on-shift on-reduce]
  (let [firsts (generate-first-sets grammar)
        cc0 (cc0 firsts grammar)
        {:keys [action-table goto-table]} (build-tables cc0 firsts grammar)]
    (fn [input]
      (let [terminals (:terminals grammar)
            scanner (s/scanner input whitespace terminals)]
        (loop [pos 0
               stack (list {:state cc0 :payload ::start})]
          (let [state (:state (first stack))
                lookaheads (valid-lookaheads terminals state)
                possible-tokens (s/scan scanner pos lookaheads)
                num-tokens (count possible-tokens)]
            (cond
              (zero? num-tokens)
                (throw+ {:type ::no-matching-token :parse-state state})
              (> num-tokens 1)
                (throw+ {:type ::multiple-matching-tokens
                         :parse-state state
                         :tokens (set (map second possible-tokens))})
              :else
                (let [[pos' token] (first possible-tokens)
                      token-type (:token-type token)
                      next-op (:operator (get terminals token-type))
                      prev-token (:payload (second stack))
                      prev-op (:operator (:term (second stack)))
                      table-values (get-in action-table [state token-type])
                      table-value (resolve-action table-values prev-token prev-op token next-op)
                      action (first table-value)]
                  (case action
                    :accept
                      (if (= token-type :asterism/eof)
                        (:payload (first stack))
                        (throw+ {:type ::extra-input
                                 :parser-state state 
                                 :token token}))

                    :reduce
                      (let [[_ lhs rhs] table-value
                            [popped remaining] (split-at (count rhs) stack)
                            state' (get-in goto-table [(:state (first remaining)) lhs])
                            children (->> popped
                                          (map :payload)
                                          reverse
                                          flatten)
                            node (on-reduce lhs rhs children)]
                        (recur pos (conj remaining {:state state' :payload node})))

                    :shift
                      (let [[_ next-state] table-value]
                        (recur pos' (conj stack {:state next-state
                                                 :payload (on-shift token)
                                                 :term (get terminals (:token-type token))})))

                    (throw+ {:type ::no-table-action
                             :state-table state
                             :token token}))))))))))
