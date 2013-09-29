(ns asterism.parser.generator
  (:require [asterism.util :as u]
            [asterism.parser.scanner :as s]
            [clojure.set :as set]
            [slingshot.slingshot :refer [throw+]]))

;;;;;;;; Terminal Processing ;;;;;;;;;

(defn- term-id [terminal]
  (:id (meta terminal)))

(defn make-terminal [[id definition]]
  (let [invalid-term {:type ::invalid-terminal :id id :definition definition}]
    (cond 
      (map? definition)
        (if (satisfies? s/IMatcher (:matcher definition))
          (vary-meta definition assoc :id id)
          (throw+ (assoc invalid-term :msg "Invalid matcher")))
      (satisfies? s/IMatcher definition)
        (with-meta {:matcher definition} {:id id})
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
        by-id (into {} (map #(vector (term-id %) %) terms))
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
                          (let [result (term-id (get @terminals matcher))]
                            (if (instance? clojure.lang.IMeta result)
                              (with-meta result (meta element))
                              result))
                          (do 
                            (swap! terminals assoc
                              matcher (make-terminal [id element]))
                            id))))))))]))
        terminals (->> @terminals
                    (map (fn [[matcher term]] [(term-id term) term]))
                    (into {}))]
    [terminals prods]))

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

(defn- get-action [table-value]
  (if (sequential? table-value)
    (first table-value)
    table-value))

(defn build-tables
  "Builds action and goto tables for the given grammar"
  [cc0 firsts grammar]
  (let [cc (cc cc0 firsts grammar)
        action-table (atom {})
        goto-table (atom {})
        update-action-table!
          (fn [state la value]
            (let [existing (get-in @action-table [state la])]
              (cond
                (nil? existing)
                  (swap! action-table assoc-in [state la] value)
                (not= existing value)
                  (throw+ {:type ::table-conflict
                           :state state
                           :lookahead la
                           :values #{existing value}}))))]
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
                (update-action-table! cc-i :asterism/eof :accept)))
      (doseq [n (:nonterminals grammar)]
        (let [cc-j (goto cc-i n firsts grammar)]
          (when-not (empty? cc-j)
            (swap! goto-table assoc-in [cc-i n] cc-j)))))
    {:action-table @action-table
     :goto-table @goto-table}))

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

;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;

(defn valid-lookaheads [state]
  (->> state
    (map
      (fn [[_ rhs pos la]]
        (if (= pos (count rhs))
          la
          (nth rhs pos))))
    (set)
    u/set-flatten))

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

(defn make-parser [grammar whitespace on-shift on-reduce]
  (let [firsts (generate-first-sets grammar)
        cc0 (cc0 firsts grammar)
        {:keys [action-table goto-table]} (build-tables cc0 firsts grammar)]
    (fn [input]
      (let [terminals (:terminals grammar)
            scanner (s/scanner input whitespace terminals)]
        (loop [pos 0
               stack (list [cc0 ::start])]
          (let [[state tree] (first stack)
                lookaheads (valid-lookaheads state)
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
                      table-value (get-in action-table [state token-type])
                      action (get-action table-value)]
                  (case action
                    :accept
                      (if (= token-type :asterism/eof)
                        (second (first stack))
                        (throw+ {:type ::extra-input
                                 :parser-state state 
                                 :token token}))

                    :reduce
                      (let [[_ lhs rhs] table-value
                            [popped remaining] (split-at (count rhs) stack)
                            state' (get-in goto-table [(ffirst remaining) lhs])
                            children (->> popped
                                          (map second)
                                          reverse
                                          flatten)
                            node (on-reduce lhs rhs children)]
                        (recur pos (conj remaining [state' node])))

                    :shift
                      (let [[_ next-state] table-value]
                        (recur pos' (conj stack [next-state (on-shift token)])))

                    (throw+ {:type ::no-table-action
                             :state-table state
                             :token token}))))))))))
