(ns asterism.parser
  (:require [clojure.set :as set]
            [asterism.util :as util]
            [asterism.scanner :as scanner]))

;;;;;;;; Terminal Processing ;;;;;;;;;

(defn- canonical-id [literal]
  (let [type (.toLowerCase (.getSimpleName (type literal)))]
    (keyword (str type "-" literal))))

(defn- canonical-terminal [[id definition]]
  (if (map? definition)
    (if (:matcher definition)
      (assoc definition :id id)
      (throw (Exception. (str "Invalid terminal definition: " definition))))
    {:id id :matcher definition}))

(defn- matcher-for [literal]
  (if (util/regex? literal)
    {:pattern (str literal)}
    literal))

(defn- matcher-map [raw-terminals]
  (let [terms (util/set-map canonical-terminal raw-terminals)
        by-id (into {} (map #(vector (:id %) %) terms))
        by-matcher (into {} (map #(vector (matcher-for (:matcher %)) %) terms))]
    (merge by-id by-matcher)))

(defn- process-terminals [nonterminals explicits prods]
  (let [starting-terms (into explicits {:asterism/empty ""})
        terminals (atom (matcher-map starting-terms))
        prods (util/map-for [[lhs prod] prods] lhs
                (util/set-for [alternative prod]
                  (util/vec-for [element alternative]
                    (if (contains? nonterminals element)
                      element
                      (let [matcher (matcher-for element)
                            id (canonical-id element)]
                        (if (contains? @terminals matcher)
                          (:id (get @terminals matcher))
                          (do 
                            (swap! terminals assoc
                              matcher (canonical-terminal [id element]))
                            id)))))))
        terminals (->> @terminals
                    (map (fn [[matcher term]] [(:id term) (dissoc term :id)]))
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
    (loop [start-value @first-sets]
      (doseq [[lhs rhs-set] productions
              rhs rhs-set]
        (let [new-set (collapse-first @first-sets rhs)]
          (swap! first-sets update-in [lhs] #(set/union % new-set))))
      (if (= start-value @first-sets)
        start-value
        (recur @first-sets)))))

;;;;;;;;;;;; CC Generation ;;;;;;;;;;;

(defn closure
  "Generates a closed state set from that set's core,
  iteratively including any items implied by those
  already in the set."
  [firsts grammar core]
  (let [updated-set (atom core)]
    (loop [items core]
      (doseq [[lhs rhs pos la] items]
        (if-let [nxt (nth rhs pos nil)]
          (if (contains? (:nonterminals grammar) nxt)
            (let [rst (conj (vec (drop (inc pos) rhs)) la)
                  prods (get (:productions grammar) nxt)]
              (swap! updated-set set/union
                (set (for [rhs prods
                           new-la (collapse-first firsts rst)]
                       [nxt rhs 0 new-la])))))))
      (if (= items @updated-set)
        items
        (recur @updated-set)))))

(defn cc0
  "Generates the initial set in the canonical collection
  for the given grammar."
  [firsts grammar]
  (let [start (:start grammar)
        start-prods (start (:productions grammar))]
    (->> start-prods
      (util/set-map (fn [rhs] [start rhs 0 :asterism/eof]))
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
        goto-table (atom {})]
    (doseq [cc-i cc]
      (doseq [[lhs rhs pos la] cc-i]
        (cond (< pos (count rhs))
                (let [nxt (get rhs pos)]
                  (when-not (contains? (:nonterminals grammar) nxt)
                    (swap! action-table
                           assoc-in
                           [cc-i nxt]
                           [:shift (goto cc-i nxt firsts grammar)])))

              (and (= pos (count rhs))
                   (not= lhs (:start grammar)))
                (swap! action-table
                       assoc-in
                       [cc-i la]
                       [:reduce lhs rhs])

              (and (= pos (count rhs))
                   (= lhs (:start grammar))
                   (= la :asterism/eof))
                (swap! action-table
                       assoc-in
                       [cc-i :asterism/eof]
                       :accept)))
      (doseq [n (:nonterminals grammar)]
        (let [cc-j (goto cc-i n firsts grammar)]
          (when-not (empty? cc-j)
            (swap! goto-table assoc-in [cc-i n] cc-j)))))
    {:action @action-table
     :goto @goto-table}))

;;;;;; Production Normalization ;;;;;;

(defn normalize
  "Takes an arbitrary grammar RHS fragment and returns a single set
  of vectors, each containing only elements of (T u NT)"
  [ws rhs]
  (letfn [(append-all [lhs-vecs rhs-vecs]
            (set (for [lhs lhs-vecs
                       rhs rhs-vecs]
                    (vec (concat lhs rhs)))))

          (interpose-ws [v]
            (if (or (nil? ws)
                    (:no-ws (meta v)))
              v
              (interpose #{[ws]} v)))

          (normalize-vec [v]
            (->> v
              interpose-ws
              (util/vec-map #(normalize ws %))
              (reduce append-all #{[]})
              util/set-flatten))

          (normalize-set [s]
            (->> s
              (util/set-map #(normalize ws %))
              util/set-flatten))

          (normalize-node [x] #{[x]})]

      (condp apply [rhs]
        set? (normalize-set rhs)
        vector? (normalize-vec rhs)
        (normalize-node rhs))))

;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;

(defn valid-lookaheads [firsts state]
  (->> state
    (util/set-map
      (fn [[_ rhs pos la]]
        (if (= pos (count rhs))
          (get firsts la)
          (get firsts (nth rhs pos)))))
    util/set-flatten))

(defn make-grammar [start whitespace explicit-terminals prods]
  (let [prods (->> prods
                (concat [::start start])
                (apply hash-map)
                (util/map-map #(normalize whitespace %2)))
        nonterminals (set (keys prods))
        [terminals prods] (process-terminals 
                            nonterminals 
                            explicit-terminals
                            prods)]
    {:start ::start
     :terminals terminals
     :nonterminals nonterminals
     :productions prods}))

(defn- make-parser [grammar on-shift on-reduce on-fail]
  (let [firsts (generate-first-sets grammar)
        cc0 (cc0 firsts grammar)
        {:keys [action goto]} (build-tables cc0 firsts grammar)]
    (fn [input]
      (loop [input [input 0]
             stack (list [cc0 ::start])]
        (let [[state tree] (first stack)
              terminals (:terminals grammar)
              lookaheads (->> (valid-lookaheads firsts state)
                              (map (fn [id] [id (get terminals id)]))
                              (into {}))
              [input' token] (scanner/scan input lookaheads state)
              token-type (:type token)
              table-value (get-in action [state token-type])]
          (case (if (sequential? table-value) (first table-value) table-value)
            :accept
              (if (= token-type :asterism/eof)
                (second (first stack))
                (on-fail state token))

            :reduce
              (let [[_ lhs rhs] table-value
                    [popped remaining] (split-at (count rhs) stack)
                    next-state (get-in goto [(ffirst remaining) lhs])
                    node (on-reduce lhs (reverse (map second popped)))]
                (recur input (conj remaining [next-state node])))

            :shift
              (let [[_ next-state] table-value]
                (recur input' (conj stack [next-state (on-shift token)])))

            (on-fail state token)))))))

;;;;;;;;;;;;;;; Public ;;;;;;;;;;;;;;;

(defn parser
  "Creates a parser for the given productions, using the given options."
  [opts & prods]
  (let [[opts prods] (if (map? opts) [opts prods] [{} (cons opts prods)])
        {:keys [make-node make-leaf on-failure start whitespace terminals]
         :or {make-node (fn [lhs child-trees] {:type lhs :children child-trees})
              make-leaf identity
              on-failure (constantly ::failure)
              start (first prods)
              whitespace #"\s*"
              terminals {}}} opts
        grammar (make-grammar start whitespace terminals prods)]
    (make-parser grammar make-leaf make-node on-failure)))

(defn no-ws [& args]
  (with-meta
    (if (and (= (count args) 1)
             (vector? (first args)))
      (first args)
      (vec args))
    {:no-ws true}))

;;;;;;;;;;; Sample Usage ;;;;;;;;;;;;;

; Recognizes simple arithmetic expressions w/ standard OoO
(def simple-expression-parser
  (parser {:whitespace nil}
    :expr #{[:expr #{"+" "-"} :term]
            :term}
    :term #{[:term #{"*" "/"} :factor]
            :factor}
    :factor #{["(" :expr ")"]
              #"\d+"
              #"[a-zA-Z]\w*"}))