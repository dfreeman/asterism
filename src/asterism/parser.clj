(ns asterism.parser
  (:require [clojure.set :as set]
            [asterism :as ast]
            [asterism.util :as util]
            [asterism.scanner :as scanner]
            [slingshot.slingshot :refer [throw+]]))

;;;;;;;;;;;;;;; Models ;;;;;;;;;;;;;;;

(extend-protocol ast/INonterminal
  clojure.lang.Keyword
  (collapse? [this]
    (let [n (name this)]
      (and (.startsWith n "<")
           (.endsWith n ">")))))

(defrecord Terminal [id matcher opts]
  ast/ITerminal
  (id [this] id)
  (matcher [this] matcher)
  (elide? [this] (let [n (name id)]
                   (and (.startsWith n "<")
                        (.endsWith n ">"))))
  (classes [this] (into #{id} (:classes opts)))
  (dominates [this] (:dominates opts #{}))
  (submits-to [this] (:submits-to opts #{})))

;;;;;;;; Terminal Processing ;;;;;;;;;

(defn make-terminal [[id definition]]
  (if (map? definition)
    (if-let [matcher (:matcher definition)]
      (->Terminal id matcher (dissoc definition :matcher))
      (throw (Exception. (str "Invalid terminal definition: " definition))))
    (->Terminal id definition {})))

(defn- generate-id [matcher]
  (let [type (.toLowerCase (.getSimpleName (type matcher)))]
    (keyword (str type "-" matcher))))

(defn- matcher-key [literal]
  (if (util/regex? literal)
    {:pattern (str literal)}
    literal))

(defn- terminal-lookup [raw-terminals]
  (let [terms (util/set-map make-terminal raw-terminals)
        by-id (into {} (map #(vector (ast/id %) %) terms))
        by-matcher (->> terms
                     (map #(vector (matcher-key (ast/matcher %)) %))
                     (into {}))]
    (merge by-id by-matcher)))

(defn- process-terminals [nonterminals explicits prods]
  (let [initial (into explicits {:asterism/empty ""})
        terminals (atom (terminal-lookup initial))
        prods (util/map-for [[lhs prod] prods] lhs
                (util/set-for [alternative prod]
                  (util/vec-for [element alternative]
                    (if (contains? nonterminals element)
                      element
                      (let [matcher (matcher-key element)
                            id (generate-id element)]
                        (if (contains? @terminals matcher)
                          (ast/id (get @terminals matcher))
                          (do 
                            (swap! terminals assoc
                              matcher (make-terminal [id element]))
                            id)))))))
        terminals (->> @terminals
                    (map (fn [[matcher term]] [(ast/id term) term]))
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
    (util/fixed-point
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
  [ws rhs]
  (letfn [(append-all [lhs-vecs rhs-vecs]
            (set (for [lhs lhs-vecs
                       rhs rhs-vecs]
                    (vec (concat lhs rhs)))))

          (interpose-ws [v]
            (if (or (nil? ws)
                    (:no-ws (meta v)))
              v
              (interpose ws v)))

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

(defn valid-lookaheads [state]
  (->> state
    (util/set-map
      (fn [[_ rhs pos la]]
        (if (= pos (count rhs))
          la
          (nth rhs pos))))
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

(defn- elide-children [terminals children]
  (->> children
    (map
      (fn [child]
        (if-not (satisfies? ast/ITerminal child)
          child
          (let [type (ast/type child)
                term (get terminals type)]
            (if (and term (ast/elide? term))
              nil
              child)))))
    (filter identity)))

(defn- make-parser [grammar on-shift on-reduce on-fail]
  (let [firsts (generate-first-sets grammar)
        cc0 (cc0 firsts grammar)
        {:keys [action-table goto-table]} (build-tables cc0 firsts grammar)]
    (fn [input]
      (let [terminals (:terminals grammar)
            scanner (scanner/scanner input terminals)]
        (loop [pos 0
               stack (list [cc0 ::start])]
          (let [[state tree] (first stack)
                lookaheads (valid-lookaheads state)
                possible-tokens (scanner/scan scanner pos lookaheads)
                num-tokens (count possible-tokens)]
            (cond
              (= 0 num-tokens)
                (on-fail "Unable to parse x (expected {y, z})" state #{})
              (> num-tokens 1)
                (on-fail "Ambiguous input" state
                         (util/set-map second possible-tokens))
              :else
                (let [[pos' token] (first possible-tokens)
                      token-type (ast/type token)
                      table-value (get-in action-table [state token-type])
                      action (get-action table-value)]
                  (case action
                    :accept
                      (if (= token-type :asterism/eof)
                        (second (first stack))
                        (on-fail state token))

                    :reduce
                      (let [[_ lhs rhs] table-value
                            [popped remaining] (split-at (count rhs) stack)
                            state' (get-in goto-table [(ffirst remaining) lhs])
                            children (->> popped
                                          (map second)
                                          reverse
                                          flatten
                                          (elide-children terminals))
                            node (if (ast/collapse? lhs)
                                   children
                                   (on-reduce lhs children))]
                        (recur pos (conj remaining [state' node])))

                    :shift
                      (let [[_ next-state] table-value]
                        (recur pos' (conj stack [next-state (on-shift token)])))

                    (on-fail "Expected {x, y}, got z" state #{token}))))))))))

;;;;;;;;;;;;;;; Public ;;;;;;;;;;;;;;;

(defn parser
  "Creates a parser for the given productions, using the given options."
  [opts & prods]
  (let [[opts prods] (if (map? opts) [opts prods] [{} (cons opts prods)])
        {:keys [make-node make-leaf on-failure start whitespace terminals]
         :or {make-node (fn [lhs child-trees] {:type lhs :children child-trees})
              make-leaf (fn [token] token)
              on-failure (fn [type state] ::failure)
              start (first prods)
              whitespace #{#"\s+" :asterism/empty}
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
(defn simple-expression-parser []
  (parser {:whitespace nil}
    :expr #{[:expr #{"+" "-"} :term]
            :term}
    :term #{[:term #{"*" "/"} :factor]
            :factor}
    :factor #{["(" :expr ")"]
              #"\d+"
              #"[a-zA-Z]\w*"}))