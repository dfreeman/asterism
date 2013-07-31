(ns asterism.parser
  (:require [clojure.set :as set]
            [asterism.util :as util]))


;;;;;;;;;;;;;;;; Misc ;;;;;;;;;;;;;;;;

(defn- nt?
  "For now, nonterminals are just keywords."
  [x]
  (and (not= :eof x) (keyword? x)))

(defn- extract-terminals [rhs]
  (set (cond
         (or (sequential? rhs) (set? rhs))
           (mapcat extract-terminals rhs)
         (nt? rhs) nil
         :else [rhs])))

;;;;;;;;; FIRST(x) Generation ;;;;;;;;

(defn- collapse-first-set
  "Calculates the additional set of possible first terminals for a
  possible RHS sequence. For instance, given the production `Factor
  -> '(' Expr ')' | ident`, one call to calculate-first-set might
  have `items` as `['(' :expr ')']`"
  [first-sets rhs-nodes]
  (loop [rhs-nodes rhs-nodes
         current-set #{}]
    (let [node (first rhs-nodes)
          node-firsts (get first-sets node)]
      (if (or (not (contains? node-firsts ""))
              (empty? (rest rhs-nodes)))
        (set/union current-set node-firsts)
        (recur (rest rhs-nodes)
               (set/union
                 current-set
                 (set/difference node-firsts #{""})))))))

(defn generate-first-sets
  "Returns a map from each terminal and non-terminal in the given grammar
  to the set of possible leftmost terminals for each."
  [terminals nonterminals productions]
  (let [first-sets (atom (-> {"" #{""} :eof #{:eof}}
                             (into (map #(vector % #{%}) terminals))
                             (into (map #(vector % #{}) nonterminals))))]
    (loop [start-value @first-sets]
      (doseq [[lhs rhs-set] productions
              rhs rhs-set]
        (let [new-set (collapse-first-set @first-sets rhs)]
          (swap! first-sets update-in [lhs] #(set/union % new-set))))
      (if (= start-value @first-sets)
        start-value
        (recur @first-sets)))))

;;;;;;;;;;;; CC Generation ;;;;;;;;;;;

(defn cc0-core
  "Generates the core for state 0 of the recognizer.
  LR(1) items are represented as a vector of the form
  [lhs rhs position lookahead]"
  [grammar]
  (let [start (:start grammar)
        start-prods (start (:productions grammar))]
    (util/set-map (fn [rhs] [start rhs 0 :eof]) start-prods)))

(defn closure
  "Generates a closed state set from that set's core,
  iteratively including any items implied by those
  already in the set."
  [core grammar]
  (let [firsts (:firsts grammar)
        updated-set (atom core)]
    (loop [items core]
      (doseq [[lhs rhs pos la] items]
        (if-let [nxt (nth rhs pos nil)]
          (if (nt? nxt)
            (let [rst (conj (vec (drop (inc pos) rhs)) la)
                  prods (get (:productions grammar) nxt)]
              (swap! updated-set set/union
                (set (for [rhs prods
                           new-la (collapse-first-set firsts rst)]
                       [nxt rhs 0 new-la])))))))
      (if (= items @updated-set)
        items
        (recur @updated-set)))))

; LR(1) item: [lhs rhs position lookahead]
(defn goto
  "Generates all possible LR(1) items that could result
  from recognizing `x` from the given state"
  [state x grammar]
  (closure
    (reduce
      (fn [acc [lhs rhs pos la]]
        (if (= (nth rhs pos nil) x)
          (conj acc [lhs rhs (inc pos) la])
          acc))
      #{} state)
    grammar))

(defn cc [grammar]
  (let [cc0 (closure (cc0-core grammar) grammar)]
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
                      next-state (goto state node grammar)]
                  (when-not (or (= #{nil} next-state)
                                (contains? cc next-state)
                                (contains? to-check next-state))
                    next-state))))
            (remove nil?)
            set))))))

;;;;;;; LR(1) Table Generation ;;;;;;;

(defn build-tables
  "Builds action and goto tables for the given grammar"
  [grammar]
  (let [cc (cc grammar)
        action-table (atom {})
        goto-table (atom {})]
    (doseq [cc-i cc]
      (doseq [[lhs rhs pos la] cc-i]
        (cond (< pos (count rhs))
                (let [nxt (get rhs pos)]
                  (when-not (nt? nxt)
                    (swap! action-table
                           assoc-in
                           [cc-i nxt]
                           [:shift (goto cc-i nxt grammar)])))

              (and (= pos (count rhs))
                   (not= lhs (:start grammar)))
                (swap! action-table
                       assoc-in
                       [cc-i la]
                       [:reduce lhs rhs])

              (and (= pos (count rhs))
                   (= lhs (:start grammar))
                   (= la :eof))
                (swap! action-table
                       assoc-in
                       [cc-i :eof]
                       :accept)))
      (doseq [n (:nonterminals grammar)]
        (let [cc-j (goto cc-i n grammar)]
          (when-not (empty? cc-j)
            (swap! goto-table assoc-in [cc-i n] cc-j)))))
    {:action @action-table
     :goto @goto-table}))

;;;;;;;;;;;;;; Parsing ;;;;;;;;;;;;;;;

(defn make-node [tag children-vec]
  (println "Identified a" tag)
  (clojure.pprint/pprint children-vec)
  (println)
  {:tag tag :children children-vec})

(defn make-leaf [token]
  (println "Read a" token)
  token)

(defn make-parser [grammar on-shift on-reduce]
  (let [{:keys [action goto]} (build-tables grammar)]
    (fn [input]
      (loop [input input
             stack (list (closure (cc0-core grammar) grammar))]
        (let [state (first stack)
              word (first input)
              table-value (get-in action [state word])]
          (cond
            (= table-value :accept)
              (second stack)
            (= (first table-value) :reduce)
              (let [[_ lhs rhs] table-value
                    [popped remaining] (split-at (* 2 (count rhs)) stack)
                    next-state (get-in goto [(first remaining) lhs])
                    node (make-node lhs (reverse (take-nth 2 (rest popped))))]
                (recur input (into remaining [node next-state])))
            (= (first table-value) :shift)
              (let [[_ next-state] table-value]
                (recur (rest input) (into stack [(make-leaf word) next-state])))
            :else
              :failure))))))

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

;;;;;;;;;;;;;;;; Public ;;;;;;;;;;;;;;

(defn grammar
  "TODO docstring"
  [opts & prods]
  (let [[opts prods] (if (map? opts)
                       [opts prods]
                       [{} (cons opts prods)])
        start (:start opts (first prods))
        ws (:ws opts #"\s*")
        prods (apply hash-map prods)
        prods (util/map-map #(normalize ws %2) prods)
        ts (extract-terminals (vals prods))
        nts (set (keys prods))]
    {:terminals ts
     :nonterminals nts
     :start start
     :productions prods
     :ws ws
     :firsts (generate-first-sets ts nts prods)}))

(defn no-ws [& args]
  (with-meta
    (if (and (= (count args) 1)
             (vector? (first args)))
      (first args)
      (vec args))
    {:no-ws true}))

;;;;;;;;;;; Sample Usage ;;;;;;;;;;;;;

; Recognizes simple arithmetic expressions w/ standard OoO
(def simple-expression-grammar
  (grammar
    :expr #{[:expr #{"+" "-"} :term]
            :term}
    :term #{[:term #{"*" "/"} :factor]
            :factor}
    :factor #{["(" :expr ")"]
              #"\d+"
              #"\w+"}))