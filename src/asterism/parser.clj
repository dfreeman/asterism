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
  [{:keys [terminals nonterminals productions]}]
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

(defn- closure
  "Generates a closed state set from that set's core,
  iteratively including any items implied by those
  already in the set."
  [firsts grammar core]
  (let [updated-set (atom core)]
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

(defn cc0
  "Generates the initial set in the canonical collection
  for the given grammar."
  [firsts grammar]
  (let [start (:start grammar)
        start-prods (start (:productions grammar))]
    (->> start-prods
      (util/set-map (fn [rhs] [start rhs 0 :eof]))
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
                  (when-not (nt? nxt)
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
                   (= la :eof))
                (swap! action-table
                       assoc-in
                       [cc-i :eof]
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

(defn make-grammar [start ws prods]
  (let [prods (->> prods
                (apply hash-map)
                (util/map-map #(normalize ws %2)))]
    {:start start
     :terminals (extract-terminals (vals prods))
     :nonterminals (set (keys prods))
     :productions prods}))

(defn- make-parser [cc0 start action goto on-shift on-reduce on-fail]
  (fn [input]
    (loop [input input
           stack (list [cc0 start])]
      (let [[state tree] (first stack)
            word (first input)
            table-value (get-in action [state word])]
        (case (if (sequential? table-value) (first table-value) table-value)
          :accept
            (if (= word :eof)
              (let [trees (reverse (map second stack))
                    [[lhs] child-trees] (split-at 1 trees)]
                  (on-reduce lhs child-trees))
              (on-fail state word))

          :reduce
            (let [[_ lhs rhs] table-value
                  [popped remaining] (split-at (count rhs) stack)
                  next-state (get-in goto [(ffirst remaining) lhs])
                  node (on-reduce lhs (reverse (map second popped)))]
              (recur input (conj remaining [next-state node])))

          :shift
            (let [[_ next-state] table-value]
              (recur (rest input) (conj stack [next-state (on-shift word)])))

          (on-fail state word))))))


;;;;;;;;;;;;;;;; Public ;;;;;;;;;;;;;;

(defn parser
  "Creates a parser for the given productions, using the given options."
  [opts & prods]
  (let [[opts prods] (if (map? opts) [opts prods] [{} (cons opts prods)])
        ; Extract options
        {:keys [make-node make-leaf on-failure start ws]
         :or {make-node (fn [lhs child-trees] {:tag lhs :children child-trees})
              make-leaf identity
              on-failure (constantly ::failure)
              start (first prods)
              ws #"\s*"}} opts
        ; Normalize the grammar proper        
        grammar (make-grammar start ws prods)
        ; Calculate some metadata
        firsts (generate-first-sets grammar)
        cc0 (cc0 firsts grammar)
        ; Build action and goto tables
        {:keys [action goto]} (build-tables cc0 firsts grammar)]
    (make-parser cc0 start action goto make-leaf make-node on-failure)))

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
  (parser
    :expr #{[:expr #{"+" "-"} :term]
            :term}
    :term #{[:term #{"*" "/"} :factor]
            :factor}
    :factor #{["(" :expr ")"]
              #"\d+"
              #"\w+"}))