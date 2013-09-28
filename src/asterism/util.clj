(ns asterism.util)

(defn- omni-flatten [init pred? coll]
  (reduce
    (fn [acc item]
      (if (pred? item)
        (apply conj acc (omni-flatten init pred? item))
        (conj acc item)))
    init coll))

(defn set-flatten [coll]
  (omni-flatten #{} set? coll))

(defn vec-flatten [coll]
  (omni-flatten [] vector? coll))

(defn fixed-point [arg fun]
  (loop [arg arg]
    (let [arg' (fun arg)]
      (if (= arg arg') arg (recur arg')))))

(defn print-tree
  ([tree] (print-tree tree "" false))
  ([tree indent skip-first-indent?]
    (let [type (or (:type tree) (:token-type tree))]
      (if skip-first-indent?
        (print type)
        (print (str indent type))))
    (if-let [lexeme (:lexeme tree)]
      (println (str " \"" lexeme "\""))
      (let [children (:children tree)]
        (if (= 1 (count children))
          (print-tree (first children) indent true)
          (do
            (println)
            (doseq [child children]
              (print-tree child (str indent "|   ") false))))))))
