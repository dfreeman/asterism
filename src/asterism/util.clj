(ns asterism.util)

(defn- omni-flatten [init pred? coll]
  (reduce
    (fn [acc item]
      (if (pred? item)
        (apply conj acc (omni-flatten init pred? item))
        (conj acc item)))
    init coll))

(defn- omni-filter [target pred? coll]
  (into target (filter pred? coll)))

(defn set-flatten [coll]
  (omni-flatten #{} set? coll))

(defn vec-flatten [coll]
  (omni-flatten [] vector? coll))

(defn set-filter [pred? coll]
  (omni-filter #{} pred? coll))

(defn vec-filter [pred? coll]
  (omni-filter [] pred? coll))

(defn map-filter [pred? coll]
  (omni-filter {} (fn [[k v]] (pred? k v)) coll))

(defn set-map [fun coll]
  (into #{} (map fun coll)))

(defn vec-map [fun coll]
  (into [] (map fun coll)))

(defn map-map [fun coll]
  (into {} (map (fn [[k v]] [k (fun k v)]) coll)))

(defmacro set-for [& body]
  `(into #{} (for ~@body)))

(defmacro vec-for [& body]
  `(into [] (for ~@body)))

(defmacro map-for [decls & body]
  `(into {} (for ~decls (vector ~@body))))

(defn regex? [x]
  (instance? java.util.regex.Pattern x))

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
