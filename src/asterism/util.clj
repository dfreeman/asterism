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