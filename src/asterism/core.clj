(ns asterism.core
  (:require [asterism.parser.protocols :as protocols]
            [asterism.parser.generator :as gen]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [slingshot.slingshot :refer [throw+]]))

(defprotocol Entity)

(defn canonicalize [s & [ns]]
  (let [ns (or ns (:ns (meta (resolve s))))]
    (symbol (str (ns-name ns))
            (str (name s)))))

(defmacro defterm [sym matcher & {:as config}]
  (let [term (assoc config :matcher matcher)]
    `(do
      (alter-meta! *ns* assoc-in [::terms '~(canonicalize sym *ns*)] ~term)
      (def ~sym ~term))))

(defmacro defsort [sym & fns]
  `(let [sort-sym# (canonicalize '~sym *ns*)]
    (defprotocol ~sym ~@fns)
    (defmacro ~(symbol (str "defent-" (string/lower-case sym)))
      ~(str "Defines an entity of the " sym " sort")
      [& body#]
      `(defent ~sort-sym# ~@body#))))


; ^Expr* name => ^{::bind name ::multi true} 'Expr
; ^Typ name => ^{::bind name ::multi false} 'Typ

(defn process-bindings [entity-name rhs local-prods]
  (let [syms (atom #{})]
    (letfn [(namespace-local [local]
              (symbol (str (ns-name *ns*)) (str entity-name ":" (name local))))

            (process-binding [sym]
              (if-let [tag (:tag (meta sym))]
                (let [tag-name (name tag)
                      multi (.endsWith tag-name "*")
                      len (count tag-name)
                      tag-name (if multi (.substring tag-name 0 (dec len)) tag-name)
                      sym (vary-meta sym dissoc :tag)]
                  (swap! syms conj sym)
                  (with-meta
                    (canonicalize (symbol tag-name))
                    {::bind sym ::multi multi}))
                (canonicalize sym)))
            
            (transform [rhs]
              (condp #(%1 %2) rhs
                vector? (mapv transform rhs)
                set? (set (map transform rhs))
                keyword? (namespace-local rhs)
                symbol? (process-binding rhs)
                rhs))]
      (vector
        (transform rhs)
        (reduce-kv #(assoc %1 (namespace-local %2) (transform %3)) {} local-prods)
        @syms))))


(defn extract-inline-map
  ([forms] (extract-inline-map forms {}))
  ([[k v & forms :as impls] prods]
    (if (keyword? k)
      (recur forms (assoc prods k v))
      [prods impls])))

(defmacro defent
  "Defines an entity of the given sort"
  [sort-sym entity-sym rhs & impls]
  (let [[inner-prods impls] (extract-inline-map impls)
        [rhs inner-prods syms] (process-bindings entity-sym rhs inner-prods)
        canonical-sort (canonicalize sort-sym)
        entity-info {:rhs rhs
                     :inner-prods inner-prods
                     :sort canonical-sort}]
    `(do
      (alter-meta! *ns* assoc-in [::entities '~(canonicalize entity-sym *ns*)] '~entity-info)
      (defrecord ~entity-sym ~(vec syms)
        Entity
        ~sort-sym
        ~@impls))))


(defn resolve-feature [s]
  (try (require s) (catch Throwable e))
  (meta (find-ns s)))

(defn find-factory [entity-sym]
  (let [factory-name (symbol (str (namespace entity-sym))
                             (str "map->" (name entity-sym)))]
    (when-let [factory-var (resolve factory-name)]
      (var-get factory-var))))

(defn sym->key [sym]
  (let [ns (str (namespace sym))
        name (str (name sym))]
    (if (empty? ns)
      (keyword name)
      (keyword ns name))))

(defn merge-bindings [rhs children]
  (let [ambiguous {:type ::ambiguous-binding
                   :msg "Multiple entities matched a single-assign binding"}
        merge-partial (partial merge-with
                               #(if (every? seq? %&)
                                  (apply concat %&)
                                  (throw+ (assoc ambiguous :values %&))))]
    (reduce-kv
      (fn [bindings nt result]
        (if-let [sym (::bind (meta nt))]
          (let [multi? (::multi (meta nt))
                key (sym->key sym)]
            (if multi?
              (update-in bindings [key] concat [result])
              (if (contains? bindings key)
                (throw+ (assoc ambiguous :symbol sym :values [(get bindings key) result]))
                (assoc bindings key result))))
          (if-let [subbindings (and (map? result) (::bindings result))]
            (merge-partial subbindings bindings)
            bindings)))
      {}
      (zipmap rhs children))))

(defmacro deflang [sym & {:keys [features root]}]
  (let [features (map resolve-feature features)
        terms (apply merge (map ::terms features))
        entities (apply merge (map ::entities features))
        entity-prods (reduce-kv #(assoc %1 %2 (:rhs %3)) {} entities)
        sort-prods (reduce-kv #(update-in %1 [(:sort %3)] set/union #{(with-meta %2 {::bind 'asterism.core/bind-through})}) {} entities)
        inner-prods (map :inner-prods (vals entities))
        prod-seq (apply concat (apply merge entity-prods sort-prods inner-prods))]
    `(def ~sym
      ~(apply gen/parser
        {:node-handler (fn [lhs rhs children]
                         (let [bindings (merge-bindings rhs children)]
                           (if-let [result (::bind-through bindings)]
                             result
                             (if-let [factory (find-factory lhs)]
                               (factory bindings)
                               {::bindings bindings}))))
         :start (canonicalize root)
         :terminals terms}
        prod-seq))))