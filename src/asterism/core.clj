(ns asterism.core
  (:require [asterism.parser.protocols]
            [asterism.util :as util]))

(defn absolute-sym [sym & [ns]]
  (symbol (str (ns-name (or ns *ns*))) (name sym)))

(defmacro defterm [sym matcher & {:as config}]
  `(def ~sym ~(assoc config :matcher matcher)))

(defmacro defsort [sym & fns]
  `(let [sort-sym# (absolute-sym '~sym)]
    (defprotocol ~sym ~@fns)
    (defmacro ~(symbol (str "defent-" (clojure.string/lower-case sym)))
      ~(str "Defines an entity of the " sym " sort")
      [& body#]
      `(defent ~sort-sym# ~@body#))))


;; ^Expr* name => ^{::bind-sym name ::multi true} Expr
;; ^:xyz name => ^{::bind-sym name ::multi false} EThisThing:xyz

(defn process-bindings [entity-sym rhs prods]
  (let [entity-sym (absolute-sym entity-sym)
        locals (set (keys prods))]
    (letfn [(normalize [tag]
              (let [tag-ns (str (or (namespace tag) (ns-name *ns*)))
                    tag-name (str (name tag))
                    multi (.endsWith tag-name "*")
                    tag-name (if-not multi tag-name (subs tag-name 0 (dec (count tag-name))))])
              (if (symbol? tag)
                tag
                (symbol (str (namespace entity-sym))
                        (str (name entity-sym) tag))))

            (tagged [sym]
              (let [sym-meta (meta sym)
                    tag (or (:tag sym-meta)
                            (some #(get sym-meta %) locals))
                    
                    nt-str (str nt-sym)
                    multi (.endsWith nt-str "*")
                    nt-str (if-not multi nt-str (subs nt-str 0 (dec (count (nt-str)))))]
                (if-not tag
                  sym
                  (with-meta (private-nt tag) (bind-info tag sym)))))

            (process [rhs]
              (condp apply [rhs]
                vector? (util/vec-map process rhs)
                set? (util/set-map process rhs)
                symbol? (tagged rhs)
                rhs))]

      [(process rhs) (into {} (map (fn [[k v]] [(private-nt k) (process v)]) prods))])))

(defmacro defent
  "Defines an entity of the given sort"
  [sort-sym entity-sym rhs & impls]
  (let [[prods impls]
          (if (map? (first impls))
            (split-at 1 impls)
            [nil impls])
        [rhs prods] (process-bindings entity-sym rhs prods)]
    `(do
      (alter-meta! *ns* update-in [::entities '~sort-sym] clojure.set/union #{'~entity-sym})
      (defrecord ^{::rhs ~rhs ::internal-prods ~prods} ~entity-sym []
        ~sort-sym
        ~@impls))))

(defmacro deflang [features]
  )

(defsort Expr (typecheck [this]))
(ns asterism.other)
(asterism.core/defent-expr EFor []
  (typecheck [this] "hello"))
(println (asterism.core/typecheck (->EFor)))