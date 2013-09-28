(ns asterism.parser.handler-test
  (require [midje.sweet :refer [facts]]
           [asterism.parser.generator :as gen]
           [asterism.parser.protocols :as p]
           [asterism.parser.handler :as handler]))

(facts "on terminal elision middleware"
  (let [p (gen/parser {:terminals {1 "one" 2 "two" 3 "three" 4 "four"}
                       :node-handler (->> handler/default-node-handler
                                          (handler/elide-terms even?))}
             :root [1 2 3 4])
        tree (p "one two three four")]
    (:type tree) => :root
    (map p/lexeme (:children tree)) => ["one" "three"]))

(facts "on production collapsing middleware"
  (let [p (gen/parser {:node-handler (->> handler/default-node-handler
                                          (handler/collapse-prods #{:x}))}
            :root [:x :y]
            :x [:y :y]
            :y #".")
        tree (p "abc")]
    (:type tree) => :root
    (map :type (:children tree)) => [:y :y :y]))

(facts "on combining middleware"
  (let [remove? #(let [n (name %)] (and (.startsWith n "<") (.endsWith n ">")))
        p (gen/parser {:terminals {:ident #"[a-zA-Z]\w*"}
                       :node-handler (->> handler/default-node-handler
                                          (handler/collapse-prods remove?)
                                          (handler/elide-terms remove?))}
            :list :<list>
            :<list> #{[:<list> :<item>] :<item>}
            :<item> :ident)
        tree (p "a bc d")]
    (:type tree) => :list
    (map p/lexeme (:children tree)) => ["a" "bc" "d"]))