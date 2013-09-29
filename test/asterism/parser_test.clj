(ns asterism.parser-test
  (require [midje.sweet :refer [facts]]
           [asterism.parser :refer :all]))

(facts "on terminal elision middleware"
  (let [p (parser {:terminals {1 "one" 2 "two" 3 "three" 4 "four"}
                   :node-handler (->> default-node-handler
                                      (elide-terms even?))}
             :root [1 2 3 4])
        tree (p "one two three four")]
    (:type tree) => :root
    (map :lexeme (:children tree)) => ["one" "three"]))

(facts "on production collapsing middleware"
  (let [p (parser {:node-handler (->> default-node-handler
                                      (collapse-prods #{:x}))}
            :root [:x :y]
            :x [:y :y]
            :y #".")
        tree (p "abc")]
    (:type tree) => :root
    (map :type (:children tree)) => [:y :y :y]))

(facts "on combining middleware"
  (let [remove? #(let [n (name %)] (and (.startsWith n "<") (.endsWith n ">")))
        p (parser {:terminals {:ident #"[a-zA-Z]\w*"}
                   :node-handler (->> default-node-handler
                                      (collapse-prods remove?)
                                      (elide-terms remove?))}
            :list :<list>
            :<list> #{[:<list> :<item>] :<item>}
            :<item> :ident)
        tree (p "a bc d")]
    (:type tree) => :list
    (map :lexeme (:children tree)) => ["a" "bc" "d"]))