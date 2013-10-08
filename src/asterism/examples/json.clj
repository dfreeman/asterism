(ns asterism.examples.json
  (:require [asterism.core :refer [defsort defentity deflang]]
            [asterism.examples.core :as core]))

(defsort Expression
  (evaluate [this] "Evaluates this expression"))

(defentity ENum [^core/num-lit token]
  Expression
  (evaluate [this] (read-string (:lexeme token))))

(defentity EString [^core/str-lit token]
  Expression
  (evaluate [this] (read-string (:lexeme token))))

(defentity ETrue ["true"]
  Expression
  (evaluate [this] true))

(defentity EFalse ["false"]
  Expression
  (evaluate [this] false))

(defentity ENull ["null"]
  Expression
  (evaluate [this] nil))

(defentity EList #{["[" "]"] ["[" :items "]"]}
  :items #{:item [:item "," :items]}
  :item ^Expression* elements

  Expression
  (evaluate [this] (map evaluate elements)))

(defentity EDict #{["{" "}"] ["{" :pairs "}"]}
  :pairs #{:pair [:pair "," :pairs]}
  :pair [^core/str-lit* keys ":" ^Expression* values]

  Expression
  (evaluate [this]
    (zipmap (map (comp keyword read-string :lexeme) keys)
            (map evaluate values))))

(deflang json
  :features [asterism.examples.core
             asterism.examples.json]
  :root Expression)

(defn parse-json [s]
  (evaluate (json s)))


(parse-json "[1, 2, 3]") ;; => [1 2 3]
(parse-json "{\"a\": [1, true, \"free\"], \"b\": null}") ;; => {:a [1 true "free"] :b nil}


;;;;;;;;;;;;;;;; Straight-up parser implementation ;;;;;;;;;;;;

#_
(def json (parser
  {:terminals {:null "null"
               :true "true"
               :false "false"
               :string #"\"(?:[^\"\\]|\\[\\\"/bfnrt]|\\u[0-9a-f]{4})*\""     ;"
               :num #"-?(?:0|[1-9]\d*)(?:.\d+)?(?:[eE][+-]?\d+)?"

               :<lbrace> "{" :<rbrace> "}"
               :<lbrack> "[" :<rbrack> "]"
               :<comma>  "," :<colon>  ":"}}

  :expr #{:num :string :true :false :null :list :dict}

  :dict #{[:<lbrace> :<rbrace>] [:<lbrace> :<pairs> :<rbrace>]}
  :<pairs> #{[:pair :<comma> :<pairs>] :pair}
  :pair [:string :<colon> :expr]

  :list #{[:<lbrack> :<rbrack>] [:<lbrack> :<items> :<rbrack>]}
  :<items> #{[:expr :<comma> :<items>] :expr}))