(ns asterism.examples.json
  (:require [asterism.core :refer [defterm defsort deflang]]))

; This was a quick transliteration from json.org; safety not guaranteed
(defterm num-lit #"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")
(defterm str-lit #"\"(?:[^\"\\]|\\[\\\"/bfnrt]|\\u[0-9a-f]{4})*\"")          ;" dumb syntax highlighting

(defsort Expr
  (evaluate [this]))

(defent-expr ENum [^num-lit token]
  (evaluate [this] (read-string (:lexeme token))))

(defent-expr EString [^str-lit token]
  (evaluate [this] (read-string (:lexeme token))))

(defent-expr ETrue ["true"]
  (evaluate [this] true))

(defent-expr EFalse ["false"]
  (evaluate [this] false))

(defent-expr ENull ["null"]
  (evaluate [this] nil))

(defent-expr EList #{["[" "]"] ["[" :items "]"]}
  :items #{:item [:item "," :items]}
  :item ^Expr* elements

  (evaluate [this] (map evaluate elements)))

(defent-expr EDict #{["{" "}"] ["{" :pairs "}"]}
  :pairs #{:pair [:pair "," :pairs]}
  :pair [^str-lit* keys ":" ^Expr* values]

  (evaluate [this]
    (zipmap (map (comp keyword read-string :lexeme) keys)
            (map evaluate values))))

(deflang json
  :features [asterism.examples.json]
  :root Expr)

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