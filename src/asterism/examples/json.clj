(ns asterism.examples.json
  (:require [asterism.core :refer [defterm defsort]]
            [asterism.parser.protocols :refer [lexeme]]))

; This was a quick transliteration from json.org; safety not guaranteed
(defterm num-lit #"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")
(defterm str-lit #"\"(?:[^\"\\]|\\[\\\"/bfnrt]|\\u[0-9a-f]{4})*\"")          ;" dumb syntax highlighting

(defsort Expr
  (evaluate [this]))

(defent-expr ENum [^num-lit token]
  (evaluate [this] (eval (lexeme token))))

(defent-expr EString [^str-lit token]
  (evaluate [this] (eval (lexeme token))))

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
    (zipmap (map (comp keyword eval lexeme) keys)
            (map evaluate values))))

(deflang json [*ns*]
  (fn [input] (evaluate input)))


(json "[1, 2, 3]") ;; => [1 2 3]
(json "{\"a\": [1, true], \"b\": null}") ;; => {:a [1 true] :b nil}


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