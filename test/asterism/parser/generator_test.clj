(ns asterism.parser.generator-test
  (:require [midje.sweet :refer [facts contains exactly throws]]
            [asterism.parser.protocols :as parser]
            [asterism.parser.generator :refer :all]))

; Recognizes strings of the form (^n )^n +
(def paren-grammar
  (make-grammar :list nil
    {:lparen "("
     :rparen ")"}
    [:list #{[:list :pair] :pair}
     :pair #{[:lparen :pair :rparen] [:lparen :rparen]}]))

; Firsts sets
(def paren-firsts
  {:asterism/start #{:lparen}
   :pair #{:lparen}
   :list #{:lparen}
   :lparen #{:lparen}
   :rparen #{:rparen}
   :asterism/empty #{:asterism/empty}
   :asterism/eof #{:asterism/eof}})

; CC for the paren-grammar
(def paren-cc [
  ; cc0
  #{[:asterism/start [:list] 0 :asterism/eof]
    [:list [:list :pair] 0 :lparen]
    [:list [:list :pair] 0 :asterism/eof]
    [:list [:pair] 0 :lparen]
    [:list [:pair] 0 :asterism/eof]
    [:pair [:lparen :pair :rparen] 0 :lparen]
    [:pair [:lparen :pair :rparen] 0 :asterism/eof]
    [:pair [:lparen :rparen] 0 :lparen]
    [:pair [:lparen :rparen] 0 :asterism/eof]}
  ; cc1
  #{[:asterism/start [:list] 1 :asterism/eof]
    [:list [:list :pair] 1 :asterism/eof]
    [:list [:list :pair] 1 :lparen]
    [:pair [:lparen :pair :rparen] 0 :asterism/eof]
    [:pair [:lparen :pair :rparen] 0 :lparen]
    [:pair [:lparen :rparen] 0 :asterism/eof]
    [:pair [:lparen :rparen] 0 :lparen]}
  ; cc2
  #{[:list [:pair] 1 :asterism/eof]
    [:list [:pair] 1 :lparen]}
  ; cc3
  #{[:pair [:lparen :pair :rparen] 0 :rparen]
    [:pair [:lparen :pair :rparen] 1 :asterism/eof]
    [:pair [:lparen :pair :rparen] 1 :lparen]
    [:pair [:lparen :rparen] 0 :rparen]
    [:pair [:lparen :rparen] 1 :asterism/eof]
    [:pair [:lparen :rparen] 1 :lparen]}
  ; cc4
  #{[:list [:list :pair] 2 :asterism/eof]
    [:list [:list :pair] 2 :lparen]}
  ; cc5
  #{[:pair [:lparen :pair :rparen] 2 :asterism/eof]
    [:pair [:lparen :pair :rparen] 2 :lparen]}
  ; cc6
  #{[:pair [:lparen :pair :rparen] 0 :rparen]
    [:pair [:lparen :pair :rparen] 1 :rparen]
    [:pair [:lparen :rparen] 0 :rparen]
    [:pair [:lparen :rparen] 1 :rparen]}
  ; cc7
  #{[:pair [:lparen :rparen] 2 :asterism/eof]
    [:pair [:lparen :rparen] 2 :lparen]}
  ; cc8
  #{[:pair [:lparen :pair :rparen] 3 :asterism/eof]
    [:pair [:lparen :pair :rparen] 3 :lparen]}
  ; cc9
  #{[:pair [:lparen :pair :rparen] 2 :rparen]}
  ; cc10
  #{[:pair [:lparen :rparen] 2 :rparen]}
  ; cc11
  #{[:pair [:lparen :pair :rparen] 3 :rparen]}])



(facts "on trivial grammar construction"
  (let [g (make-grammar :goal nil {} [:goal "abc"])]
    (:terminals g) => {:string-abc (make-terminal [:string-abc "abc"])
                       :asterism/empty (make-terminal [:asterism/empty ""])}
    (:nonterminals g) => #{:asterism/start :goal}
    (:start g) => :asterism/start
    (:productions g) => {:asterism/start #{[:goal]}
                         :goal #{[:string-abc]}}))

(facts "on discovering terminals"
  (let [r345 #"345" ; Patterns use identity
        r9 #"9"     ; to judge equality
        g (make-grammar :a nil {}
            [:a ["1" [[:b "2"]] :b r345]
             :b #{[#{"6" :a} r9] :a ["7" :a "8"]}])]
    (:terminals g) => 
      {:string-1 (make-terminal [:string-1 "1"])
       :string-2 (make-terminal [:string-2 "2"])
       :pattern-345 (make-terminal [:pattern-345 r345])
       :string-6 (make-terminal [:string-6 "6"])
       :string-7 (make-terminal [:string-7 "7"])
       :string-8 (make-terminal [:string-8 "8"])
       :pattern-9 (make-terminal [:pattern-9 r9])
       :asterism/empty (make-terminal [:asterism/empty ""])}))

(facts "on normalization"
  (let [g (make-grammar :a nil {}
            [:a ["1" "2"]
             :b #{"1" #{["2" "3"] ["4" "5"]}}
             :c ["1" #{"2" ["3" "4"]}]
             :d :a])
        p (:productions g)]
    (:a p) => #{[:string-1 :string-2]}
    (:b p) => #{[:string-1] [:string-2 :string-3] [:string-4 :string-5]}
    (:c p) => #{[:string-1 :string-2] [:string-1 :string-3 :string-4]}
    (:d p) => #{[:a]}))

(facts "on whitespace injection"
  (let [g (make-grammar :a "BLANK" {}
            [:a ["1" "2"]
             :b #{["1" "2"] ["3" "4"]}
             :c ["1" #{"2" "3"}]
             :d [:a :b :c]
             :e (no-ws "1" "2")
             :f #{["1" (no-ws "2" "3") "4"]}])
        p (:productions g)]
    (:a p) => #{[:string-1 :string-BLANK :string-2]}
    (:b p) => #{[:string-1 :string-BLANK :string-2]
                [:string-3 :string-BLANK :string-4]}
    (:c p) => #{[:string-1 :string-BLANK :string-2]
                [:string-1 :string-BLANK :string-3]}
    (:d p) => #{[:a :string-BLANK :b :string-BLANK :c]}
    (:e p) => #{[:string-1 :string-2]}
    (:f p) => #{[:string-1 :string-BLANK :string-2 
                 :string-3 :string-BLANK :string-4]}))

(facts "on computing FIRST(x)"
  (let [g (make-grammar :a nil {}
            [:a #{"a1" ["a2" "a3"] ""}
             :b #{"b1" [#{"b2" "b3"} "b4"]}
             :c [:a :b]])
        firsts (generate-first-sets g)]
    (:a firsts) => #{:string-a1 :string-a2 :asterism/empty}
    (:b firsts) => #{:string-b1 :string-b2 :string-b3}
    (:c firsts) => #{:string-a1 :string-a2 :string-b1 :string-b2 :string-b3}
    (generate-first-sets paren-grammar) => paren-firsts))

(facts "on determining state transitions in CC"
  (let [eof :asterism/eof
        nxts   [  eof :lparen :rparen :list :pair ]
        table [[  nil    3      nil     1     2   ]   ; 0
               [  nil    3      nil    nil    4   ]   ; 1
               [  nil   nil     nil    nil   nil  ]   ; 2
               [  nil    6       7     nil    5   ]   ; 3
               [  nil   nil     nil    nil   nil  ]   ; 4
               [  nil   nil      8     nil   nil  ]   ; 5
               [  nil    6      10     nil    9   ]   ; 6
               [  nil   nil     nil    nil   nil  ]   ; 7
               [  nil   nil     nil    nil   nil  ]   ; 8
               [  nil   nil     11     nil   nil  ]   ; 9
               [  nil   nil     nil    nil   nil  ]   ; 10
               [  nil   nil     nil    nil   nil  ]]] ; 11
    (doseq [[i expected] (into {} (map-indexed vector table))
            [j lookahead] (map vector expected nxts)]
      (goto (paren-cc i) lookahead paren-firsts paren-grammar)
        => (exactly (or (get paren-cc j #{}))))))

(facts "on generating the canonical collection for a grammar"
  (let [cc0 (cc0 paren-firsts paren-grammar)
        generated-cc (cc cc0 paren-firsts paren-grammar)]
    (count generated-cc) => 12
    (doseq [cc_i paren-cc]
      generated-cc => (contains (exactly cc_i)))))

(facts "on building action and goto tables"
  (let [cc0 (cc0 paren-firsts paren-grammar)
        {:keys [action-table goto-table]} 
          (build-tables cc0 paren-firsts paren-grammar)
        actions {
          0 {:lparen [:shift (paren-cc 3)]}
          1 {:asterism/eof :accept
             :lparen [:shift (paren-cc 3)]}
          2 {:asterism/eof [:reduce :list [:pair]]
             :lparen [:reduce :list [:pair]]}
          3 {:lparen [:shift (paren-cc 6)]
             :rparen [:shift (paren-cc 7)]}
          4 {:asterism/eof [:reduce :list [:list :pair]]
             :lparen [:reduce :list [:list :pair]]}
          5 {:rparen [:shift (paren-cc 8)]}
          6 {:lparen [:shift (paren-cc 6)]
             :rparen [:shift (paren-cc 10)]}
          7 {:asterism/eof [:reduce :pair [:lparen :rparen]]
             :lparen [:reduce :pair [:lparen :rparen]]}
          8 {:asterism/eof [:reduce :pair [:lparen :pair :rparen]]
             :lparen [:reduce :pair [:lparen :pair :rparen]]}
          9 {:rparen [:shift (paren-cc 11)]}
          10 {:rparen [:reduce :pair [:lparen :rparen]]}
          11 {:rparen [:reduce :pair [:lparen :pair :rparen]]}}
        gotos {
          0 {:list (paren-cc 1)
             :pair (paren-cc 2)}
          1 {:pair (paren-cc 4)}
          3 {:pair (paren-cc 5)}
          6 {:pair (paren-cc 9)}}]
    (doseq [[idx expected] actions]
      (get action-table (paren-cc idx)) => (exactly expected))
    (doseq [[idx expected] gotos]
      (get goto-table (paren-cc idx)) => (exactly expected))))

(facts "on collapsing nonterminals and eliding terminals"
  (let [p (parser {:whitespace nil
                   :terminals {:<space> " "
                               :ident #"[a-zA-Z]\w*"}}
            :list :<list>
            :<list> #{[:<list> :<space> :<item>] :<item>}
            :<item> :ident)
        tree (p "a bc d")]
    (:type tree) => :list
    (map parser/lexeme (:children tree)) =>
      ["a" "bc" "d"]))