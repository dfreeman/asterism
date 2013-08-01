(ns asterism.parser-test
  (:require [midje.sweet :refer [facts contains exactly throws]]
            [asterism.parser :refer :all]
            [clojure.pprint :refer [pprint]]))

; Recognizes strings of the form (^n )^n +
(def paren-grammar
  (make-grammar :list nil
    {:lparen "("
     :rparen ")"}
    [:list #{[:list :pair] :pair}
     :pair #{[:lparen :pair :rparen] [:lparen :rparen]}]))

; Firsts sets
(def paren-firsts
  {:asterism.parser/start #{:lparen}
   :pair #{:lparen}
   :list #{:lparen}
   :lparen #{:lparen}
   :rparen #{:rparen}
   :asterism/empty #{:asterism/empty}
   :asterism/eof #{:asterism/eof}})

; CC for the paren-grammar
(def paren-cc [
  ; cc0
  #{[:asterism.parser/start [:list] 0 :asterism/eof]
    [:list [:list :pair] 0 :lparen]
    [:list [:list :pair] 0 :asterism/eof]
    [:list [:pair] 0 :lparen]
    [:list [:pair] 0 :asterism/eof]
    [:pair [:lparen :pair :rparen] 0 :lparen]
    [:pair [:lparen :pair :rparen] 0 :asterism/eof]
    [:pair [:lparen :rparen] 0 :lparen]
    [:pair [:lparen :rparen] 0 :asterism/eof]}
  ; cc1
  #{[:asterism.parser/start [:list] 1 :asterism/eof]
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
    (:terminals g) => {:string-abc {:matcher "abc"}
                       :asterism/empty {:matcher ""}}
    (:nonterminals g) => #{:asterism.parser/start :goal}
    (:start g) => :asterism.parser/start
    (:productions g) => {:asterism.parser/start #{[:goal]}
                         :goal #{[:string-abc]}}))

(facts "on discovering terminals"
  (let [r345 #"345" ; Patterns use identity
        r9 #"9"     ; to judge equality
        g (make-grammar :a nil {}
            [:a ["1" [[:b "2"]] :b r345]
             :b #{[#{"6" :a} r9] :a [7 :a 8]}])]
    (:terminals g) => 
      {:string-1 {:matcher "1"}
       :string-2 {:matcher "2"}
       :pattern-345 {:matcher r345}
       :string-6 {:matcher "6"}
       :long-7 {:matcher 7}
       :long-8 {:matcher 8}
       :pattern-9 {:matcher r9}
       :asterism/empty {:matcher ""}}))

(facts "on normalization"
  (let [g (make-grammar :a nil {}
            [:a [1 2]
             :b #{1 #{[2 3] [4 5]}}
             :c [1 #{2 [3 4]}]
             :d :a])
        p (:productions g)]
    (:a p) => #{[:long-1 :long-2]}
    (:b p) => #{[:long-1] [:long-2 :long-3] [:long-4 :long-5]}
    (:c p) => #{[:long-1 :long-2] [:long-1 :long-3 :long-4]}
    (:d p) => #{[:a]}))

(facts "on whitespace injection"
  (let [g (make-grammar :a "BLANK" {}
            [:a [1 2]
             :b #{[1 2] [3 4]}
             :c [1 #{2 3}]
             :d [:a :b :c]
             :e (no-ws 1 2)
             :f #{[1 (no-ws 2 3) 4]}])
        p (:productions g)]
    (:a p) => #{[:long-1 :string-BLANK :long-2]}
    (:b p) => #{[:long-1 :string-BLANK :long-2] [:long-3 :string-BLANK :long-4]}
    (:c p) => #{[:long-1 :string-BLANK :long-2] [:long-1 :string-BLANK :long-3]}
    (:d p) => #{[:a :string-BLANK :b :string-BLANK :c]}
    (:e p) => #{[:long-1 :long-2]}
    (:f p) => #{[:long-1 :string-BLANK :long-2 :long-3 :string-BLANK :long-4]}))

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
        {:keys [action goto]} (build-tables cc0 paren-firsts paren-grammar)
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
      (get action (paren-cc idx)) => (exactly expected))
    (doseq [[idx expected] gotos]
      (get goto (paren-cc idx)) => (exactly expected))))

; This will change drastically when scanning actually works
(facts "on making a parser"
  (let [p (parser {:whitespace nil}
            :goal :first
            :first ["a" "b"])]
    (p "xxx") => (throws Exception "not enough!")
    (p "ab") => 
      {:type :goal
       :children [{:type :first
                   :children 
                    [{:type :string-a
                      :lexeme "a"
                      :meta {:start 0
                             :length 1}}
                     {:type :string-b
                      :lexeme "b"
                      :meta {:start 1
                             :length 1}}]}]}))