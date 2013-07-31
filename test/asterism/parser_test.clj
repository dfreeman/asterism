(ns asterism.parser-test
  (:require [midje.sweet :refer [facts contains exactly]]
            [asterism.parser :refer :all]
            [clojure.pprint :refer [pprint]]))

; Recognizes strings of the form (^n )^n +
(def paren-grammar
  (make-grammar :goal nil
    [:goal :list
     :list #{[:list :pair] :pair}
     :pair #{["(" :pair ")"] ["(" ")"]}]))

; Firsts sets
(def paren-firsts
  {:goal #{"("}
   :pair #{"("}
   :list #{"("}
   "("   #{"("}
   ")"   #{")"}
   ""    #{""}
   :eof  #{:eof}})

; CC for the paren-grammar
(def paren-cc [
  ; cc0
  #{[:goal [:list] 0 :eof]
    [:list [:list :pair] 0 "("]
    [:list [:list :pair] 0 :eof]
    [:list [:pair] 0 "("]
    [:list [:pair] 0 :eof]
    [:pair ["(" :pair ")"] 0 "("]
    [:pair ["(" :pair ")"] 0 :eof]
    [:pair ["(" ")"] 0 "("]
    [:pair ["(" ")"] 0 :eof]}
  ; cc1
  #{[:goal [:list] 1 :eof]
    [:list [:list :pair] 1 :eof]
    [:list [:list :pair] 1 "("]
    [:pair ["(" :pair ")"] 0 :eof]
    [:pair ["(" :pair ")"] 0 "("]
    [:pair ["(" ")"] 0 :eof]
    [:pair ["(" ")"] 0 "("]}
  ; cc2
  #{[:list [:pair] 1 :eof]
    [:list [:pair] 1 "("]}
  ; cc3
  #{[:pair ["(" :pair ")"] 0 ")"]
    [:pair ["(" :pair ")"] 1 :eof]
    [:pair ["(" :pair ")"] 1 "("]
    [:pair ["(" ")"] 0 ")"]
    [:pair ["(" ")"] 1 :eof]
    [:pair ["(" ")"] 1 "("]}
  ; cc4
  #{[:list [:list :pair] 2 :eof]
    [:list [:list :pair] 2 "("]}
  ; cc5
  #{[:pair ["(" :pair ")"] 2 :eof]
    [:pair ["(" :pair ")"] 2 "("]}
  ; cc6
  #{[:pair ["(" :pair ")"] 0 ")"]
    [:pair ["(" :pair ")"] 1 ")"]
    [:pair ["(" ")"] 0 ")"]
    [:pair ["(" ")"] 1 ")"]}
  ; cc7
  #{[:pair ["(" ")"] 2 :eof]
    [:pair ["(" ")"] 2 "("]}
  ; cc8
  #{[:pair ["(" :pair ")"] 3 :eof]
    [:pair ["(" :pair ")"] 3 "("]}
  ; cc9
  #{[:pair ["(" :pair ")"] 2 ")"]}
  ; cc10
  #{[:pair ["(" ")"] 2 ")"]}
  ; cc11
  #{[:pair ["(" :pair ")"] 3 ")"]}])



(facts "on trivial grammar construction"
  (let [g (make-grammar :goal nil [:goal ""])]
    (:terminals g) => #{""}
    (:nonterminals g) => #{:goal}
    (:start g) => :goal
    (:productions g) => {:goal #{[""]}}))

(facts "on discovering terminals"
  (let [r345 #"345" ; Patterns use identity
        r9 #"9"     ; to judge equality
        g (make-grammar :a nil
            [:a ["1" [[:b "2"]] :b r345]
             :b #{[#{"6" :a} r9] :a [7 :a 8]}])]
    (:terminals g) => #{"1" "2" r345 "6" 7 8 r9}))

(facts "on normalization"
  (let [g (make-grammar :a nil
            [:a [1 2]
             :b #{1 #{[2 3] [4 5]}}
             :c [1 #{2 [3 4]}]
             :d :a])
        p (:productions g)]
    (:a p) => #{[1 2]}
    (:b p) => #{[1] [2 3] [4 5]}
    (:c p) => #{[1 2] [1 3 4]}
    (:d p) => #{[:a]}))

(facts "on whitespace injection"
  (let [g (make-grammar :a "BLANK"
            [:a [1 2]
             :b #{[1 2] [3 4]}
             :c [1 #{2 3}]
             :d [:a :b :c]
             :e (no-ws 1 2)
             :f #{[1 (no-ws 2 3) 4]}])
        p (:productions g)]
    (:a p) => #{[1 "BLANK" 2]}
    (:b p) => #{[1 "BLANK" 2] [3 "BLANK" 4]}
    (:c p) => #{[1 "BLANK" 2] [1 "BLANK" 3]}
    (:d p) => #{[:a "BLANK" :b "BLANK" :c]}
    (:e p) => #{[1 2]}
    (:f p) => #{[1 "BLANK" 2 3 "BLANK" 4]}))

(facts "on computing FIRST(x)"
  (let [g (make-grammar :a nil
            [:a #{"a1" ["a2" "a3"] ""}
             :b #{"b1" [#{"b2" "b3"} "b4"]}
             :c [:a :b]])
        firsts (generate-first-sets g)]
    (:a firsts) => #{"a1" "a2" ""}
    (:b firsts) => #{"b1" "b2" "b3"}
    (:c firsts) => #{"a1" "a2" "b1" "b2" "b3"}
    (generate-first-sets paren-grammar) => paren-firsts))

(facts "on determining state transitions in CC"
  (let [nxts   [ :eof  "("   ")" :list :pair ]
        table [[  nil   3    nil    1     2  ]   ; 0
               [  nil   3    nil   nil    4  ]   ; 1
               [  nil  nil   nil   nil   nil ]   ; 2
               [  nil   6     7    nil    5  ]   ; 3
               [  nil  nil   nil   nil   nil ]   ; 4
               [  nil  nil    8    nil   nil ]   ; 5
               [  nil   6    10    nil    9  ]   ; 6
               [  nil  nil   nil   nil   nil ]   ; 7
               [  nil  nil   nil   nil   nil ]   ; 8
               [  nil  nil   11    nil   nil ]   ; 9
               [  nil  nil   nil   nil   nil ]   ; 10
               [  nil  nil   nil   nil   nil ]]] ; 11
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
          0 {"(" [:shift (paren-cc 3)]}
          1 {:eof :accept
             "(" [:shift (paren-cc 3)]}
          2 {:eof [:reduce :list [:pair]]
             "(" [:reduce :list [:pair]]}
          3 {"(" [:shift (paren-cc 6)]
             ")" [:shift (paren-cc 7)]}
          4 {:eof [:reduce :list [:list :pair]]
             "(" [:reduce :list [:list :pair]]}
          5 {")" [:shift (paren-cc 8)]}
          6 {"(" [:shift (paren-cc 6)]
             ")" [:shift (paren-cc 10)]}
          7 {:eof [:reduce :pair ["(" ")"]]
             "(" [:reduce :pair ["(" ")"]]}
          8 {:eof [:reduce :pair ["(" :pair ")"]]
             "(" [:reduce :pair ["(" :pair ")"]]}
          9 {")" [:shift (paren-cc 11)]}
          10 {")" [:reduce :pair ["(" ")"]]}
          11 {")" [:reduce :pair ["(" :pair ")"]]}}
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
  (let [p (parser {:ws nil}
            :goal :first
            :first ["a" "b"])]
    (p ["a"]) => :asterism.parser/failure
    (p ["a" "b" :eof]) => 
      {:tag :goal
       :children [{:tag :first
                   :children ["a" "b"]}]}))