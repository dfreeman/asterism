(ns asterism.parser.grammar-test
  (:require [midje.sweet :refer [facts]]
            [asterism.parser.grammar :refer :all]))

(facts "on trivial grammar construction"
  (let [g (make-grammar :goal {} [:goal "abc"])]
    (:terminals g) => {:string-abc (make-terminal [:string-abc "abc"])
                       :asterism/empty (make-terminal [:asterism/empty ""])}
    (:nonterminals g) => #{:asterism/start :goal}
    (:start g) => :asterism/start
    (:productions g) => {:asterism/start #{[:goal]}
                         :goal #{[:string-abc]}}))

(facts "on discovering terminals"
  (let [r345 #"345" ; Patterns use identity
        r9 #"9"     ; to judge equality
        g (make-grammar :a {}
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
  (let [g (make-grammar :a {}
            [:a ["1" "2"]
             :b #{"1" #{["2" "3"] ["4" "5"]}}
             :c ["1" #{"2" ["3" "4"]}]
             :d :a])
        p (:productions g)]
    (:a p) => #{[:string-1 :string-2]}
    (:b p) => #{[:string-1] [:string-2 :string-3] [:string-4 :string-5]}
    (:c p) => #{[:string-1 :string-2] [:string-1 :string-3 :string-4]}
    (:d p) => #{[:a]}))
