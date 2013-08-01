(ns asterism.scanner-test
  (:require [midje.sweet :refer [facts]]
            [asterism.scanner :refer :all]))

(facts "on terminal dominance types"
  (doseq [terminals [; submits-to id
                     {:int {}
                      :ident {:submits-to #{:int}}}
                     ; submits-to class
                     {:int {:classes #{:keyword}}
                      :ident {:submits-to #{:keyword}}}
                     ; dominates id
                     {:int {:dominates #{:ident}}
                      :ident {}}
                     ; dominates class
                     {:int {:dominates #{:name}}
                      :ident {:classes #{:name}}}]]
    (dominates? terminals :int :ident) => true
    (dominates? terminals :ident :int) => false))

(facts "on terminal dominance"
  (let [scanner (scanner
                  "a ab ac ad"
                  {:a {:matcher "a"
                       :dominates #{:b}}
                   :b {:matcher "ab"
                       :dominates #{:c-class}}
                   :c {:matcher "ac"
                       :classes #{:c-class}}
                   :d {:matcher "ad"
                       :submits-to #{:a}}
                   :e {:matcher "a"}})]
    (scan scanner 0 #{:a}) => #{[1 {:type :a :lexeme "a"}]}
    (scan scanner 0 #{:b}) => #{[1 {:type :a :lexeme "a"}]}
    (scan scanner 0 #{:c}) => #{[1 {:type :a :lexeme "a"}]}
    (scan scanner 0 #{:d}) => #{[1 {:type :a :lexeme "a"}]}
    (scan scanner 2 #{:a}) => #{[3 {:type :a :lexeme "a"}]}
    (scan scanner 2 #{:b}) => #{[4 {:type :b :lexeme "ab"}]}
    (scan scanner 5 #{:c}) => #{[7 {:type :c :lexeme "ac"}]}
    (scan scanner 8 #{:d}) => #{[10 {:type :d :lexeme "ad"}]}
    (scan scanner 0 #{:e}) => #{[1 {:type :e :lexeme "a"}]}
    (scan scanner 0 #{:c :e}) => #{[1 {:type :e :lexeme "a"}]
                                   [1 {:type :a :lexeme "a"}]}))