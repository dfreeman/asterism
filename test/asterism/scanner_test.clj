(ns asterism.scanner-test
  (:require [midje.sweet :refer [facts]]
            [asterism :as ast]
            [asterism.mocks]
            [asterism.util :as util]
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
  (let [types (fn [tokens] 
                (util/set-for [[offset token] tokens]
                  (ast/type token)))
        scanner (scanner
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
    (types (scan scanner 0 #{:a})) => #{:a}
    (types (scan scanner 0 #{:b})) => #{:a}
    (types (scan scanner 0 #{:c})) => #{:a}
    (types (scan scanner 0 #{:d})) => #{:a}
    (types (scan scanner 2 #{:a})) => #{:a}
    (types (scan scanner 2 #{:b})) => #{:b}
    (types (scan scanner 5 #{:c})) => #{:c}
    (types (scan scanner 8 #{:d})) => #{:d}
    (types (scan scanner 0 #{:e})) => #{:e}
    (types (scan scanner 0 #{:c :e})) => #{:a :e}))