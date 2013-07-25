(ns asterism.core-test
  (:require [asterism.core :refer :all]
            [midje.sweet :refer :all]))

(facts "about the core"
  "it's being tested" => truthy
  nil => falsey)