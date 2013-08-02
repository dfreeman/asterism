(ns asterism.parser.mocks
  (:require [asterism.parser.protocols :as parser]
            [midje.sweet :refer [facts]]))

(facts "to make that warning go away"
  true => true)

(extend-protocol parser/ITerminal
  clojure.lang.Associative
  (id [this] (:id this))
  (matcher [this] (:matcher this))
  (elide? [this] (:elide? this))
  (classes [this] (:classes this))
  (dominates [this] (:dominates this))
  (submits-to [this] (:submits-to this)))

(extend-protocol parser/IToken
  clojure.lang.Associative
  (token-type [this] (:type this))
  (lexeme [this] (:lexeme this))
  (source-info [this] (:source-info this)))