(ns asterism.parser.protocols
  (:refer-clojure :exclude [type]))

(defprotocol IMatcher
  (matches? [this input offset type source-info]
    "Tests whether this object matches the given input starting at the given
    offset. If it does not match, returns nil. If it does, returns a vector
    containing the number of characters consumed and an IToken instance."))

(defprotocol INonterminal
  (collapse? [this] 
    "If true, this nonterminal will be collapsed, splicing its descendants
    directly into its parent's list of children"))

; Must be instances of IMeta
(defprotocol ITerminal
  (matcher [this] "The IMatcher this terminal uses to consume input")
  (elide? [this] "If true, this terminal will not be included in the tree")
  (classes [this] "The classes to which this terminal belongs")
  (dominates [this] "The IDs/classes of terminal which this one dominates")
  (submits-to [this] "The IDs/classes of terminals which dominate this one"))

(defprotocol IToken
  (token-type [this] "The ID of the terminal that produced this token")
  (lexeme [this] "The lexeme that this token consumed")
  (source-info [this] "Metadata regarding where this token originated in source"))

(defn matcher? [x]
  (satisfies? IMatcher x))

(defn nonterminal? [x]
  (satisfies? INonterminal x))

(defn terminal? [x]
  (satisfies? ITerminal x))

(defn token? [x]
  (satisfies? IToken x))