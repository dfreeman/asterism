(ns asterism.examples.core
  (:require [asterism.core :refer [defterm]]))

; This was a quick transliteration from json.org; in general I believe they're also compatible
; with the Clojure reader, so just calling read-string on the resulting lexeme should get a
; reasonable value. Safety not guaranteed, though.

(defterm num-lit #"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")
(defterm str-lit #"\"(?:[^\"\\]|\\[\\\"/bfnrt]|\\u[0-9a-f]{4})*\"")   ;" syntax highlighting fail