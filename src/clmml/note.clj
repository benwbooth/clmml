(ns clmml.note
  "Symbol parser for Clmml. Parses notes, rests, and measures."
  (:require [edu.arizona.fnparse [hound :as h] [core :as c]])
  (:use [edu.arizona.fnparse.hound 
         :only [match format-parse-error rep rep* hook 
                defrule cat lit opt lex set-term term <end-of-input>]]
        [clojure.contrib.string :only [codepoints]]
        [clojure.contrib.condition]
        [matchure])
  (:import (edu.arizona.fnparse.hound State)))

; - The ^ character clashes with the clojure metadata reader macro, so it
; would need to be changed to !, for example, when writing direct clojure
; code.
; - The / character clashes with clojure's namespace separator character, so
; it could be changed to %, for example.
; - The @ character clashes with clojure's deref reader macro. Since it's
; rarely used, you could just replace it with something like ~(time #)
; instead.
; - The channel pressure syntax, e.g. +60, would have to be changed to
; something like ++60, because clojure interprets +60 as the numeric literal
; 60, and drops the + sign.
; - The [] constant insertion clashes with clojure's vector syntax, so it
; could be changed to <>.
; - Since we're now using <> to represent constant insertion in clojure,
; interval and microtonal notation could be expressed as <<>>.
;
; c##*WH..++>2<3!70?30
; bnwh..

(defn str-codepoints 
  "Take an input string and return a lazy seq of codepoints 
  as a series of one-codepoint strings."
  [string]
  (map #(String. (Character/toChars %)) (codepoints string)))

(defn codepoint
  "Take a string and return the integer value of the first codepoint."
  [string]
  (.codePointAt string 0))

(defn str* [objects]
  (apply str objects))

;;; The parser starts here:

(defrule digits
  "Decimal digits"
  (hook #(Integer. (str* %))
          (rep (set-term "Decimal digits" (str-codepoints "0123456789")))))

(defrule decimal
  "A decimal number"
  (hook #(Double. (str* %))
          (cat digits (opt (cat (lit ".") digits)))))

(defn note-name-char [string]
  (if (empty? string) string 
    (or (Character/isLetter (codepoint string)) 
        (= "#" string) (= "_" string) (= "$" string))))

(defrule note-name
  "Note name"
  (hook #(str* %)
          (rep (term "Unicode Letter, #, $, or _"
            note-name-char))))

(defrecord PlusMinusDigits [rel value])

(defn-match make-plus-minus-digits
  ([nil] (PlusMinusDigits. false nil))
  ([(and (number? ?) ?value)] (PlusMinusDigits. false value))
  ([["+" nil]] (PlusMinusDigits. true 1))
  ([["+" (and (number? ?) ?value)]] (PlusMinusDigits. true value))
  ([["+" [& ?value]]] (PlusMinusDigits. true (inc (count value))))
  ([["-" nil]] (PlusMinusDigits. true -1))
  ([["-" (and (number? ?) ?value)]] (PlusMinusDigits. true (- value)))
  ([["-" [& ?value]]] (PlusMinusDigits. true (- (inc (count value))))))

(defrule plus-minus-digits
  "Plus, minus, equals a number"
  (hook make-plus-minus-digits
    (h/+ (cat (lit "+") (opt (h/+ (rep (lit "+")) digits)))
         (cat (lit "-") (opt (h/+ (rep (lit "-")) digits)))
         digits)))

(defrule plus-minus-decimal
  "Plus, minus, equals a decimal number"
  (hook make-plus-minus-digits
    (h/+ (cat (lit "+") (opt (h/+ (rep (lit "+")) decimal)))
         (cat (lit "-") (opt (h/+ (rep (lit "-")) decimal)))
         decimal)))

(defrecord Octave [value])

(defrule octave 
  "Note octave"
  (hook #(Octave. %) plus-minus-digits))

(defrule note
  "Note name with optional octave"
  (cat note-name (opt octave)))

(defrule note-number
  "Note Number"
  (cat (lit "<") plus-minus-decimal (lit ">")))

(defrecord MBT [measures beats ticks])

(defrule mbt
  "Measures:Beats:Ticks duration"
  (hook (fn-match
          ([[?m nil]] (MBT. m 0 0))
          ([[?m [_ ?b nil]]] (MBT. m b 0))
          ([[?m [_ ?b [_ ?t]]]] (MBT. m b t)))
  (cat digits 
         (opt (cat (lit ":") digits 
                       (opt (cat (lit ":") digits)))))))

(defrule smpte
  "SMPTE duration"
  (cat (opt (cat 
               (opt (cat 
                      (opt (cat 
                             digits (lit ":"))) 
                      digits (lit ":"))) 
               digits (lit ":"))) digits (lit "s")))

(defrule dotted
  "Dotted duration modifier"
  (rep (lit ".")))

(defrule fraction-notation
  "Fractional notation"
  (rep (cat (set-term 
                  "Fractional Notation" 
                  (str-codepoints "whqistxo")) (opt dotted))))

(defrule fraction
  "%-delimited fraction"
  (cat (opt digits) 
         (rep (lit "%")) 
         (opt digits)))

(defrule duration-atom
  "Single duration atom"
  (cat 
    (h/+ 
      fraction-notation 
      ; smpte, fraction and mbt can all start with a number, 
      ; so enable backtracking here.
      (lex fraction)
      (lex smpte)
      (lex mbt))
    (opt dotted)))

(defrule plus-minus
  "Plus or minus symbol"
  (h/+ (lit "+") (lit "-")))

(defrule duration
  "Duration attribute"
  (cat (rep (lit "*")) 
         (opt (cat duration-atom 
                       (rep* (cat plus-minus duration-atom))))))

(defrecord Displacement [direction value])

(defrule displacement
  "Note displacement attribute"
  (h/hook (fn-match
            ([[">" (and (instance? PlusMinusDigits ?) ?p)]] (Displacement. :right p))
            ([[">" [& ?value]]] (Displacement. :right (PlusMinusDigits. false (inc (count value)))))
            ([["<" (and (instance? PlusMinusDigits ?) ?p)]] (Displacement. :left p))
            ([["<" [& ?value]]] (Displacement. :left (PlusMinusDigits. false (inc (count value))))))
    (h/+ (cat (lit ">") (h/+ (rep* (lit ">")) plus-minus-digits))
         (cat (lit "<") (h/+ (rep* (lit "<")) plus-minus-digits)))))

(defrecord Attack [value])

(defrule attack
  "Note attack attribute"
  (hook (fn-match 
            ([["!" (and (instance? PlusMinusDigits ?) ?p)]] (Attack. p))
            ([["!" [& ?value]]] (Attack. (PlusMinusDigits. false (inc (count value))))))
          (cat (lit "!") (h/+ (rep* (lit "!")) plus-minus-digits))))

(defrecord Decay [value])

(defrule decay
  "Note decay attribute"
  (hook (fn-match 
            ([["?" (and (instance? PlusMinusDigits ?) ?p)]] (Decay. p))
            ([["?" [& ?value]]] (Decay. (PlusMinusDigits. false (inc (count value))))))
          (cat (lit "?") (h/+ (rep* (lit "?")) plus-minus-digits))))

(defrule note-token
  "Note token"
  (cat (h/+ note note-number) 
         (opt duration)
         (opt displacement)
         (opt attack)
         (opt decay)))

(defrule measure
  "Measure bar"
  (hook (fn-match 
            ([["|" nil]] {:type :measure :value 1})
            ([["|" (and (integer? ?) ?value)]] {:type :measure :value value})
            ([["|" [& ?value]]] {:type :measure :value (inc (count value))}))
          (cat (lit "|") (opt (+ (rep (lit "|")) digits)))))

(defrule token
  "Token"
  (cat (h/+ note-token measure) <end-of-input>))

(defrecord Location 
  [line column]
  c/ALocation
    (c/location-code [this] 
                   (if (= line 1) 
                     (format "character %s" column)
                     (format "line %s, character %s" line column)))
  c/ALineAndColumnLocation
    (c/location-inc-line [this] (assoc this :line (inc line), :column 1))
    (c/location-inc-column [this] (assoc this :column (inc column))))

(def standard-break-chars #{"\n" "\b" "\f"})

(defn alter-location [string]
  {:pre #{(string? string)}}
  (if (standard-break-chars string)
    c/location-inc-line c/location-inc-column))

(defn read-symbol
  "Parse a clmml symbol."
  [input]
  (match 
    ; Build a state object with Location starting from 1.
    ; Convert the input into a list of codepoint strings.
    (State. (str-codepoints input) 0 (Location. 1 1) #{} nil alter-location)
    token
    :success-fn (fn [product position] product)
    :failure-fn (fn [error]
                  (raise :type :parse-error 
                         :message (format "Parsing error: %s" 
                                          (format-parse-error error))))))

