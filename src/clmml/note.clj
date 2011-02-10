(ns clmml.note
  "Symbol parser for Clmml. Parses notes, rests, and measures."
  (:require [edu.arizona.fnparse [hound :as h] [core :as c]]
            [clojure.contrib [except :as except]])
  (:use [clojure.contrib.string :only [codepoints]])
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
; c##WH..++>2<3!70?30
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

(h/defrule digits
  "Decimal digits"
  (h/hook #(Integer. (str* %))
          (h/rep (h/set-term "Decimal digits" (str-codepoints "0123456789")))))

(h/defrule decimal
  "A decimal number"
  (h/hook #(Double. (str* %))
          (h/cat digits (h/opt (h/cat (h/lit ".") digits)))))

(defn note-name-char [string]
  (if (empty? string) string 
    (or (Character/isLetter (codepoint string)) 
        (= "#" string) (= "_" string) (= "$" string))))

(h/defrule note-name
  "Note name"
  (h/hook #(str* %)
          (h/rep (h/term "Unicode Letter, #, $, or _"
            #))))

(h/defrule plus-minus
  "Plus or minus symbol"
  (h/+ (h/lit "+") (h/lit "-")))

(h/defrule plusses
  "Series of plusses"
  (h/rep (h/lit "+")))

(h/defrule minuses
  "Series of minuses"
  (h/rep (h/lit "-")))

(h/defrule plus-minus-digits
  "Plus, minus, equals a number"
  (h/+ (h/cat (h/lit "+") (h/+ (h/rep (h/lit "+")) digits))
       (h/cat (h/lit "-") (h/+ (h/rep (h/lit "-")) digits))
       digits))

(h/defrule plus-minus-decimal
  "Plus, minus, equals a decimal number"
  (h/+ (h/cat (h/lit "+") (h/+ (h/rep (h/lit "+")) decimal))
       (h/cat (h/lit "-") (h/+ (h/rep (h/lit "-")) decimal))
       decimal))

(h/defrule octave 
  "Note octave"
  plus-minus-digits)

(h/defrule note
  "Note name with optional octave"
  (h/cat note-name (h/opt octave)))

(h/defrule note-number
  "Note Number"
  (h/cat (h/lit "<") plus-minus-decimal (h/lit ">")))

(h/defrule mbt
  "Measures:Beats:Ticks duration"
  (h/cat digits 
         (h/opt (h/cat (h/lit ":") digits 
                       (h/opt (h/cat (h/lit ":") digits))))))

(h/defrule smpte
  "SMPTE duration"
  (h/cat (h/opt (h/cat 
               (h/opt (h/cat 
                      (h/opt (h/cat 
                             digits (h/lit ":"))) 
                      digits (h/lit ":"))) 
               digits (h/lit ":"))) digits (h/lit "s")))

(h/defrule dotted
  "Dotted duration modifier"
  (h/rep (h/lit ".")))

(h/defrule fraction-notation
  "Fractional notation"
  (h/rep (h/cat (h/set-term 
                  "Fractional Notation" 
                  (str-codepoints "whqistxo")) (h/opt dotted))))

(h/defrule fraction
  "%-delimited fraction"
  (h/cat (h/opt digits) 
         (h/rep (h/lit "%")) 
         (h/opt digits)))

(h/defrule duration-atom
  "Single duration atom"
  (h/cat 
    (h/+ 
      fraction-notation 
      ; smpte, fraction and mbt can all start with a number, 
      ; so enable backtracking here.
      (h/lex fraction)
      (h/lex smpte)
      (h/lex mbt))
    (h/opt dotted)))

(h/defrule duration
  "Duration attribute"
  (h/cat (h/rep (h/lit "*")) 
         (h/opt (h/cat duration-atom 
                       (h/rep* (h/cat plus-minus duration-atom))))))

(h/defrule displacement
  "Note displacement attribute"
  (h/cat (h/+ (h/lit ">") (h/lit "<")) (h/opt plus-minus-digits)))

(h/defrule attack
  "Note attack attribute"
  (h/cat (h/rep (h/lit "!")) (h/opt plus-minus-digits)))

(h/defrule decay
  "Note decay attribute"
  (h/cat (h/rep (h/lit "?")) (h/opt plus-minus-digits)))

(h/defrule note-token
  "Note token"
  (h/cat (h/+ note note-number) 
         (h/opt duration)
         (h/opt displacement)
         (h/opt attack)
         (h/opt decay)))

(h/defrule measure
  "Measure bar"
  (h/cat (h/lit "|") (h/opt (h/+ (h/rep (h/lit "|")) digits))))

(h/defrule token
  "Token"
  (h/cat (h/+ note-token measure) h/<end-of-input>))

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
  (h/match 
    ; Build a state object with Location starting from 1.
    ; Convert the input into a list of codepoint strings.
    (State. (str-codepoints input) 0 (Location. 1 1) #{} nil alter-location)
    token
    :success-fn (fn [product position] product)
    :failure-fn (fn [error]
                  (except/throwf "Parsing error: %s"
                    (h/format-parse-error error)))))

