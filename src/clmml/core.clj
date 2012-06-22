;; TODO:
;;  key signatures
;;  time signatures
;;  chords, inversions
;;  cc's
;;  rpn's
;;  sysex
;;  meta events
;;  GM instruments
;;  Percussion instruments
;;  pitch wheel
;;  channel pressure
;;  program change

;; allowable characters in clojure symbols:
;; a1!#$&*_-=+|:<>'?/.
;; / must be followed by a non-number
;; : cannot be followed by another :
;; # not followed by ' or _, not at beginning
;; ' cannot be at the beginning

(ns clmml.core
  (:refer-clojure :exclude [char replace reverse])
  (:use [the.parsatron] 
        [data.priority-map]
        [clojure.string]
        [overtone.midi]
        [overtone.osc]))

(defrecord Music [music duration])

;; (def options 
;;   {:track 0 ; track number
;;    :channel 0 ; channel number
;;    :meter [4 4] ; meter:
;;                 ;   beats-per-measure (default 4)
;;                 ;   beats-per-whole-note (default 4)
;;                 ;   ticks-per-metronome-click (depends on PPQ setting)
;;                 ;   notated-32nd-notes-per-quarter-note (default 8)
;;    :last-meter-event 0 ; tick value of last meter event
;;    :buffer 100 ; buffer size in ticks--necessary for negative note displacement
;;    :ticks 0 ; current tick value
;;    :target-ticks nil ; stop generaing MIDI events after target-ticks is reached
;;    :octave 4 ; default octave
;;    :value-fn #({:octave %}) ; callback function for when a value is assigned
;;    :displacement 0 ; note start time displacement in +/- ticks
;;    :duration 0 ; default note duration amount in ticks
;;    :duration-unit 0 ; duration unit size in ticks (ratios are multiplied by this)
;;    :attack 64 ; default attack value (0-127)
;;    :release 64 ; default release value (0-127)
;;    :key-pressure 64 ; default key-pressure value (0-127)
;;    })

;; table of note names -> relative values
(def notes (apply hash-map '(
    C -12 C# -11 C## -10 Cb -13 Cbb -14 Cn -12 
    D -10 D# -9  D## -8  Db -11 Dbb -12 Dn -10
    E -8  E# -7  E## -6  Eb -9  Ebb -10 En -8
    F -7  F# -6  F## -5  Fb -8  Fbb -9  Fn -7
    G -5  G# -4  G## -3  Gb -6  Gbb -7  Gn -5
    A -3  A# -2  A## -1  Ab -4  Abb -5  An -3
    B -1  B# 0   B## 1   Bb -2  Bbb -3  Bn -1
    c 0   c# 1   c## 2   cb -1  cbb -2  cn 0  
    d 2   d# 3   d## 4   db 1   dbb 0   dn 2  
    e 4   e# 5   e## 6   eb 3   ebb 2   en 4  
    f 5   f# 6   f## 7   fb 4   fbb 3   fn 5  
    g 7   g# 8   g## 9   gb 6   gbb 5   gn 7  
    a 9   a# 10  a## 11  ab 8   abb 7   an 9  
    b 11  b# 12  b## 13  bb 10  bbb 9   bn 11 
  )))

;; convert note/octave value to MIDI key number
(defn note-value [note attrs]
  (let [octave (max -1 (min 9 (or (:octave attrs) 4)))
        note-val (find notes (symbol note))]  
    (max 0 (min 127 (+ (* 12 (inc octave)) note-val)))))

(defn play-note [note attrs]
  (fn [options]
    (let [attrs (merge options attrs)
          value (note-value note attrs)
          ticks (or (:ticks attrs) 0)
          begin-ticks (+ ticks (or (:displacement attrs) 0))
          end-ticks (+ ticks (or (:duration attrs) 0))
          note-on-event (javax.sound.midi.MidiEvent. 
                          (doto (javax.sound.midi.ShortMessage.)
                            (.setMessage
                              javax.sound.midi.ShortMessage/NOTE_ON 
                              (max 0 (min 15 (:channel attrs)))
                              value
                              (max 0 (min 127 (:attack attrs)))))
                          begin-ticks)
          key-pressure-event (javax.sound.midi.MidiEvent. 
                               (doto (javax.sound.midi.ShortMessage.)
                                 (.setMessage
                                   javax.sound.midi.ShortMessage/POLY_PRESSURE
                                   (max 0 (min 15 (:channel attrs)))
                                   value
                                   (max 0 (min 127 (:key-pressure attrs)))))
                               begin-ticks)
          note-off-event (javax.sound.midi.MidiEvent. 
                           (doto (javax.sound.midi.ShortMessage.)
                             (.setMessage
                               javax.sound.midi.ShortMessage/NOTE_OFF
                               (max 0 (min 15 (:channel attrs)))
                               value
                               (max 0 (min 127 (:release attrs)))))
                           end-ticks)]
      {:value-fn #({:octave %})
       :music (if (contains? attrs :key-pressure-on)
                (if (and (not (contains? attrs :attack-on))
                         (not (contains? attrs :release-on)))
                  (list key-pressure-event {:ticks end-ticks})
                  (list note-on-event key-pressure-event note-off-event))
                (list note-on-event note-off-event))})))

(defn parse-value [param]
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)
          old-value (or (find param options) 0)]
      (let->> [sign (choice (many1 (char \+)) (many1 (char \-)) (char \=) (always nil))
               value (many (digit))]
              (if (and (empty? sign) (empty? value))
                (never)
                (param 
                  (let [value (or value 0)]
                    (cond
                      (or (nil? sign) (= (get sign 0) \+))
                      (+ old-value (count sign) value)
                      (= (get sign 0) \-)
                      (- old-value (count sign) value)
                      (= sign \=)
                      (if (nil? value) old-value value)))))))))

(defparser parse-ratio []
  (let->> [n (many (digit))]
          (choice
            (let->> [div (many1 (char \|))
                     d (many (digit))]
                    (/ (/ (if (empty? n) 1 (Integer/parseInt n)) 
                          (if (empty? d) 2 (Integer/parseInt d))) 
                       (long (Math/pow 2 (dec (count div))))))
            (let->> [_ (always nil)]
                    (if (empty? n) (never) (Integer/parseInt n))))))

(defparser parse-duration-letters []
  (let->> [s (many1 (choice (char \w) (char \h) (char \q) (char \e) (char \s) 
                            (char \t) (char \x) (char \o)))]
          (let [m {\w 1 \h 1/2 \q 1/4 \e 1/8 \s 1/16 \t 1/32 \x 1/64 \o 1/128}]
            (reduce + (map #(find (lower-case %) m) s)))))

(defn ratio-to-ticks [ratio]
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)
          divisionType (:division_type options)
          resolution (:resolution options)
          meter (:meter options)
          beats-per-measure (or (first meter) 4)
          beats-per-whole-note (or (second meter) 4)
          quarter-notes-per-beat (/ 4 beats-per-whole-note)
          duration-unit (or (:duration-unit options) 1)]
      (always (cond
                ;; duration-unit is fraction of a beat
                (= divisionType javax.sound.midi.Sequence/PPQ)
                  (* ratio duration-unit quarter-notes-per-beat resolution)
                ;; duration-unit is fraction of a second
                :else (* ratio duration-unit resolution))))))

(defparser parse-dotted-duration []
  (choice
    (let->> [ratio (choice (parse-ratio) (parse-duration-letters))
             dots (many (char \.))
             ticks (ratio-to-ticks (* ratio (- 2 (/ 1 (long (Math/pow 2 (count dots)))))))]
            ticks)
    (let->> [dots (many1 (char \.))
             ticks (ratio-to-ticks (* 1 (- 2 (/ 1 (long (Math/pow 2 (count dots)))))))]
            ticks)))

(defn timing-to-ticks [t1 t2 t3 t4]
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)
          divisionType (:division_type options)
          resolution (:resolution options)
          meter (:meter options)
          beats-per-measure (first meter)
          beats-per-whole-note (second meter)
          quarter-notes-per-beat (/ 4 beats-per-whole-note) ]
      (always (cond
                (= divisionType javax.sound.midi.Sequence/PPQ)
                  (let [m t2 b t3 t t4]
                    (+ (* m beats-per-measure quarter-notes-per-beat resolution)
                       (* b quarter-notes-per-beat resolution)
                       t))
                :else
                  (let [h t1 m t2 s t3 f t4]
                    (+ (* h 60 60 resolution)
                       (* m 60 resolution)
                       (* s resolution)
                       f)))))))

(defparser parse-timing []
  (let->> [t1 (many (digit))
           _ (char \')
           t2 (many (digit))]
           (choice 
             (let->> [_ (char \')
                      t3 (many (digit))]
                     (choice
                       (let->> [_ (char \')
                                t4 (many (digit))]
                               (timing-to-ticks (if (empty? t1) 0 (Integer/parseInt t1))
                                                (if (empty? t2) 0 (Integer/parseInt t2))
                                                (if (empty? t3) 0 (Integer/parseInt t3))
                                                (if (empty? t4) 0 (Integer/parseInt t4))))
                       (let->> [_ (always nil)]
                               (timing-to-ticks 0 
                                                (if (empty? t1) 0 (Integer/parseInt t1))
                                                (if (empty? t2) 0 (Integer/parseInt t2))
                                                (if (empty? t3) 0 (Integer/parseInt t3))))))
             (let->> [_ (always nil)]
                     (timing-to-ticks 0 0 (if (empty? t1) 0 (Integer/parseInt t1))
                                      (if (empty? t2) 0 (Integer/parseInt t2)))))))

;; shunting-yard algorithm: http://en.wikipedia.org/wiki/Shunting_yard_algorithm
(defparser parse-duration-expr []
  (let->> [value (choice (attempt (parse-dotted-duration)) 
                         (parse-timing))
           values (many (let->> [op (choice (char \+) (char \-) (char \*))
                                 value (choice (attempt (parse-dotted-duration)) 
                                               (parse-timing))]
                                (list (symbol op) value)))]
          (let [durexp (cons value (flatten values))
                opmap {'+ 0 '- 0 '* 1}]
              (loop [durexp durexp outq [] opstack []]
                (let [token (first durexp)]
                  (cond 
                    (empty? durexp)
                      (if (empty? opstack)
                        (last outq)
                        (recur durexp 
                               (conj (vector (drop 2 outq)) 
                                     (eval (cons (last opstack) (take 2 outq)))) 
                               (pop opstack)))
                    (char? token)
                      (if (and (not (empty? opstack)) 
                               (<= (find opmap token) (find opmap (last opstack))))
                        (recur durexp 
                               (conj (vector (drop 2 outq)) 
                                     (eval (cons (last opstack) (take 2 outq)))) 
                               (pop opstack))
                        (recur (rest durexp) outq (conj opstack token)))
                    (number? token)
                      (recur (rest durexp) (conj outq token) opstack)))))))

(defparser parse-duration []
  (let->> [_ (char \:)
           expr (parse-duration-expr)]
          expr))

(defparser parse-params [ch param]
  (let->> [op (many1 (char ch))
           value (choice (parse-value #({param %})) (always nil))]
          (let [old-value (or (find param options) 0)]
            {param 
             (if (nil? value)
               (* old-value (- 2 (/ 1 (long (Math/pow 2 (count op))))))
               (* value (- 2 (/ 1 (long (Math/pow 2 (dec (count op))))))))})))

(defparser parse-attack [] 
           (parse-params \! :attack))
(defparser parse-release [] 
           (parse-params \? :release))
(defparser parse-key-pressure [] 
           (parse-params \& :key-pressure))

(defparser parse-displacement [] 
  (choice (let->> [_ (char \<)
                  displacement (parse-duration-expr)]
                  {:displacement (- displacement)})
          (let->> [_ (char \>)
                  displacement (parse-duration-expr)]
                  {:displacement displacement})))

(defparser parse-track-channel []
  (let->> [_ (char \$)
           attrs (choice 
                   (let->> [_ (char \.)
                            channel (many1 (digit))]
                           {:channel channel})
                   (let->> [track (many1 (digit))
                            channel (choice 
                                      (let->>[_ (char \.) 
                                              channel (many (digit))]
                                              channel)
                                      (always nil))]
                           (if (nil? channel)
                             {:track track}
                             {:track track :channel channel})))]
          attrs))

(defparser parse-note-attrs []
  (let->> [octave (choice (parse-value #({:octave %})) (always nil))
           attrs (many (choice (let->> [duration (parse-duration)]
                                       {:duration duration})
                               (let->> [attack (parse-attack)]
                                       {:attack attack 
                                        :attack-on true})
                               (let->> [release (parse-release)]
                                       {:release release 
                                        :release-on true})
                               (let->> [key-pressure (parse-key-pressure)]
                                       {:key-pressure key-pressure 
                                        :key-pressure-on true})
                               (let->> [displacement (parse-displacement)]
                                       {:displacement displacement})
                               (let->> [track-channel (parse-track-channel)]
                                       {:track-channel track-channel})))]
          (apply merge (cons octave attrs))))

;; define note parsers
(doseq [note (keys notes)]
  (eval 
    `(defparser ~note [] 
        (play-note ~(name note) (parse-note-attrs)))))

;; rest parser
(defparser r []
  (let->> [duration (choice (parse-duration) (always nil))]
          (fn [options]
            (let [ticks (or (:ticks options) 0)
                  duration (or duration (:duration-unit options) 0)]
              {:ticks (+ ticks duration)}))))

(defparser parse-symbol [] 
  (let->> [l1 (choice (letter) (char \_))
           l2 (many (choice (letter) (digit) (char \_) (char \#)))]
          (let [symbol-name (replace-first (str l1 l2)  #"__\d+__auto__$" "#")
                symbol-var (second (find (ns-map *ns*) (symbol symbol-name)))]
            (if (nil? symbol-var)
              (never)
              (let [symbol-val (deref symbol-var)]
                (cond (fn? symbol-val)
                        (apply symbol-val)
                      (delay? symbol-val)
                        (force symbol-val)
                      :else (never)))))))

(defn parse-measure-bar []
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)
          divisionType (:division_type options)
          resolution (:resolution options)
          meter (:meter options)
          beats-per-measure (or (first meter) 4)
          beats-per-whole-note (or (second meter) 4)
          quarter-notes-per-beat 
            (if (= 0 beats-per-whole-note) 0 (/ 4 beats-per-whole-note))
          ticks (or (:ticks options) 0)
          ticks-per-measure (* beats-per-measure quarter-notes-per-beat resolution)
          last-meter-event (or (:last-meter-event options) 0)]
      (let->> [op (many1 (char \|))
               value (or (many (digit)) 0)]
              (if (= divisionType javax.sound.midi.Sequence/PPQ)
                {:ticks (+ ticks
                           (if (= 0 ticks-per-measure) 0 
                             (mod (- ticks last-meter-event) 
                                  ticks-per-measure))
                           (* ticks-per-measure (+ (dec (count op)) (dec value))))}
                nil)))))

(defn parse-token []
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)]
      (choice (parse-symbol)
              (parse-value (or (:value-fn options) #({:octave %})))
              (parse-duration)
              (parse-attack)
              (parse-release)
              (parse-key-pressure)
              (parse-displacement)
              (parse-measure-bar)
              (parse-track-channel)))))

;; play gets called on every tick event. Takes a playlist priority map
;; and an output function which receives MidiMessages
;; TODO: string, keyword, symbol, number -> (run (parse-token) ....)
;; delay, fn, map, handle metadata
(defn play [playlist out current-ticks buffer-ticks]
  (if (empty? (:queue playlist))
    playlist
    (let [buffer-ticks (or buffer-ticks 0)
          [[m _] ticks] (peek playlist)]
      (if (> (+ current-ticks buffer-ticks) ticks)
        playlist
        (let [container (if (or (vector? m) (seq? m)) m (list m))
              music (first container)
              next (if (vector? container) (vec (rest container)) (rest container))]
          (cond
           ;; symbols, etc.
           (string? music)
           (recur (assoc (pop playlist)
                    [(cons (run (parse-token) (seq music)) next) ticks] ticks)
                  out current-ticks buffer-ticks)
           (keyword? music)
           (recur (assoc (pop playlist)
                    [(cons (run (parse-token) (seq (str music))) next) ticks] ticks)
                  out current-ticks buffer-ticks)
           (symbol? music)
           (recur (assoc (pop playlist)
                    [(cons (run (parse-token) (seq (name music))) next) ticks] ticks)
                  out current-ticks buffer-ticks)
           (number? music)
           (recur (assoc (pop playlist)
                    [(cons (run (parse-token) (seq (str music))) next) ticks] ticks)
                  out current-ticks buffer-ticks)
           ;; functions, etc.
           (delay? music)
           (recur (assoc (pop playlist)
                    [(cons (force music (meta music)) next) ticks] ticks)
                  out current-ticks buffer-ticks)
           (fn? music)
           (recur (assoc (pop playlist)
                    [(cons (apply music (meta music)) next) ticks] ticks)
                  out current-ticks buffer-ticks)
           ;; convert map -> metadata
           ;; if map contains the :music key, convert it into music
           ;; and set its metadata to map
           (map? music)
           (recur (assoc (pop playlist)
                    [(if (contains? music :music)
                       (if (vector? container)
                         (vec (cons (with-meta (:music music) (dissoc music :music)) next))
                         (cons (with-meta (:music music) (dissoc music :music)) next))
                       (with-meta next (dissoc music :music))) ticks] ticks)
                  out current-ticks buffer-ticks)
           ;; rewrite the terms to factor non-seq/vector terms to the
           ;; first position. Make sure metadata gets propagated
           ;; through seqs/vectors.
           (and (vector? music) (not (empty? music)))
           (recur (assoc (pop playlist)
                    [(vec (cons (with-meta (first music) (merge (meta music) (meta (first music))))
                                (list (if (vector? container)
                                        (with-meta (vec (cons (vec (rest music)) next)) (merge (meta music) (meta (first music)))))
                                        (with-meta (cons (vec (rest music)) next) (merge (meta music) (meta (first music)))))))
                     ticks] ticks)
                  out current-ticks buffer-ticks)
           (and (seq? music) (not (empty? music)))
           (recur (assoc (pop playlist)
                    [(cons (with-meta (first music) (merge (meta music) (meta (first music))))
                           (list (if (vector? container)
                             (with-meta (vec (cons (rest music) next)) (merge (meta music) (meta (first music))))
                             (with-meta (cons (rest music) next) (merge (meta music) (meta (first music)))))))
                     ticks] ticks)
                  out current-ticks buffer-ticks)
           ;; Music records send themselves to the out function, then
           ;; queue the next part after duration.
           (instance? Music music)
           ;; handle displacement
           (if (and (:displacement (meta music)) (< current-ticks (+ ticks (:displacement (meta music)))))
             (recur (assoc (pop playlist)
                      [container (+ ticks (:displacement (meta music)))] (+ ticks (:displacement (meta music))))
                    out current-ticks buffer-ticks)
             (do (when (not (nil? (:music music)))
                   (out (:music music) (meta music)))
                 ;; If this came from a vector, set the duration to zero.
                 (let [duration (if (vector? container) 0 (:duration music))]
                   (recur (assoc (pop playlist)
                            [next (+ ticks duration)] (+ ticks duration))
                          out current-ticks buffer-ticks))))
           ;; skip anything else
           :else
           (recur (pop (:queue playlist)) out current-ticks buffer-ticks)))))))

