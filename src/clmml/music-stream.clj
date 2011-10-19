;; TODO:
;;  key signatures
;;  time signatures
;;  chords
;;  cc's
;;  pitch wheel
;;  program change

(ns aria
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(def options 
  {:sequence nil ; sequence object
   :track 0 ; track number
   :channel 0 ; channel number
   :meter [4 4] ; meter [beats-per-measure beats-per-whole-note &
                ;        ticks-per-metronome-click 
                ;        notated-32nd-notes-per-quarter-note]
   :last-meter-event 0 ; tick value of last meter event
   :buffer 1000 ; buffer size in ticks--necessary for negative note displacement
   :ticks 0 ; current tick value
   :target-ticks 0 ; stop generaing MIDI events after target-ticks is reached
   :value 4 ; default assignment value
   :value-fn #({:value %}) ; callback function for when a value is assigned
   :displacement 0 ; note start time displacement in +/- ticks
   :duration 0 ; default note duration amount in ticks
   :duration-unit 0 ; duration unit size in ticks (ratios are multiplied by this)
   :attack 64 ; default attack value (0-127)
   :release 64 ; default release value (0-127)
   :key-pressure 64 ; default key-pressure value (0-127)
   })

(gen-class
   :name clmml.MusicStream
   :state music
   :init init
   :prefix "MusicStream-"
   :constructors {[java.lang.Object] []}
   :implements [clmml.SequenceAdvanceListener])

;; music is a vector of lazy-seqs of music to play simultaneously
(defn MusicStream-init [music]
  [] (ref music))

;; java interop function that gets called by Sequence whenever it is detected
;; that more sequence needs to be evaluated.
(defn MusicStream-advance [this sequence-]
  (let [ticks (.getTickLength sequence-)
        sequencer (.getSequencer sequence-)
        target-ticks (if (nil? sequencer) 
                       ticks 
                       (+ (or (:buffer options) 1000) 
                          (.getTickPosition sequencer)))]
    (dosync (alter (.music this) play 
                   (merge options {:ticks ticks
                                   :target-ticks target-ticks})))))

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
    f 5   f# 6   f## 7   fb 4   fbb 3   fN 5  
    g 7   g# 8   g## 9   gb 6   gbb 5   gn 7  
    a 9   a# 10  a## 11  ab 8   abb 7   an 9  
    b 11  b# 12  b## 13  bb 10  bbb 9   bn 11 
  )))

;; convert note/octave value to MIDI key number
(defn note-value [note attrs]
  (let [octave (max -1 (min 9 (or (:value attrs) 4)))
        note-val (find notes (symbol note))]  
    (max 0 (min 127 (+ (* 12 (inc octave)) note-val)))))

(defn play-note [note attrs]
  (fn [options]
    (let [attrs (merge options attrs)
          value (note-value note attrs)
          ticks (or (:ticks attrs) 0)
          begin-ticks (+ ticks (or (:displacement attrs) 0))
          end-ticks (+ ticks (or (:duration attrs) 0))
          note-on [(bit-or javax.sound.midi.ShortMessage/NOTE_ON 
                              (max 0 (min 15 (:channel attrs))))
                      value
                      (max 0 (min 127 (:attack attrs)))]
          note-on-event (MidiEvent. (MidiMessage. 
                                      (byte-array (seq note-on))
                                      (count note-on)) begin-ticks)
          key-pressure [(bit-or javax.sound.midi.ShortMessage/POLY_PRESSURE 
                                     (max 0 (min 15 (:channel attrs))))
                             value
                             (max 0 (min 127 (:key-pressure attrs)))]
          key-pressure-event (MidiEvent. (MidiMessage. 
                                           (byte-array (seq key-pressure))
                                           (count key-pressure)) begin-ticks)
          note-off [(bit-or javax.sound.midi.ShortMessage/NOTE_OFF 
                               (max 0 (min 15 (:channel attrs))))
                       value
                       (max 0 (min 127 (:release attrs)))]
          note-off-event (MidiEvent. (MidiMessage. 
                                           (byte-array (seq note-off))
                                           (count note-off)) end-ticks)]
      (if (contains? attrs :key-pressure-on)
        (if (and (not (contains? attrs :attack-on))
                 (not (contains? attrs :release-on)))
          (list key-pressure-event {:ticks end-ticks})
          (list note-on-event key-pressure-event note-off-event))
        (list note-on-event note-off-event)))))

(defparser parse-note-attrs []
  (let->> [value (let->> [value (choice (parse-value #({:value %})) (always nil))]
                         (if (nil? value) {} {:value value}))
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
          (apply merge (cons value attrs))))

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

;; allowable characters in clojure symbols:
;; a1!#$&*_-=+|:<>'?/.
;; / must be followed by a non-number
;; : cannot be followed by another :
;; # not followed by ' or _, not at beginning
;; ' cannot be at the beginning

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
           (choice (let->> [displacement (parse-params \< :displacement)]
                           {:displacement (- (:displacement displacement))}
                   (parse-params \> :displacement))))

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

(defn parse-measure-bar []
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)
          sequence- (:sequence options)
          divisionType (.getDivisionType sequence-)
          resolution (.getResolution sequence-)
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

;; shunting-yard algorithm: http://en.wikipedia.org/wiki/Shunting_yard_algorithm
(defparser parse-duration []
  (let->> [_ (char \:)
           value (choice (attempt (parse-dotted-duration)) 
                         (parse-timing))
           values (many (let->> [op (choice (char \+) (char \-) (char \*))
                                 value (choice (attempt (parse-dotted-duration)) 
                                               (parse-timing))]
                                (list (symbol op) value)))]
          (let [durexp (cons value (flatten values))
                opmap {'+ 0 '- 0 '* 1}]
            (fn [sequence- ticks options]
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
                      (recur (rest durexp) (conj outq token) opstack))))))))

(defn ratio-to-ticks [ratio]
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)
          sequence- (:sequence options)
          divisionType (.getDivisionType sequence-)
          resolution (.getResolution sequence-)
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

(defparser parse-duration-letters []
  (let->> [s (many1 (choice (char \w) (char \h) (char \q) (char \e) (char \s) 
                            (char \t) (char \x) (char \o)))]
          (let [m {\w 1 \h 1/2 \q 1/4 \e 1/8 \s 1/16 \t 1/32 \x 1/64 \o 1/128}]
            (reduce + (map #(find (lower-case %) m) s)))))

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

(defn timing-to-ticks [t1 t2 t3 t4]
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)
          sequence- (:sequence options)
          divisionType (.getDivisionType sequence-)
          resolution (.getResolution sequence-)
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

(defn parse-token []
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (let [options (meta input)]
      (choice (parse-symbol)
              (parse-value (or (:value-fn options) #({:value %})))
              (parse-duration)
              (parse-attack)
              (parse-release)
              (parse-key-pressure)
              (parse-displacement)
              (parse-measure-bar)
              (parse-track-channel)))))

;; play the music to the sequence for ticks time
;; then return the updated music
;; TODO: avoid stack overflows
(defn play [music options]
  (let [options 
        (if (instance? clojure.lang.IMeta music) 
          (merge options (meta music)) 
          options)
        sequence- (:sequence options)
        ticks (or (:ticks options) 0)
        target-ticks (or (:target-ticks options) 0)
        track (or (:track options) 0)]
    (cond 
      ;; we've already processed up until target-ticks, just return the music
      ;; without processing it.
      (>= ticks target-ticks) 
        music
      ;; convert MidiMessages to MidiEvents
      (instance? javax.sound.midi.MidiMessage music)
        (do (when (not (nil? sequence-)) 
              (let [tracks (.getTracks sequence-)
                    num-tracks (count tracks)]
                (when (> num-tracks 0)
                  (.add (aget tracks (max 0 (min num-tracks track))) 
                        (javax.sound.midi.MidiEvent. music ticks)))))
          nil)
      ;; MidiEvents get passed through directly
      (instance? javax.sound.midi.MidiEvent music)
        (recur (list {:ticks (.getTick music)} (.getMessage music)) options)
      ;; parse tokens
      (string? music) 
        (recur (run (parse-token) (with-meta (seq music) options)) options)
      (keyword? music) 
        (recur (run (parse-token) (with-meta (seq (str music)) options)) options)
      (symbol? music) 
        (recur (run (parse-token) (with-meta (seq (name music)) options)) options)
      (number? music) 
        (recur (run (parse-token) (with-meta (seq (str music)) options)) options)
      ;; functions which update the sequence and/or return music
      (delay? music) (recur (force music (merge options (meta music))) 
                            (merge options (meta music)))
      (fn? music) (recur (apply music (merge options (meta music))) 
                         (merge options (meta music)))
      ;; vectors play their contents in parallel
      (vector? music)
        (if (empty? music)
          nil
          (let [music (loop [music music 
                            options options
                            outmusic []]
                        (let [m (first music)
                              ms (rest music)]
                          (if (map? m)
                            ;; update state and continue
                            (recur ms (merge options m) (conj outmusic m))
                            ;; play each value in the vector, overriding ticks
                            ;; with the current tick value
                            (let [result (play m (merge options {:ticks ticks}))]
                              (if (empty? ms)
                                (conj outmusic result)
                                (recur ms options (conj outmusic result)))))))]
            (if (every? map? music)
              ;; merge finished vectors into a single map
              {:ticks (or (:ticks (apply merge music)) ticks)}
              ;; return unfinished vector
              (with-meta music options))))
      ;; seqs play their contents in sequence
      (seq? music)
        (if (empty? music)
          {:ticks ticks}
          (let [m (first music)
                ms (rest music)]
            (if (map? m)
              ;; update state and continue
              (recur ms (merge options m))
              (let [result (play m options)]
                (cond
                  ;; update state and continue
                  (map? result) (recur ms (merge options result))
                  ;; unfinished seq - just return
                  (seq? result) (with-meta (cons result ms) options)
                  ;; unfinished vector - keep it and attempt to continue
                  ;; processing this seq
                  (vector? result) 
                    ;; last map in vector 
                    (let [opt (if (map? (last result)) (last result) nil)]
                      (if (nil? opt)
                        ;; last element of vector unfinished, no need to continue
                        (with-meta (cons result ms) options)
                        ;; attempt to continue processing this seq
                        ;; FIXME: this might overflow the stack!
                        (let [more (play ms (merge options opt))]
                          (with-meta (cons result (if (seq? more) more (list more))) options))))
                  ;; unrecognized element - skip
                  (nil? result) (recur ms options)
                  ;; MidiMessage/Event - not ready yet, do not continue
                  :else (with-meta (cons result ms) options))))))
      ;; anything else is ignored
      :else nil)))

