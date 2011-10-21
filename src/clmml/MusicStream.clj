(ns clmml.MusicStream
  (:use [clmml.music-stream])
  (:gen-class
    :state music
    :init init
    :constructors {[java.lang.Object] []}
    :implements [clmml.SequenceAdvanceListener]))

;; music is a vector of lazy-seqs of music to play simultaneously
(defn -init [music]
  [] (ref music))

;; java interop function that gets called by Sequence whenever it is detected
;; that more sequence needs to be evaluated.
(defn -advance [this sequence-]
  (let [sequencer (.getSequencer sequence-)
        ticks (.getTickLength sequence-)
        target-ticks (if (nil? sequencer) 
                       ticks 
                       (+ (or (:buffer options) 100) 
                          (.getTickPosition sequencer)))]
    (dosync (alter (.music this) play 
                   (merge options {:ticks ticks
                                   :target-ticks target-ticks})))))

