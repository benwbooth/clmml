(ns clmml.test.core
  (:use [clmml.core] :reload)
  (:use [clojure.test]))

(deftest test-notes
  (let [music '(c d e f g)
        music-stream (MusicStream. music)
        sequence- (clmml/Sequence. PPQ 96 1)]
    (do (play music {:sequence sequence-})
      (MidiSystem/write(sequence- 1 )

    (is false "No tests have been written.")))
