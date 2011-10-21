(defproject clmml "0.1.0-SNAPSHOT"
  :description "Clojure DSL for music programming"
  :java-source-path "src/java"
  :disable-implicit-clean true
  :aot [clmml.MusicStream]
  :dependencies [[org.clojure/clojure "1.3.0"]
                 ;[the/parsatron "0.0.2"]
                 [overtone/midi-clj "0.2.1"]
                 [overtone/osc-clj "0.7.1"]]
)
