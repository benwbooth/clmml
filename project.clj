(defproject clmml "0.1.0-SNAPSHOT"
  :description "Clojure DSL for music programming"
  :java-source-path "src/java"
  ;;:disable-implicit-clean true
  :main clmml.core
  :repl-init clmml.core
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.priority-map "0.0.1"]
                 [the/parsatron "0.0.2"]
                 [overtone/midi-clj "0.4.0"]
                 [overtone/osc-clj "0.7.1"]])
