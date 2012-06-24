(defproject clmml "0.1.0-SNAPSHOT"
  :description "Clojure DSL for music programming"
  :java-source-path "src/java"
  ;;:disable-implicit-clean true
  :main clmml.core
  :repl-init clmml.core
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [the/parsatron "0.0.2"]
                 [org.clojure/core.match "0.2.0-alpha9"]
                 [overtone/midi-clj "0.4.0"]
                 [overtone/osc-clj "0.7.1"]])
