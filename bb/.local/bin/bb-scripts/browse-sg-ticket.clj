#!/usr/bin/env bb

(require '[clojure.string :as str])
(require '[clojure.java.shell :as shell])

(shell/sh
 "xdg-open"
 (str
  "https://singularitygroup.atlassian.net/browse/"
  (->
   *command-line-args*
   first
   (str/split #"/")
   last)))
