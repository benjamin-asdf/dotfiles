#!/usr/bin/env bb

(require
 '[clojure.java.shell :as shell])




(run!
 #(shell/sh "kill" %)
 (str/split-lines
  (:out
   (shell/sh
    "lsof"
    (str "-i:" (first *command-line-args*))
    "-n"
    "-P"
    "-t"))))

(comment

(shell/sh "lsof" (str "-i:" 40800) "-n" "-P" "-t")
(shell/sh "lsof" (str "-i:" 33242) "-n" "-P" "-t")


  )
