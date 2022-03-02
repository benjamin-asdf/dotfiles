#!/usr/bin/env bb

(require
 '[clojure.java.shell :as shell])

(shell/sh
   "kill"
   (str/trim
    (:out
     (shell/sh
      "lsof"
      (str "-i:" (first *command-line-args*))
      "-n"
      "-P"
      "-t"))))
