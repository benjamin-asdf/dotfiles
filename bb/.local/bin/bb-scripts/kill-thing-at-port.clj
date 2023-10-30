#!/usr/bin/env bb

(require
 '[clojure.java.shell :as shell])

(defn kill! [port]
  (run!
   #(shell/sh "kill" %)
   (str/split-lines
    (:out
     (shell/sh
      "lsof"
      (str "-i:" port)
      "-n"
      "-P"
      "-t")))))


(kill! (first *command-line-args*))
;; (kill! "8080")
(comment

  (shell/sh "lsof" (str "-i:" 40800) "-n" "-P" "-t")
  (shell/sh "lsof" (str "-i:" 33242) "-n" "-P" "-t")
  (shell/sh "lsof" (str "-i:" 8080) "-n" "-P" "-t")
  {:exit 0, :out "77563\n", :err ""}
  (kill! "8080"))
