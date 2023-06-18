#!/bin/bb

(ns bb_print_deps_or_alias
  (:require
   [clojure.java.shell :as shell]
   [clojure.pprint :as pp]
   [babashka.fs :as fs]))

(let [file (fs/file "deps.edn")
      deps (-> (shell/sh "bb" "print-deps") :out read-string)
      deps
      (if-not
          (fs/exists? file)
          deps
          (update-in
           (read-string (slurp file))
           [:aliases :bb :extra-deps]
           merge
           (:deps deps)))]
  (spit file (with-out-str (pp/pprint deps))))
