#!/usr/bin/env bb
;; palette.bb -- keep the copies of the mememacs palette in sync.
;;
;;   bb palette.bb            rewrite drifted colors in the target files
;;   bb palette.bb --check    report drift, exit 1 when there is any
;;
;; Source of truth: the (defconst mememacs-visuals/NAME "#rrggbb") forms in
;; mememacs/.emacs-mememacs.d/lisp/visual.el.
;;
;; A target file tags its colors with a comment (any comment syntax):
;;
;;     palette: NAME NAME ...
;;
;; The next #rrggbb literals in that file, one per name and in that order, are
;; that color. `_' skips a literal that is not from the palette. Colors are
;; compared ignoring case, so a file is only touched on real drift.
;;
;; Files that cannot keep comments (flameshot rewrites its ini) are listed in
;; `untagged' with a regex whose first group is the color.

(ns palette
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def root (.getParentFile (io/file (System/getProperty "babashka.file"))))

(def source "mememacs/.emacs-mememacs.d/lisp/visual.el")

(def targets
  ["ranger/.stumpwm.d/init.lisp"
   "ranger/.Xresources"
   "qutebrowser/.config/qutebrowser/config.py"
   "scripts/.local/bin/pathoverride/dmenu"
   "dunst/.config/dunst/dunstrc"])

(def untagged
  [{:file "flameshot/.config/flameshot/flameshot.ini"
    :re #"(?m)^uiColor=(#[0-9a-fA-F]{6})"
    :color "heliotrope"}])

(defn palette []
  (into {}
        (map (fn [[_ color-name hex]] [color-name hex]))
        (re-seq #"\(defconst mememacs-visuals/([a-z0-9-]+)\s+\"(#[0-9a-fA-F]{6})\""
                (slurp (io/file root source)))))

(def token-re
  #"palette:[ \t]+([a-z0-9_-]+(?:[ \t]+[a-z0-9_-]+)*)[ \t]*$|#[0-9a-fA-F]{6}\b")

(defn synced
  "Walks TEXT and returns {:text new-text :drift [...] :errors [...]}."
  [colors file text]
  (let [m (re-matcher (re-pattern (str "(?m)" token-re)) text)]
    (loop [pending [] out (StringBuilder.) pos 0 drift [] errors []]
      (if-not (.find m)
        {:text (str (.append out (subs text pos)))
         :drift drift
         :errors (cond-> errors
                   (seq pending)
                   (conj (str file ": no color literal left for " (str/join " " pending))))}
        (let [names (.group m 1)
              found (.group m)
              out (.append out (subs text pos (.start m)))]
          (if names
            (recur (str/split names #"[ \t]+") (.append out found) (.end m) drift
                   (cond-> errors
                     (seq pending)
                     (conj (str file ": tag ended before " (str/join " " pending)))))
            (let [color-name (first pending)
                  want (get colors color-name)]
              (cond
                (or (nil? color-name) (= "_" color-name))
                (recur (vec (rest pending)) (.append out found) (.end m) drift errors)

                (nil? want)
                (recur (vec (rest pending)) (.append out found) (.end m) drift
                       (conj errors (str file ": unknown color " color-name)))

                (= (str/lower-case want) (str/lower-case found))
                (recur (vec (rest pending)) (.append out found) (.end m) drift errors)

                :else
                (recur (vec (rest pending)) (.append out want) (.end m)
                       (conj drift (str file ": " color-name " " found " -> " want))
                       errors)))))))))

(defn synced-untagged [colors {:keys [file re color]} text]
  (let [want (get colors color)
        [_ found] (re-find re text)]
    (cond
      (nil? found) {:text text :drift [] :errors [(str file ": pattern not found")]}
      (= (str/lower-case want) (str/lower-case found)) {:text text :drift [] :errors []}
      :else {:text (str/replace-first text found want)
             :drift [(str file ": " color " " found " -> " want)]
             :errors []})))

(defn -main [& args]
  (let [check? (some #{"--check"} args)
        colors (palette)
        results (concat
                 (for [file targets
                       :let [text (slurp (io/file root file))]]
                   (assoc (synced colors file text) :file file :old text))
                 (for [{:keys [file] :as target} untagged
                       :let [text (slurp (io/file root file))]]
                   (assoc (synced-untagged colors target text) :file file :old text)))
        drift (mapcat :drift results)
        errors (mapcat :errors results)]
    (run! println errors)
    (run! println drift)
    (when-not check?
      (doseq [{:keys [file text old]} results
              :when (not= text old)]
        (spit (io/file root file) text)))
    (println (format "palette: %d colors, %d files, %d drifted%s, %d errors"
                     (count colors) (count results) (count drift)
                     (if (and (seq drift) (not check?)) " (fixed)" "")
                     (count errors)))
    (when (or (seq errors) (and check? (seq drift)))
      (System/exit 1))))

(apply -main *command-line-args*)
