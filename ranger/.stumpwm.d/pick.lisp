;; pick.lisp -- StumpWM front end for `pick' (sources x verbs) and
;; `act-selection' (Embark for X). The contract lives at the top of the
;; `pick' script (scripts/.local/bin/pick); sources and verbs are files in
;; ~/.config/pick/{sources,verbs}/ (stow package `scripts').
;;
;; *pick-map*:
;;   <letter>  pick from the source whose header says "# key: <letter>"
;;   SPC       dmenu over all sources
;;   .         act on the selection (PRIMARY, fallback CLIPBOARD)
;;   ,         act on CLIPBOARD
;;
;; After adding a source (or changing a key) run `pick-reload'.
;; Everything runs asynchronously; dmenu opens on the current head.

(in-package :stumpwm)

(defvar *pick-sources-dir*
  (merge-pathnames ".config/pick/sources/" (user-homedir-pathname)))

(defun pick-shell-quote (string)
  (format nil "'~a'"
          (with-output-to-string (out)
            (loop for c across string
                  do (if (char= c #\')
                         (write-string "'\\''" out)
                         (write-char c out))))))

(defun pick-run (program &rest args)
  "Run PROGRAM -m <current head> ARGS... without waiting."
  (run-shell-command
   (format nil "~a -m ~d~{ ~a~}" program (head-number (current-head))
           (mapcar #'pick-shell-quote (remove nil args)))))

(defcommand pick (&optional source verb) ((:string) (:string))
  "Pick from SOURCE (dmenu over sources when omitted), then act with VERB
(dmenu over the applicable verbs when omitted)."
  (pick-run "pick" source verb))

(defcommand act-selection (&optional verb) ((:string))
  "Embark for X: offer the verbs applicable to PRIMARY (fallback CLIPBOARD)."
  (pick-run "act-selection" verb))

(defcommand act-clipboard (&optional verb) ((:string))
  "Like act-selection, but read CLIPBOARD."
  (pick-run "act-selection" "-c" verb))

(defun pick-source-keys ()
  "List of (KEY . SOURCE) from the \"# key:\" headers of the source files."
  (loop for file in (ignore-errors (uiop:directory-files *pick-sources-dir*))
        for name = (file-namestring file)
        for key = (ignore-errors
                   (with-open-file (in file)
                     (loop repeat 15
                           for line = (read-line in nil)
                           while line
                           when (uiop:string-prefix-p "# key:" line)
                             return (string-trim " " (subseq line 6)))))
        when (and key (plusp (length key)))
          collect (cons key name)))

(defun pick-make-map ()
  (let ((m (make-sparse-keymap)))
    (loop for (key . source) in (pick-source-keys)
          do (define-key m (kbd key) (format nil "pick ~a" source)))
    (define-key m (kbd "SPC") "pick")
    (define-key m (kbd ".") "act-selection")
    (define-key m (kbd ",") "act-clipboard")
    m))

(defparameter *pick-map* (pick-make-map))

(defcommand pick-reload () ()
  "Rebuild *pick-map* from the source headers."
  (setf *pick-map* (pick-make-map))
  (message "pick: ~{~a ~a~^, ~}"
           (loop for (k . s) in (pick-source-keys) collect k collect s)))
