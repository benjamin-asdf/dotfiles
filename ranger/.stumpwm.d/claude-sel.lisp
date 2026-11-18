;; claude-sel: run Claude "verbs" on the X selection.
;;
;; The verbs live in ~/.config/claude-sel/prompts (stow package `scripts`,
;; file scripts/.config/claude-sel/prompts); the work is done by the
;; `claude-sel` script (scripts/.local/bin/claude-sel).
;;
;; *claude-sel-map* (bind it under some prefix, e.g. s-, c):
;;   <letter>  verb from the prompts file, input PRIMARY (fallback CLIPBOARD),
;;             result -> CLIPBOARD
;;   SPC       dmenu picker over all verbs
;;   C-v       sub-map: same keys, but paste the result back (replace selection)
;;   C-e       sub-map: same keys, but show the result in Emacs (*claude-sel*)
;;   C-c       sub-map: same keys, but read CLIPBOARD (e.g. Wispr Flow dictation);
;;             it has its own C-v / C-e
;;
;; After editing the prompts file run the command `claude-sel-reload`.

(in-package :stumpwm)

(defvar *claude-sel-prompts-file*
  (merge-pathnames ".config/claude-sel/prompts" (user-homedir-pathname)))

(defun claude-sel-read-prompts ()
  "Return a list of (KEY . NAME) from the prompts file, skipping keyless (\"-\") ones."
  (with-open-file (in *claude-sel-prompts-file* :if-does-not-exist nil)
    (when in
      (loop for line = (read-line in nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab) line)
            for fields = (remove "" (uiop:split-string trimmed :separator '(#\Space #\Tab))
                                 :test #'string=)
            when (and (>= (length fields) 2)
                      (char/= (char trimmed 0) #\#)
                      (string/= (first fields) "-"))
              collect (cons (first fields) (second fields))))))

(defcommand claude-sel (name &optional (input "auto") (output "clipboard"))
    ((:string "Verb: ") (:string "Input (auto|primary|clipboard): ")
     (:string "Output (clipboard|paste|emacs): "))
  "Run claude-sel NAME asynchronously on the current head."
  (run-shell-command
   (format nil "claude-sel -m ~d -i ~a -o ~a ~a"
           (head-number (current-head)) input output name)))

(defun claude-sel-make-map (input output &optional (submaps t))
  (let ((m (make-sparse-keymap)))
    (loop for (key . name) in (claude-sel-read-prompts)
          do (define-key m (kbd key) (format nil "claude-sel ~a ~a ~a" name input output)))
    (define-key m (kbd "SPC") (format nil "claude-sel pick ~a ~a" input output))
    (when submaps
      (define-key m (kbd "C-v") (claude-sel-make-map input "paste" nil))
      (define-key m (kbd "C-e") (claude-sel-make-map input "emacs" nil))
      (when (string= input "auto")
        (define-key m (kbd "C-c") (claude-sel-make-map "clipboard" output t))))
    m))

(defparameter *claude-sel-map* (claude-sel-make-map "auto" "clipboard"))

(defcommand claude-sel-reload () ()
  "Rebuild *claude-sel-map* from the prompts file."
  (setf *claude-sel-map* (claude-sel-make-map "auto" "clipboard"))
  (message "claude-sel: ~d verbs" (length (claude-sel-read-prompts))))
