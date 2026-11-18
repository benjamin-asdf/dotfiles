;; record.lisp -- StumpWM front end for the `record' script.
;; One verb, a target per key; lowercase = no audio, uppercase = mic +
;; system audio. Pressing any of them while recording stops it.

(in-package :stumpwm)

(defcommand record (args) ((:rest "record args: "))
  "Run `record ARGS' asynchronously (starts, or stops a running recording)."
  (run-shell-command (format nil "record ~a" (or args ""))))

(defparameter *record-map*
  (let ((m (make-sparse-keymap)))
    (loop for (key target) in '(("r" "region") ("w" "window")
                                ("m" "monitor") ("f" "full"))
          do (define-key m (kbd key) (format nil "record ~a" target))
             (define-key m (kbd (string-upcase key))
               (format nil "record ~a -a both" target)))
    m))

;; The original s-, r command: toggle a recording of a selected region.
(defcommand start-or-stop-recording () ()
  (message (if (probe-file "/tmp/recordingpid")
               "stop recording"
               "select for recording"))
  (run-shell-command "record region"))
