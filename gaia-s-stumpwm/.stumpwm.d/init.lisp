(in-package :stumpwm)

(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

(defcommand swank () ()
  "Turn on the swank server the first time.
Load a file that re-defines swank and then calls it."
  ;; Be careful with the quotes!
  (load
   "~/.stumpwm.d/swank.lisp")
  (echo-string
   (current-screen)
   "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))

(define-key *root-map* (kbd "C-s") "swank")

(bind "k" "move-focus up")
(bind "j" "move-focus down")
(bind "l" "move-focus right")
(bind "h" "move-focus left")
(bind "d" "kill")

(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-w") "fselect")
(define-key *top-map* (kbd "s-d") "delete-window")
(define-key *top-map* (kbd "s-o") "only")
(define-key *top-map* (kbd "s-e") "emacs")

(defcommand flameshot-gui () ()
  (run-shell-command "flameshot gui"))
(define-key *top-map* (kbd "s-p") "flameshot-gui")

(defcommand browser () ()
  (run-or-raise "qutebrowser" '(:class "qutebrowser")))
;; make a group
(define-key *top-map* (kbd "s-u") "browser")

(defcommand lock () ()
  (run-shell-command "best-lock.sh"))
(define-key *top-map* (kbd "s-9")  "lock")
