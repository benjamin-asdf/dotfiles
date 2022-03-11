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
;;
(define-key *root-map* (kbd "C-s") "swank")

(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "l") "move-focus left")
(define-key *root-map* (kbd "h") "move-focus right")
(define-key *root-map* (kbd "d") "kill")
