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
(bind "C-h" '*help-map*)

(setf  *help-keys* '("?" "H-h"))

(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-w") "fselect")
(define-key *top-map* (kbd "s-d") "delete-window")
(define-key *top-map* (kbd "s-o") "only")
(define-key *top-map* (kbd "s-e") "emacs")
(define-key *top-map* (kbd "s-m") "mode-line")
(define-key *top-map* (kbd "s-i") "pull-from-windowlist")
(define-key *top-map* (kbd "s-g") '*groups-map*)

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

(Load-module "cpu")

(setf
 *screen-mode-line-format*
 "[^B%n^b] %C")

(load-module "pass")
(define-key *top-map* (kbd "s-a") "pass-copy")

(defcommand kill-unclutter () ()
  (run-shell-command "pkill unclutter"))
(defcommand start-unclutter () ()
  (run-shell-command "unclutter &"))

(define-interactive-keymap
    normie-mode
    (:on-enter #'kill-unclutter
     :on-exit #'start-unclutter)
  ((kbd "J") "ratrelwarp  0 +5")
  ((kbd "j") "ratrelwarp  0 +30")
  ((kbd "K") "ratrelwarp  0 -5")
  ((kbd "k") "ratrelwarp  0 -30")
  ((kbd "H") "ratrelwarp -5  0")
  ((kbd "h") "ratrelwarp -30  0")
  ((kbd "L") "ratrelwarp +5  0")
  ((kbd "l") "ratrelwarp +30  0")
  ((kbd "d") "ratclick 1")
  ((kbd "c") "ratclick 3"))

(define-key *top-map* (kbd "s-;") "normie-mode")


(push '(:class "Unity-editor") *deny-raise-request*)
(push '(:class "Unity-editor") *deny-map-request*)

(defmacro comment (&rest body))

;; windowlist then go thought the same class wouuld be nice



(comment
 (setf *debug-level* 1)
 (redirect-all-output (data-dir-file "output" "log")))
