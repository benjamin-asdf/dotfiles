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

;; todo make a group
(define-key *top-map* (kbd "s-u") "browser")

(defcommand slack () ()
  (run-or-raise "slack" '(:class "Slack")))
(define-key *top-map* (kbd "s-s") "slack")

(defcommand pull-emacs () ()
  (run-or-pull "emacs" '(:class "Emacs")))
(define-key *top-map* (kbd "s-E") "pull-emacs")

(define-key *top-map* (kbd "s-9")  "lock")
(defcommand mail () ()
  (window-send-string "Benjamin.Schwerdtner@gmail.com"))

(defvar *my-comma-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (kbd "m") "mail")
    m))

(define-key *top-map* (kbd "s-,") '*my-comma-map*)

 (setf *load-path* nil)
 (init-load-path "/home/benj/.stumpwm.d/modules/")
(load-module "cpu")

(setf
 *screen-mode-line-format*
 "[^B%n^b] %C")

(defcommand lock () ()
  (run-shell-command "best-lock.sh"))

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

(defun select-windows-with-class ())


(defcommand (pull-from-windowlist-curr-class tile-group)
    (&optional (fmt *window-format*)) (:rest)
  "Like `pull-from-windowlist` but only select
windows of the same class as the current window."
  (let* ((curr-class (window-class (current-window)))
	 (windows (remove-if-not
		   (lambda (w)
		     (equal curr-class
			    (window-class w)))
		   (group-windows (current-group))))
	 (pulled-window (select-window-from-menu
			 windows
                         fmt)))
    (when pulled-window
      (pull-window pulled-window))))

(define-key *top-map* (kbd "H-o") "pull-from-windowlist-curr-class")

(defmacro comment (&rest body))

;; windowlist then go thought the same class wouuld be nice
;; also window list fitler same class


;; window hook or sth to put qutebro

;; I don't want to hit tab if there is only 1 thing in the
;; selection list

;; replace flameshot maybe
;; no drawing stuff though

;; 1-2 things for make vid

(comment
 (load-module "screenshot")
 (screen-windows (current-screen))
 (setf *deny-raise-request* nil *deny-map-request* nil)
 (setf *debug-level* 1)
 (redirect-all-output (data-dir-file "output" "log"))
 (equal *module-dir* (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/modules")))
 (list-modules))
