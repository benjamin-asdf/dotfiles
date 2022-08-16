(in-package :stumpwm)

(set-font "-xos4-terminus-medium-r-normal-*-20-*-*-*-*-*-*-*")

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

(defcommand nyxt () ()
  (run-or-raise "nyxt" '(:class "Nyxt")))

;; todo make a group?
(define-key *top-map* (kbd "s-u") "browser")
(define-key *top-map* (kbd "s-n") "nyxt")

(defcommand slack () ()
  (run-or-raise "slack" '(:class "Slack")))
(define-key *top-map* (kbd "s-s") "slack")

(defcommand pull-emacs () ()
  (run-or-pull "emacs" '(:class "Emacs")))
(define-key *top-map* (kbd "s-E") "pull-emacs")

(define-key *top-map* (kbd "s-9")  "lock")
(defcommand mail () ()
  (window-send-string "Benjamin.Schwerdtner@gmail.com"))

(defcommand
    start-or-stop-recording
    ()
    ()
  (message
   (if (probe-file
	"/tmp/recordingpid")
       "stop recording"
       "select for recording"))
  (run-shell-command
   "video-selected"))

(defmacro def-just-a-shell-command
    (name script)
    "Def a command via `defcommand` and return the command name string."
  `(symbol-name
    (command-name
     (defcommand ,name () ()
       (run-shell-command ,script)))))

;; (defun def-shell-command-cmd (s))

(defvar *my-comma-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (kbd "m") "mail")
    (stumpwm:define-key m (kbd "r") "start-or-stop-recording")
    (stumpwm:define-key m (kbd "y")
      (def-just-a-shell-command emacs-kill-xselection "ec-kill-xselection"))
    (stumpwm:define-key m (kbd "c")
      (def-just-a-shell-command dunst-close-all "dunstctl close-all"))
    (stumpwm:define-key m (kbd "x")
      (def-just-a-shell-command dunst-close-all "kill-unity"))
    m))

(define-key *top-map* (kbd "s-,") '*my-comma-map*)

(setf *load-path* nil)
(init-load-path "~/.stumpwm.d/modules/")
(load-module "cpu")
(load-module "mem")
(setf mem::*mem-modeline-fmt* "MEM: %a %p %b")

(defun rec-modeline (ml)
  (if (probe-file
       "/tmp/recordingpid")
      "RECORDING"
      ""))

(add-screen-mode-line-formatter #\R 'rec-modeline)

(setf *screen-mode-line-format* "[^B%n^b] %C | %M  %R")

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

;; seriosly popping windows 10 times and then not fixing the fucking class
;; 0 regards for the user, just money money splash screens
(let ((lst
	'((:class "Unity-editor")
	  (:title "(Importing)")
	  (:title "Importing")
	  (:title "Hold on...")
	  (:title "Importing (iteration 2)")
	  (:title "Importing (iteration 3)")
	  (:title "Importing (iteration 4)")
	  (:title "Importing (iteration 5)")
	  (:title "Importing (iteration 6)"))))
  (setf *deny-map-request* (append *deny-map-request* lst))
  (setf *deny-raise-request* (append *deny-raise-request* lst)))


(defun class-windows (class group)
  (remove-if-not

   (lambda (w)
     (equal class (window-class w)))
   (group-windows group)))

(defcommand (pull-from-windowlist-curr-class tile-group)
    (&optional (fmt *window-format*)) (:rest)
  "Like `pull-from-windowlist` but only select
windows of the same class as the current window."
  (let* ((curr-class (window-class (current-window)))
	 (windows (class-windows curr-class (current-group)))
	 (pulled-window (select-window-from-menu
			 windows
                         fmt)))
    (when pulled-window
      (pull-window pulled-window))))


(defcommand (create-group-from-curr-class-windows tile-group) () ()
  "Create a group and put all current class windows there."
  (let* ((class (window-class (current-window)))
	 (windows (class-windows class (current-group)))
	 (group (add-group (current-screen) class)))
    (mapc (lambda (w) (move-window-to-group w group)) windows)
    (switch-to-group group)))

(define-key *top-map* (kbd "H-o") "pull-from-windowlist-curr-class")
(define-key *groups-map* (kbd "w") "create-group-from-curr-class-windows")

;; windowlist then go thought the same class wouuld be nice
;; also window list fitler same class


;; window hook or sth to put qutebro

;; I don't want to hit tab if there is only 1 thing in the
;; selection list

;; replace flameshot maybe
;; no drawing stuff though

(defmacro comment (&rest _))

(comment

 (create-group-from-curr-class-windows)
 (group-indicate-focus (current-group))
 (group-sync-all-heads (current-group))

 (message "foo                                       fofoo")
 (setf *urgent-window-hook* nil)
 (load-module "screenshot")
 (screen-windows (current-screen))
 (setf *deny-raise-request* nil *deny-map-request* nil)
 (setf *debug-level* 0)
 (redirect-all-output (data-dir-file "output" "log"))
 (equal *module-dir* (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/modules")))
 (list-modules)
 (setf *honor-window-moves* nil)
 (defmethod group-move-request ((group tile-group) (window tile-window) x y relative-to)
   (when *honor-window-moves*
     (dformat 3 "Window requested new position ~D,~D relative to ~S~%" x y relative-to)
     (let* ((pointer-pos (multiple-value-list (xlib:global-pointer-position *display*)))
            (pos  (if (eq relative-to :parent)
		      (list
		       (+ (xlib:drawable-x (window-parent window)) x)
		       (+ (xlib:drawable-y (window-parent window)) y))
		      (list (first pointer-pos) (second pointer-pos))))
            (frame (apply #'find-frame group pos)))
       (when frame
         (pull-window window frame)))))

 (load-module "wifi")
 (setf
  *screen-mode-line-format*
  "[^B%n^b] %C | %M  %R %I")
 (load-module "battery-portable")

 (setf
  *screen-mode-line-format*
  "[^B%n^b] %C | %M  %R %B")

 (mapcar #'window-title (group-windows (current-group)))

 (mapcar
  (lambda (w)
    `(:id ,(window-id w) :title ,(window-title w)))
  (group-windows (current-group)))

 (pull-window (window-by-id 23073195))



 )
