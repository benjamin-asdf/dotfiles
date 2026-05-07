(in-package :stumpwm)


(defmacro comment (&rest _)
  (declare (ignore _)))

(set-font "-xos4-terminus-medium-r-normal-*-22-*-*-*-*-*-*-*")
(defvar mm/avy-keys "adfjklophgb")

(setf *frame-number-map* mm/avy-keys
      *group-number-map* mm/avy-keys
      *window-number-map* mm/avy-keys)

(bind "k" "move-window up")
(bind "j" "move-window down")
(bind "l" "move-window right")

(bind "h" "move-window left")
(bind "M-k" "exchange-direction up")
(bind "M-j" "exchange-direction down")
(bind "M-l" "exchange-direction right")

(bind "M-h" "exchange-direction left")
(bind "d" "kill")

(bind "C-h" '*help-map*)

(setf  *help-keys* '("?" "H-h"))

;; todo when emacs thee maximize
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-w") "fselect")
(define-key *top-map* (kbd "s-d") "delete-window")
(define-key *top-map* (kbd "s-o") "only")



(define-key *top-map* (kbd "s-e") "emacs")
(define-key *top-map* (kbd "s-m") "mode-line")
(define-key *top-map* (kbd "s-i") "pull-from-windowlist")

(define-key *top-map* (kbd "s-g") '*groups-map*)

(defcommand flameshot-gui () ()
  ;; (run-shell-command "flameshot gui")
  (run-shell-command "flameshot-clipboard"))

(define-key *top-map* (kbd "s-p") "flameshot-gui")

(defcommand browser () ()
  (if
   (equal "" (run-shell-command "pgrep qutebrowser" t))
   (run-or-raise "qutebrowser" '(:class "qutebrowser"))
   (run-shell-command "qute-window")))

(defcommand nyxt () ()
  (run-or-raise "nyxt" '(:class "Nyxt")))
;; todo make a group?
(define-key *top-map* (kbd "s-u") "browser")
(define-key *top-map* (kbd "s-n") "nyxt")

(defcommand pull-emacs () ()
  (run-or-pull "emacs" '(:class "Emacs")))

(define-key *top-map* (kbd "s-E") "pull-emacs")
(define-key *top-map* (kbd "s-9")  "lock")
(define-key *top-map* (kbd "s-z") "suspend")

(defcommand mail () ()
  (window-send-string "Benjamin.Schwerdtner@gmail.com"))

(defcommand phone () ()
  (window-send-string (string-trim '(#\Newline) (run-shell-command "cat ~/phone-number" t))))
(defcommand my-first-name () ()
  (window-send-string "Benjamin"))
(defcommand my-last-name () ()
  (window-send-string "Schwerdtner"))
(defcommand my-linked-in () ()
  (window-send-string "https://www.linkedin.com/in/benjamin-schwerdtner-4987a1140/"))

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

(defvar framelist nil)

(defcommand (swap-this-window tile-group) () ()
  (let* ((f1
           (tile-group-current-frame (current-group)))
         (f2
           (progn (message "Select Window Two")
                  (choose-frame-by-number (current-group)))))
    (when (and f1 f2)
      (let ((w1 (frame-window f1))
            (w2 (frame-window f2)))
        (when w1 (pull-window w1 f2))
        (when w2 (pull-window w2 f1))
        (focus-frame (current-group) f2)))))

(defcommand edit-string-with-emacs () ()
  (let ((file-s (concat "/tmp/"
                        (string-trim '(#\Newline) (run-shell-command "uuidgen" t))))
        (w (current-window)))
    (hsplit)
    (move-focus :right)
    (sb-thread:make-thread
     (lambda ()
       (let ((emacs-command
               (format
                nil
                "timeout 2m emacsclient -c -e \"(mm/edit-with-editor \\\"~a\\\")\"" file-s)))
         (run-shell-command emacs-command t)
         (when (probe-file file-s)
           (stumpwm:call-in-main-thread
            (lambda ()
              (ignore-errors (remove-split))
              (window-send-string
               (let* ((s (string-trim '(#\Newline)
                                      (with-output-to-string (s)
                                        (with-open-file
                                            (is file-s :direction :input)
                                          (loop for line = (read-line is nil is)
                                                until
                                                (eq line is)
                                                do (format s "~A~%" line)))))))
                 ;; LOL
                 (cond
                   ((string= "teams-for-linux" (window-class w))
                    (reverse s))
                   (t s)))
               w)))))))))

(defcommand run-or-raise-teams () ()
  (run-or-raise "teams-for-linux" '(:class "teams-for-linux")))
(defcommand slack () ()
  (run-or-raise "slack" '(:class "Slack")))

(defun mm/select-window-dmenu ()
  "Select a window across all groups using dmenu. Returns the window or nil."
  (let* ((windows (apply #'append
                         (mapcar #'group-windows
                                 (screen-groups (current-screen)))))
         (tmpfile "/tmp/stumpwm-windows"))
    (with-open-file (s tmpfile :direction :output :if-exists :supersede)
      (dolist (w windows)
        (format s "~d~c~d [~a] ~a: ~a~%"
                (xlib:window-id (window-xwin w))
                #\Tab
                (xlib:window-id (window-xwin w))
                (group-name (window-group w))
                (window-class w)
                (window-title w))))
    (let ((result (string-trim '(#\Newline #\Space #\Tab)
                               (run-shell-command
                                (format nil "stump-window-select ~d"
                                        (head-number (current-head)))
                                t))))
      (when (and result (not (string= result "")))
        (let* ((xid (parse-integer result :junk-allowed t)))
          (when xid
            (find xid windows
                  :key (lambda (w) (xlib:window-id (window-xwin w))))))))))

(defcommand pull-window-across-groups () ()
  (let ((window (mm/select-window-dmenu)))
    (when window
      (move-window-to-group window (current-group))
      (pull-window window))))

(defcommand mm/find-window () ()
  (let ((win (mm/select-window-dmenu)))
    (when win
      (focus-all win))))

(defun mm/write-stumpwm-windows ()
  "Write window list to /tmp/stumpwm-windows."
  (let ((windows (apply #'append
                        (mapcar #'group-windows
                                (screen-groups (current-screen))))))
    (with-open-file (s "/tmp/stumpwm-windows" :direction :output :if-exists :supersede)
      (dolist (w windows)
        (format s "~d~c~d [~a] ~a: ~a~%"
                (xlib:window-id (window-xwin w))
                #\Tab
                (xlib:window-id (window-xwin w))
                (group-name (window-group w))
                (window-class w)
                (window-title w))))
    windows))

;; (defcommand mm/visual-find-window () ()
;;   "Visual window switcher with screenshot previews. Non-blocking."
;;   (let ((windows (mm/write-stumpwm-windows)))
;;     (sb-thread:make-thread
;;      (lambda ()
;;        (let ((result (string-trim
;;                       '(#\Newline #\Space #\Tab)
;;                       (run-shell-command
;;                        "timeout 30 /home/benj/repos/clj-windowswitcher/windowswitcher"
;;                        t))))
;;          (when (and result (not (string= result "")))
;;            (let ((xid (parse-integer result :junk-allowed t)))
;;              (when xid
;;                (call-in-main-thread
;;                 (lambda ()
;;                   (let ((win (find xid windows
;;                                    :key (lambda (w)
;;                                           (xlib:window-id (window-xwin w))))))
;;                     (when win
;;                       (focus-all win)))))))))))))


(defparameter *my-comma-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (kbd "m") "mail")
    (stumpwm:define-key m (kbd "p") "phone")
    (stumpwm:define-key m (kbd "f") "my-first-name")
    (stumpwm:define-key m (kbd "N") "my-last-name")
    (stumpwm:define-key m (kbd "l") "my-linked-in")
    (stumpwm:define-key m (kbd "e") "edit-string-with-emacs")
    (stumpwm:define-key m (kbd "w") "swap-this-window")
    (stumpwm:define-key m (kbd "r") "start-or-stop-recording")
    ;; (stumpwm:define-key m (kbd "y")
    ;;   (def-just-a-shell-command emacs-kill-xselection "ec-kill-xselection"))
    ;; (stumpwm:define-key m (kbd "c")
    ;;   (def-just-a-shell-command dunst-close-all "dunstctl close-all"))
    ;; (stumpwm:define-key m (kbd "x")
    ;;   (def-just-a-shell-command dunst-close-all "kill-unity"))
    (stumpwm:define-key m (kbd "n") "normie-mode")
    
    (stumpwm:define-key m (kbd "s") "run-or-raise-teams")
    
    (stumpwm:define-key m (kbd "a") "mm/find-window")
    ;; (stumpwm:define-key m (kbd "v") "mm/visual-find-window")
    (stumpwm:define-key m (kbd "A") "pull-window-across-groups")
    
    m))

(define-key *top-map* (kbd "s-s") "slack")
(define-key *top-map* (kbd "s-t") "run-or-raise-teams")

(define-key *top-map* (kbd "s-,") '*my-comma-map*)

(defcommand close-all-fullscreens-in-screen () ()
     (loop for window in (screen-windows (current-screen))
           when (window-fullscreen window)
             do (deactivate-fullscreen window)))

(define-key *top-map* (kbd "s--") "close-all-fullscreens-in-screen")

(setf *load-path* nil)
(init-load-path "~/.stumpwm.d/modules/")
(load-module "cpu")
(load-module "mem")

(setf mem::*mem-modeline-fmt* "MEM: %a %p %b")



(define-key *top-map* (kbd "s") nil)
(defun rec-modeline (ml)
 (declare (ignore ml))
  (if (probe-file
      "/tmp/recordingpid")
     "RECORDING"
     ""))

(add-screen-mode-line-formatter #\R 'rec-modeline)

(setf *screen-mode-line-format* "[^B%n^b] %C | %M  %R")

(load-module "battery-portable")
(setf *screen-mode-line-format* "[^B%n^b] %C | %M  %R %B")


(defcommand lock () ()
  (run-shell-command "best-lock.sh"))

(defcommand suspend () ()
  (run-shell-command "xterm -e 'sudo systemctl suspend'"))

(defcommand cmd-xkill () () (run-shell-command "xkill"))
(define-key *top-map* (kbd "s-]") "cmd-xkill")


(defcommand passmenu () ()
  (run-shell-command "passmenu-orderless"))

(defcommand passmenu-type () ()
  (run-shell-command "passmenu-orderless --type"))

(define-key *top-map* (kbd "s-a") "passmenu")
(define-key *top-map* (kbd "s-A") "passmenu-type")
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

(define-interactive-keymap
    mm/group-mode ()
  ((kbd "j") "gnext")
  ((kbd "k") "gprev")
  ((kbd "n") "gnew")
  ((kbd "K") "gprev-with-window")
  ((kbd "J") "gnext-with-window")
  ((kbd ",") "grouplist")
  ((kbd "a") "gselect"))

(define-key *groups-map* (kbd "s-g") "mm/group-mode")

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

;; thanks gavin
;; https://github.com/Gavinok/stump-conf

(defun emacsp (win)
  "Returns non-nil when WIN is an emacs window."
  (when win
    (string-equal (window-class win) "Emacs")))

(defmacro exec-el (expression)
  "execute emacs lisp do not collect it's output"
  `(eval-string-as-el (format nil "~a" ',expression)))

(defun eval-string-as-el (elisp &optional collect-output-p)
  "evaluate a string as emacs lisp"
  (let ((result
          (run-shell-command
           (string-downcase (format nil "timeout --signal=9 1m emacsclient --eval '~a'" elisp))
           collect-output-p)))
    (handler-case (read-from-string result)
      ;; Pass back a string when we can't read from the string
      (error () result))))

(defun eval-el-1 (form)
  "Eval FORM in emacs (via emacsclient) and return it's output.
FORM should be a quoted list."
  (eval-string-as-el (format nil "~a" form) t))

(defmacro eval-el (expression)
  "evaluate emacs lisp and collect it's output"
  `(eval-el-1 ',expression))

(defun emacs-winmove (direction)
  "executes the emacs function winmove-DIRECTION where DIRECTION is a string"
  (eval-string-as-el
   (concat "(mm/windmove \"" direction "\")") t))

;;; Window focusing

(defun better-move-focus (ogdir)
  "Similar to move-focus but also treats emacs windows as Xorg windows"
  (declare (type (member :up :down :left :right) ogdir))
  (flet ((mv () (move-focus ogdir)))
    (if (emacsp (current-window))
        (when
            (not (equal (emacs-winmove (string-downcase (string ogdir))) "ok"))
          (mv))
        (mv))))

(defcommand my-mv (dir) ((:direction "Enter direction: "))
  (when dir (better-move-focus dir)))
(define-key *top-map* (kbd "s-h") "my-mv left")
(define-key *top-map* (kbd "s-j") "my-mv down")
(define-key *top-map* (kbd "s-k") "my-mv up")
(define-key *top-map* (kbd "s-l") "my-mv right")

(defun make-an-emacs ()
  (deactivate-fullscreen (current-window))
  (vsplit)
  (move-focus :down)
  (exec-el (make-frame)))

(defcommand make-emacs-or-shell () ()
  (if (emacsp (current-window))
      (exec-el (mm/shell-via-async-shell-command))
    (exec-el (make-frame))))

(defcommand mm-kill-window-or-in-emacs () ()
  (if (emacsp (current-window))
      (exec-el (mm/delete-window-or-frame))
    (remove-split)))

(defcommand mm-consult-windows () ()
  (unless (emacsp (current-window))
    (make-an-emacs))
  (exec-el (mm/consult-stumpwm-windows)))

(define-key *top-map* (kbd "s-.") "mm-consult-windows")
(define-key *top-map* (kbd "s-x") "mm-kill-window-or-in-emacs")
(define-key *top-map* (kbd "s-RET") "make-emacs-or-shell")

;;; SLY setup

;; (ql:quickload :slynk)
;; (defvar *slynk-port* slynk::default-server-port)
;; (defparameter *stumpwm-slynk-session* nil)


(define-remapped-keys
 '(("Nyxt"
    ("M-n"   . "Down")
    ("M-p"   . "Up"))
   ("jetbrains-rider"
    ("M-n"   . "Down")
    ("M-p"   . "Up"))
   ("teams-for-linux"
    ("M-n"   . "Down")
    ("M-p"   . "Up"))
   ("Slack"
    ("M-n"   . "Down")
    ("M-p"   . "Up"))))


;; (load-module "battery-portable")
;; (setf *screen-mode-line-format* "[^B%n^b] %C | %M  %R %B")


(comment
 ;; (progn
 ;;   (load-module "stumptray")
 ;;   (defun ben/select-systray-head (heads)
 ;;     (or
 ;;      (first
 ;;       (remove-if-not #'stumpwm::head-mode-line heads))
 ;;      (error "No heads have a modeline on this screen.")))
 ;;   (setf stumptray::*tray-head-selection-fn* #'ben/select-systray-head)
 ;;   (stumptray::stumptray))

 (window-list)

 (when (not (equal (emacs-winmove (string-downcase (string "right"))) 'OK))
   'foo)

 (when (not (equal (emacs-winmove (string-downcase (string 'left))) 'OK))
   'foo)
 
 (exec-el (message "hi2"))
 (eval-el (current-bufferr))
 (group-indicate-focus (current-group))
 (group-sync-all-heads (current-group))
 (setf *debug-level* 0)
 (redirect-all-output (data-dir-file "output" "log"))
 (equal *module-dir* (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/modules")))

 (run-or-raise "slack" '(:class "Slack"))

 (window-class
  (car (remove-if-not
        (lambda (w)
          (search
           "- Slack"
           (window-title w)))
        (group-windows (current-group)))))
 "Slack"

 (list-modules)
 (my-mv :left)
 (load-module "wifi")
 (setf *screen-mode-line-format* "[^B%n^b] %C | %M  %R %I")
 (run-shell-command "hostname" t)
 (load-module "battery-portable")
 (setf *screen-mode-line-format* "[^B%n^b] %C | %M  %R %B")
 (tile-group-current-frame (current-group))
 (group-windows (current-group)))


;;; Window picker: temporary tiled view of all windows

(defvar *expose-n-max* 9)
(defvar *window-picker-saved-dump* nil)
(defvar *window-picker-saved-window* nil)
(defvar *window-picker-selected* nil)
(defvar *window-picker-fullscreen-windows* nil)

(defun mm/only-head (group head)
  "Collapse HEAD to a single frame, like `only' but for a specific head."
  (let* ((windows (remove-if (lambda (w) (typep w 'float-window))
                             (head-windows group head)))
         (frame (copy-frame head)))
    (when windows
      (mapc (lambda (w)
              (hide-window w)
              (setf (window-frame w) frame))
            windows)
      (setf (frame-window frame) (car windows)
            (tile-group-frame-head group head) frame
            (tile-group-current-frame group) frame))))

(defun mm/recursive-tile-head (n group head)
  "Like recursive-tile but only splits frames belonging to HEAD."
  (unless (<= n 1)
    (let* ((frames (head-frames group head))
           (areas (mapcar (lambda (f) (* (frame-width f) (frame-height f)))
                          frames))
           (idx (position (reduce #'max areas) areas))
           (frame (nth idx frames))
           (w (frame-width frame))
           (h (frame-height frame)))
      (focus-frame group frame)
      (if (< w h) (vsplit) (hsplit)))
    (mm/recursive-tile-head (- n 1) group head)))

(defun mm/expose-tile-per-head (group)
  "Tile windows per head, keeping each window on its original screen."
  ;; Snapshot window assignments per head before modifying frame trees,
  ;; so that vsplit/hsplit side-effects cannot leak windows across heads.
  (let ((head-windows-alist
         (loop for head in (group-heads group)
               collect (cons head
                             (remove-if (lambda (w) (typep w 'float-window))
                                        (head-windows group head))))))
    (dolist (entry head-windows-alist)
      (let* ((head (car entry))
             (windows (cdr entry))
             (n (length windows)))
        (when (> n 1)
          (mm/only-head group head)
          (mm/recursive-tile-head (min *expose-n-max* n) group head)
          ;; Re-assign windows to this head's frames, undoing any cross-head pulls.
          (let ((frames (head-frames group head)))
            (loop for w in windows
                  for f in frames
                  do (setf (window-frame w) f
                           (frame-window f) w))))))))

(defun mm/sort-windows-by-frame-position (group)
  "Sort group windows by frame position (top-to-bottom, left-to-right)."
  (let ((sorted (sort (copy-list (group-windows group))
                      (lambda (a b)
                        (let ((fa (window-frame a))
                              (fb (window-frame b)))
                          (or (< (frame-y fa) (frame-y fb))
                              (and (= (frame-y fa) (frame-y fb))
                                   (< (frame-x fa) (frame-x fb)))))))))
    (setf (slot-value group 'stumpwm::windows) sorted)))

(defun window-picker-enter ()
  (let ((group (current-group)))
    (setf *window-picker-saved-dump* (dump-group group)
          *window-picker-saved-window* (group-current-window group)
          *window-picker-selected* nil
          *window-picker-fullscreen-windows*
          (loop for w in (group-windows group)
                when (window-fullscreen w)
                collect w
                and do (deactivate-fullscreen w)))
    (mm/expose-tile-per-head group)
    (mm/sort-windows-by-frame-position group)
    (when *window-picker-saved-window*
      (focus-frame group (window-frame *window-picker-saved-window*)))))

(defun window-picker-exit ()
  (let ((group (current-group))
        (target (or *window-picker-selected* *window-picker-saved-window*)))
    (when *window-picker-saved-dump*
      (restore-group group *window-picker-saved-dump*)
      (dolist (w *window-picker-fullscreen-windows*)
        (when (find w (group-windows group))
          (activate-fullscreen w)))
      (when target
        (group-focus-window group target))
      (setf *window-picker-saved-dump* nil
            *window-picker-saved-window* nil
            *window-picker-selected* nil
            *window-picker-fullscreen-windows* nil))))

(defcommand window-picker-select () ()
  (setf *window-picker-selected* (group-current-window (current-group))))

(defcommand window-picker-move (dir &optional fallback) ((:direction "Dir: ")
                                                         (:direction "Fallback: "))
  "Move focus in DIR, falling back to FALLBACK if no neighbour."
  (let* ((group (current-group))
         (frame (tile-group-current-frame group))
         (frames (group-frames group)))
    (if (neighbour dir frame frames)
        (move-focus dir)
        (when (and fallback (neighbour fallback frame frames))
          (move-focus fallback)))))

(define-interactive-keymap (window-picker tile-group)
    (:on-enter #'window-picker-enter
     :on-exit #'window-picker-exit
     :exit-on ((kbd "ESC") (kbd "C-g")))
  ((kbd "s-l")    "move-focus right")
  ((kbd "s-h")    "move-focus left")
  ((kbd "s-k")    "window-picker-move up right")
  ((kbd "s-j")    "window-picker-move down right")
  ((kbd "s-n")    "window-picker-move down right")
  ((kbd "s-p")    "window-picker-move up right")
  ((kbd "Right")  "move-focus right")
  ((kbd "Left")   "move-focus left")
  ((kbd "Up")     "window-picker-move up")
  ((kbd "Down")   "window-picker-move down")
  ((kbd "l")      "move-focus right")
  ((kbd "h")      "move-focus left")
  ((kbd "k")      "window-picker-move up right")
  ((kbd "j")      "window-picker-move down right")
  ((kbd "n")      "fnext")
  ((kbd "p")      "fprev")
  ((kbd "RET")    "window-picker-select" t))

(define-key *top-map* (kbd "s-w") "window-picker")
