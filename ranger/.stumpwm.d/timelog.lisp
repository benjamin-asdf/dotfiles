;; timelog.lisp -- automatic time log of window focus.
;;
;; Appends one TSV line per focus/title change to
;; ~/.local/share/timelog/YYYY-MM-DD.tsv:
;;
;;   ISO-timestamp <TAB> group <TAB> class <TAB> title
;;
;; An unchanged window is re-logged every *timelog-heartbeat* seconds, so a
;; gap longer than that in the log means the machine was off/asleep.
;;
;; Idle: a sampler runs every *timelog-sample-seconds*. When the X idle
;; time (XScreenSaver extension, `idle-time') exceeds
;; *timelog-idle-threshold*, it writes an IDLE line backdated to the moment
;; input stopped:
;;
;;   ISO-timestamp <TAB> IDLE <TAB> <TAB> <idle-seconds>
;;
;; and logs nothing until input resumes; then the current window is logged
;; again. Lock screen time counts as idle automatically.
;;
;; Report: `timelog' (bb script in ~/.local/bin), or the `timelog-today'
;; command below.
;;
;; Everything that runs from a hook or timer is wrapped in `ignore-errors':
;; a failing hook in the WM is far worse than a missing log line.

(in-package :stumpwm)

(defvar *timelog-dir* (merge-pathnames ".local/share/timelog/" (user-homedir-pathname)))
(defvar *timelog-sample-seconds* 30)
(defvar *timelog-idle-threshold* 300
  "Seconds without input after which the user counts as idle.")

(defvar *timelog-heartbeat* 300
  "Re-log an unchanged window after this many seconds, so long stretches in
one window are distinguishable from gaps (suspend, WM restart).")

(defvar *timelog-last-time* 0)
(defvar *timelog-last* nil
  "List (group class title) of the last logged entry, or :idle.")
(defvar *timelog-timer* nil)

(defun timelog-clean (s)
  "Replace tabs/newlines so a field never breaks the TSV line."
  (substitute-if #\Space (lambda (c) (member c '(#\Tab #\Newline #\Return)))
                 (or s "")))

(defun timelog-iso (universal-time)
  (multiple-value-bind (s m h d mo y) (decode-universal-time universal-time)
    (values (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d" y mo d h m s)
            (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d))))

(defun timelog-write (universal-time fields)
  (multiple-value-bind (iso day) (timelog-iso universal-time)
    (let ((path (merge-pathnames (concatenate 'string day ".tsv") *timelog-dir*)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :append
                                :if-does-not-exist :create
                                :external-format :utf-8)
        (format out "~a~{~c~a~}~%" iso
                (loop for f in fields collect #\Tab collect (timelog-clean f)))))))

(defun timelog-current ()
  "List (group class title) for what currently has focus."
  (let* ((group (current-group))
         (win (current-window)))
    (list (if group (group-name group) "")
          (if win (window-class win) "")
          (if win (window-title win) ""))))

(defun timelog-key (entry)
  "Comparison key for ENTRY: the title minus leading non-alphanumerics, so
spinner glyphs in terminal titles (Claude Code: ✳ ◐ ◑ ...) don't count as
a change."
  (destructuring-bind (group class title) entry
    (list group class
          (string-left-trim '(#\Space)
                            (subseq title (or (position-if #'alphanumericp title)
                                              (length title)))))))

(defun timelog-idle-seconds ()
  (or (ignore-errors (idle-time (current-screen))) 0))

(defun timelog-maybe-log ()
  "Log the focused window if it differs from the last entry (or the heartbeat
is due). Cheap no-op otherwise."
  (ignore-errors
   (let ((idle (timelog-idle-seconds)))
     (if (> idle *timelog-idle-threshold*)
         (unless (eq *timelog-last* :idle)
           (setf *timelog-last* :idle)
           (timelog-write (- (get-universal-time) (round idle))
                          (list "IDLE" "" (format nil "~d" (round idle)))))
         (let ((cur (timelog-current))
               (now (get-universal-time)))
           (unless (and (consp *timelog-last*)
                        (equal (timelog-key cur) (timelog-key *timelog-last*))
                        (< (- now *timelog-last-time*) *timelog-heartbeat*))
             (setf *timelog-last* cur
                   *timelog-last-time* now)
             (timelog-write now cur)))))))

(defun timelog-focus-window-hook (&rest args)
  (declare (ignore args))
  (timelog-maybe-log))

(defun timelog-focus-group-hook (&rest args)
  (declare (ignore args))
  (timelog-maybe-log))

(defun timelog-start ()
  (add-hook *focus-window-hook* 'timelog-focus-window-hook)
  (add-hook *focus-group-hook* 'timelog-focus-group-hook)
  (when *timelog-timer* (ignore-errors (cancel-timer *timelog-timer*)))
  (setf *timelog-timer*
        (run-with-timer *timelog-sample-seconds* *timelog-sample-seconds*
                        'timelog-maybe-log)))

(defun timelog-stop ()
  (remove-hook *focus-window-hook* 'timelog-focus-window-hook)
  (remove-hook *focus-group-hook* 'timelog-focus-group-hook)
  (when *timelog-timer* (ignore-errors (cancel-timer *timelog-timer*)))
  (setf *timelog-timer* nil))

(defcommand timelog-today () ()
  "Show today's time report as a StumpWM message."
  (let ((out (ignore-errors (run-shell-command "timelog today --short 2>&1" t))))
    ;; ^ is StumpWM's color escape; double it so titles render literally.
    (message "~a" (cl-ppcre:regex-replace-all "\\^" (or out "timelog failed") "^^"))))

(defcommand timelog-toggle () ()
  "Pause/resume the time log."
  (if *timelog-timer*
      (progn (timelog-stop) (message "timelog paused"))
      (progn (timelog-start) (message "timelog running"))))

;; Reloading this file is safe: hooks are added idempotently and the timer
;; is replaced.
(timelog-start)
