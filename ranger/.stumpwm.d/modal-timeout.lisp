;; modal-timeout.lisp -- modal key reads give up instead of waiting forever.
;;
;; A pending key sequence (after a prefix key) or an open prompt grabs the
;; keyboard and waits in xlib:process-event with :timeout nil. A stray prefix
;; key, or a prompt on a head that is not visible, then looks like a hung
;; window manager. Here the three readers from stumpwm's input.lisp are
;; redefined with a deadline; when it passes they return C-g, so every caller
;; takes its normal abort path.
;;
;;   *key-seq-timeout*   seconds without a key inside a key sequence
;;   *prompt-timeout*    seconds without a key in a prompt (colon, eval,
;;                       read-one-char, frame picker ...)
;;
;; nil turns a timeout off. The clock restarts on every key. A timeout can
;; fire late, by less than its own length: clx restarts its wait on every X
;; event, the deadline is only checked when an event arrives or a wait ends.
;;
;; From a tty, when it is stuck anyway: stumpwm-unstick

(in-package :stumpwm)

(defvar *key-seq-timeout* 5
  "Seconds a key sequence waits for its next key, nil to wait forever.")

(defvar *prompt-timeout* 60
  "Seconds a prompt waits for its next key, nil to wait forever.")

(defvar *modal-read-timeout-var* '*prompt-timeout*
  "The timeout variable that applies to the modal read in progress.")

(defvar *modal-read-timeouts* nil
  "Universal times at which a modal read timed out, newest first.")

(defun modal-timeout-key ()
  "The (code . state) of C-g, what a timed out read returns."
  (multiple-value-bind (code state) (key-to-keycode+state (kbd "C-g"))
    (cons code state)))

(defun process-modal-event (handler)
  "Like (xlib:process-event *display* :handler HANDLER :timeout nil), but
return (:key-press code . state) for C-g once the current timeout has passed."
  (let ((seconds (symbol-value *modal-read-timeout-var*)))
    (if (null seconds)
        (xlib:process-event *display* :handler handler :timeout nil)
        (let ((deadline (+ (get-internal-real-time)
                           (* seconds internal-time-units-per-second))))
          (flet ((remaining ()
                   (/ (- deadline (get-internal-real-time))
                      internal-time-units-per-second)))
            (or (catch 'modal-read-timeout
                  (loop
                    (when (<= (remaining) 0)
                      (throw 'modal-read-timeout nil))
                    (let ((ev (xlib:process-event
                               *display*
                               :handler (lambda (&rest event-slots)
                                          ;; Throwing out of the handler leaves
                                          ;; the event queued, like returning nil.
                                          (when (<= (remaining) 0)
                                            (throw 'modal-read-timeout nil))
                                          (apply handler event-slots))
                               :timeout (remaining))))
                      (when ev (return ev)))))
                (progn
                  (push (get-universal-time) *modal-read-timeouts*)
                  (cons :key-press (modal-timeout-key)))))))))

(defun read-key ()
  "Return a dotted pair (code . state) key."
  (loop for ev = (process-modal-event #'read-key-handle-event) do
    (when (and (consp ev)
               (eq (first ev) :key-press))
      (return (rest ev)))))

(defun read-key-or-click ()
  (loop for ev = (process-modal-event #'read-key-or-click-handle-event)
        do
           (when (consp ev)
             (when (eq (first ev) :key-press)
               (return (values nil (rest ev) nil nil)))
             (when (eq (first ev) :button-press)
               (return (values t nil (second ev) (third ev)))))))

(defun read-key-or-selection ()
  (loop for ev = (process-modal-event #'read-key-or-selection-handle-event) do
    (cond ((stringp ev)
           (return ev))
          ((and (consp ev)
                (eq (first ev) :key-press))
           (return (rest ev))))))

;; Key sequences read through handle-keymap; everything else is a prompt.
(defvar *handle-keymap-untimed* #'handle-keymap
  "handle-keymap as stumpwm defines it, kept across reloads of this file.")

(defun handle-keymap (&rest args)
  (let ((*modal-read-timeout-var* '*key-seq-timeout*))
    (apply *handle-keymap-untimed* args)))
