;; marks.lisp -- vim-style window marks, one key: s-' then a char.
;;
;;   s-' a   jump to mark a; if a is unset, mark the current window as a.
;;           On the marked window already: jump back (toggle).
;;   s-' A   (re)assign a to the current window.
;;   s-' '   jump to the previously focused window, across groups.
;;   s-' SPC show the marks.
;;
;; A mark remembers group name, frame number, window class and title, and
;; is saved to *marks-file*, so it survives window and WM restarts:
;;   1. the marked window if it still exists,
;;   2. else the best window of the same class (same group+frame first,
;;      then same group, then exact title),
;;   3. else at least the group + frame it lived in.
;; Only when even the group is gone is the letter free again.

(in-package :stumpwm)

(defvar *marks-file*
  (merge-pathnames ".local/share/stumpwm/marks.sexp" (user-homedir-pathname)))

(defvar *marks* (make-hash-table)
  "Lowercase char -> plist (:window :group :frame :class :title).")

(defvar *mark-previous-window* nil
  "The window focused before the current one, in any group.")

;; -- mark data

(defun mark-live-window (window)
  (and window (member window (all-windows)) window))

(defun mark-window-frame-number (window)
  (ignore-errors (frame-number (window-frame window))))

(defun mark-from-window (window)
  (list :window window
        :group (group-name (window-group window))
        :frame (mark-window-frame-number window)
        :class (window-class window)
        :title (window-name window)))

(defun mark-refresh (mark)
  "Update a mark's location from its live window (windows move)."
  (let ((window (mark-live-window (getf mark :window))))
    (if window (mark-from-window window) mark)))

(defun marks-save ()
  (ignore-errors
   (ensure-directories-exist *marks-file*)
   (with-open-file (out *marks-file* :direction :output :if-exists :supersede)
     (with-standard-io-syntax
       (let ((*print-readably* nil))
         (prin1 (loop for char being the hash-keys of *marks*
                        using (hash-value mark)
                      collect (cons char (let ((m (copy-list (mark-refresh mark))))
                                           (remf m :window)
                                           m)))
                out))))))

(defun marks-load ()
  (ignore-errors
   (with-open-file (in *marks-file* :if-does-not-exist nil)
     (when in
       (with-standard-io-syntax
         (let ((*read-eval* nil))
           (loop for (char . mark) in (read in nil)
                 unless (gethash char *marks*)
                   do (setf (gethash char *marks*) mark))))))))

;; -- resolving

(defun mark-group (mark)
  (find-group (current-screen) (getf mark :group)))

(defun mark-best-window (mark)
  "The live window, else the best same-class candidate, else nil."
  (or (mark-live-window (getf mark :window))
      (let ((group (getf mark :group))
            (frame (getf mark :frame))
            (best nil)
            (best-score -1))
        (dolist (w (all-windows))
          (when (equal (window-class w) (getf mark :class))
            (let* ((same-group (equal (group-name (window-group w)) group))
                   (score (+ (if (and same-group
                                      (eql (mark-window-frame-number w) frame))
                                 4 0)
                             (if same-group 2 0)
                             (if (equal (window-name w) (getf mark :title)) 1 0))))
              (when (> score best-score)
                (setf best w best-score score)))))
        best)))

(defun mark-goto-place (mark)
  "Window gone: go to the mark's group and frame. Nil if the group is gone."
  (let ((group (mark-group mark)))
    (when group
      (switch-to-group group)
      (let ((frame (and (getf mark :frame)
                        (typep group 'tile-group)
                        (frame-by-number group (getf mark :frame)))))
        (when frame (focus-frame group frame)))
      (message "~a window gone; at ~a / frame ~a"
               (getf mark :class) (getf mark :group) (getf mark :frame))
      t)))

;; -- display

(defun mark-label (mark)
  (let ((title (or (getf mark :title) "")))
    (format nil "~a/~a ~a: ~a~a"
            (getf mark :group) (getf mark :frame) (getf mark :class)
            (subseq title 0 (min 40 (length title)))
            (if (mark-best-window mark) "" "  (gone)"))))

(defun marks-summary ()
  (let ((lines '()))
    (maphash (lambda (char mark)
               (push (format nil "~a  ~a" char (mark-label mark)) lines))
             *marks*)
    (if lines
        (format nil "~{~a~^~%~}" (sort lines #'string<))
        "no marks -- press a letter to mark this window")))

;; -- actions

(defun mark-set (char window)
  (let ((mark (mark-from-window window)))
    (setf (gethash char *marks*) mark)
    (marks-save)
    (message "mark ~a -> ~a" char (mark-label mark))))

(defun mark-jump-previous ()
  (if (mark-live-window *mark-previous-window*)
      (focus-all *mark-previous-window*)
      (message "no previous window")))

(defun mark-jump (char current)
  "Jump to mark CHAR; mark CURRENT instead when CHAR is free."
  (let* ((mark (gethash char *marks*))
         (target (and mark (mark-best-window mark))))
    (cond
      ((and target (eq target current)) (mark-jump-previous))
      (target
       ;; re-bind to the found window, so the next jump is exact
       (setf (getf (gethash char *marks*) :window) target)
       (focus-all target))
      ((and mark (mark-goto-place mark)))
      (current (mark-set char current))
      (t (message "no window to mark")))))

(defun mark-track-previous (current last)
  (ignore-errors
   (when (and last (not (eq current last)))
     (setf *mark-previous-window* last))))

(remove-hook *focus-window-hook* 'mark-track-previous)
(add-hook *focus-window-hook* 'mark-track-previous)

(defcommand mark-or-jump () ()
  "Read one char: jump to its mark, or create it if unset. See marks.lisp."
  (message-no-timeout "mark/jump:~%~a" (marks-summary))
  (let ((char (read-one-char (current-screen)))
        (current (current-window)))
    (unmap-all-message-windows)
    (cond
      ((null char))
      ((char= char #\') (mark-jump-previous))
      ((char= char #\Space) (message "~a" (marks-summary)))
      ((not (alpha-char-p char)))       ;; C-g, ESC, digits ...: abort
      ((upper-case-p char)
       (if current
           (mark-set (char-downcase char) current)
           (message "no window to mark")))
      (t (mark-jump char current))))
  (marks-save))

(marks-load)

(define-key *top-map* (kbd "s-'") "mark-or-jump")
