;;; Functions-1

(defvar mememacs/config-dir
  (expand-file-name "~/.emacs-mememacs.d/"))

(defun mememacs/find-init-file ()
  "Open current init file."
  (interactive)
  (find-file
   (expand-file-name
    (concat mememacs/config-dir "init.el"))))

(defun mememacs/copy-dir-name-name-as-kill-dwim ()
  ""
  (interactive)
  (kill-new
   (file-name-directory (expand-file-name default-directory))))

(mememacs/leader-def
  "fe" #'mememacs/find-init-file
  "fD" #'mememacs/copy-dir-name-name-as-kill-dwim)

(defun mememacs/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer
   (get-buffer-create
     "*scratch*")))

(mememacs/leader-def
  "bs" #'mememacs/switch-to-scratch-buffer)

(defun mememacs/lispy-eval-line ()
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (special-lispy-eval)))

(defun mememacs/cancel-debugs ()
  (interactive)
  (cancel-debug-on-entry)
  (cancel-debug-on-variable-change)
  (untrace-all))

(defun mememacs/mkstr (obj)
  (with-output-to-string
    (princ obj)))

(defun mememacs/eval-last-sexp-dwim (arg)
  "Eval last sexp.
If it is a quoted symbol, eval symbol value instead.
See `eval-last-sexp'."
  (interactive "P")
  (let ((s (sexp-at-point)))
    (if (eq 'quote (car-safe s))
	(with-temp-buffer
	  (insert (mememacs/mkstr (cadr s)))
	  (goto-char (point-max))
	  (eval-last-sexp arg))
      (eval-last-sexp arg))))

(general-def
  :states '(normal motion)
  "," nil
  ",e" '(:ignore t :which-key "eval")
  ",el" #'mememacs/lispy-eval-line
  ",ef" #'eval-defun
  ",eb" #'eval-buffer
  ",ed" #'edebug-defun
  ",ee" #'mememacs/eval-last-sexp-dwim
  ",et" #'toggle-debug-on-error
  ",eq" #'toggle-debug-on-quit
  ",d" '(:ignore t :which-key "devel")
  ",dv" #'debug-on-variable-change
  ",dd" #'debug-on-entry
  ",dt" #'trace-function
  ",dx" #'mememacs/cancel-debugs)

(defun mememacs/switch-to-message-buffer ()
  ""
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun mememacs/ghetto-kill-and-open-buffer ()
  "Kill buffer and open again."
  (interactive)
  (when-let ((p (point))
	     (f (buffer-file-name)))
    (kill-this-buffer)
    (find-file f)
    (goto-char p)))

(mememacs/leader-def
  "bm" #'mememacs/switch-to-message-buffer
  "bR" #'mememacs/ghetto-kill-and-open-buffer
  "br" #'revert-buffer)



(provide 'functions-1)
