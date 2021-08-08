;;; Functions-1

(defun mememacs/find-init-file ()
  "Open current init file."
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

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

(general-def
  :states '(normal motion)
  "," nil
  ",e" '(:ignore t :which-key "eval")
  ",el" #'mememacs/lispy-eval-line
  ",ef" #'eval-defun
  ",ed" #'edebug-defun
  ",d" '(:ignore t :which-key "devel")
  ",dv" #'debug-on-variable-change
  ",dd" #'debug-on-entry
  ",dt" #'trace-function
  ",dx" #'mememacs/cancel-debugs)

(provide 'functions-1)
