;;; Functions-1

(defun mememacs/find-init-file ()
  "Open current init file."
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(mememacs/leader-def
 "fe" #'mememacs/find-init-file)


(provide 'functions-1)
