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
    *scratch*"")))

(mememacs/leader-def
  "bs" #'mememacs/switch-to-scratch-buffer)




(provide 'functions-1)
