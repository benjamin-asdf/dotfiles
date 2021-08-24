(general-create-definer
  mememacs/leader-def
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(general-create-definer
  mememacs/comma-def
  :states '(normal visual emacs)
  :prefix ",")

(general-def
  evil-window-map
  "m" #'delete-other-windows
  "d" #'evil-window-delete)

(mememacs/leader-def
  "SPC" #'helm-M-x
  "t" '(:ignore t)
  "n" '(:ignore t)
  "nn" #'display-line-numbers-mode
  "nw" #'widen
  "nd" #'narrow-to-defun
  "nr" #'narrow-to-region


  "b" '(:ignore t :which-key "b..")
  "bd" #'kill-this-buffer
  "be" #'erase-buffer
  "bw" #'toggle-read-only
  "bb" #'helm-mini
  "b." #'hydra-buffer/body

  "f" '(:ignore t :which-key "f..")
  "fd" #'delete-file
  "fs" #'save-buffer
  "ff" #'helm-find-files
  "fr" #'helm-recentf

  "u" #'undo-tree-visualize

  "w" '(evil-window-map :which-key "window")

  "s" '(:ignore t :which-key "search")
  "ss" #'helm-swoop-without-pre-input
  "sS" #'helm-swoop

  "j" '(:ignore t)
  ;; "jr" #'

  "jd" #'dired-jump
  "jD" #'dired-jump-other-window
  "jf" #'find-function
  "jF" #'find-function-other-window
  "jv" #'find-variable
  "jV" #'find-variable-other-window
  "jb" #'bookmark-jump
  "je" '(:ignore t :which-key "emacs")
  "jel" #'find-library
  ;; "jel" #'lisp-find-map
  "jm" #'view-echo-area-messages


  "/" #'helm-do-grep-ag
  "hc" #'describe-char
  "hm" #'describe-mode
  "hi" #'helm-info-emacs

  "x" '(:ignore t :which-key "text")
  "xi" #'indent-region
  "xt" '(:ignore t)
  "xtw" #'transpose-words

  "p" '(:ignore t :which-key "procs..")
  "pa" #'list-processes

  (kbd "<tab>") #'ambrevar/switch-to-last-buffer

  )




(general-def
  "C-o" #'evil-jump-forward
  "C-i" (defun jump-back ()
	  (interactive)
	  (when (in major-mode 'clojure-mode)
	    (cider-pop-back))
	  (let ((p (point)))
	    (evil-jump-backward)
	    (when (eq p (point))
	      (pop-tag-mark)))))


(provide 'init-general)
