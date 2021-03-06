(general-create-definer
  mememacs/leader-def
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(general-create-definer
  mememacs/comma-def
  :states '(normal visual emacs)
  :prefix ",")

(general-create-definer
  mememacs/local-def
  :states '(normal visual emacs insert motion)
  :prefix "C-,")

(general-def
  evil-window-map
  "m" #'delete-other-windows
  "d" #'evil-window-delete)

(mememacs/leader-def
  "t" '(:ignore t)
  "n" '(:ignore t)
  "nn" #'display-line-numbers-mode

  "b" '(:ignore t )
  "bd" #'kill-current-buffer
  "be" #'erase-buffer
  "bw" #'toggle-read-only
  "b." #'hydra-buffer/body

  "f" '(:ignore t )
  "fs" #'save-buffer
  "fS" #'save-some-buffers

  "u" #'undo-tree-visualize

  "w" evil-window-map


  "j" '(:ignore t)
  ;; "jr" #'

  "jd" #'dired-jump
  "jD" #'dired-jump-other-window
  "jf" #'find-function
  "jF" #'find-function-other-window
  "jv" #'find-variable
  "jV" #'find-variable-other-window
  "jb" #'bookmark-jump
  "je" '(:ignore t )
  "jel" #'find-library
  ;; "jel" #'lisp-find-map
  "jm" #'view-echo-area-messages


  "hc" #'describe-char
  "hm" #'describe-mode

  "x" '(:ignore t )
  "xi" #'indent-region
  "xt" '(:ignore t)
  "xtw" #'transpose-words

  "p" '(:ignore t )
  "pa" #'list-processes

  (kbd "<tab>") #'ambrevar/switch-to-last-buffer)

(general-unbind evil-motion-state-map "SPC")

(general-def
  "H-<return>" #'save-buffer
  "C-M-j" #'consult-buffer)


;; todo why is this overriden or whatever

(defun mememacs/jump-back ()
  (interactive)
  (if (in major-mode 'clojure-mode)
      (cider-pop-back)
    (let ((p (point)))
      (evil-jump-backward)
      (when (eq p (point))
	(pop-tag-mark)))))

;; xref pop marker stack
;; we need to figure out something general

(general-def
  :states '(normal visual motion)
  "C-i" #'evil-jump-forward
  "C-o" #'mememacs/jump-back
  "gr" #'revert-buffer)

(global-set-key
  (kbd
   "s-<backspace>")
  #'evil-delete-whole-line)


(provide 'init-general)
