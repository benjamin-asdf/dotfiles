(general-def
  evil-window-map
  "m" #'delete-other-windows
  "d" #'evil-window-delete)

(mememacs/leader-def
  "t" '(:ignore t)
  "n" '(:ignore t)
  "nn" #'display-line-numbers-mode

  "b" '(:ignore t)
  "bd" #'kill-current-buffer
  "be" #'erase-buffer
  "bw" #'toggle-read-only
  "b." #'hydra-buffer/body
  "f" '(:ignore t)
  "fs" #'save-buffer
  "fS" #'save-some-buffers
  "u" #'undo-tree-visualize
  "w" evil-window-map
  "j" '(:ignore t)

  "jd" #'dired-jump
  "jD" #'dired-jump-other-window
  "jf" #'find-function
  "jF" #'find-function-other-window
  "jv" #'find-variable
  "jV" #'find-variable-other-window
  "jb" #'bookmark-jump
  "je" '(:ignore t )
  "jel" #'find-library
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
  "C-M-j" #'consult-buffer
  "M-/" #'dabbrev-expand
  "H-j" #'evil-window-bottom
  "H-k" #'evil-window-top)


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

(mememacs/comma-def
  "n" '(:ignore t)
  "nw" #'widen
  "nd" #'narrow-to-defun
  "nr" #'narrow-to-region
  "np" #'narrow-to-page

  "a" '(:ignore t)
  "al" #'list-processes
  "at" #'helm-timers
  "s" '(:ignore t)
  "ss" #'consult-line
  "sS" #'consult-line-multi

  "fe" #'mememacs/find-init-file
  "fl" #'find-file-at-point

  "re" #'evil-show-registers

  "rp" (defun mm/evil-paste-clipboard ()
	 (interactive)
	 (evil-paste-from-register ?*)))

(defhydra flyspell-hydra ()
  ("j" #'evil-next-flyspell-error)
  ("k" #'evil-prev-flyspell-error)
  ("," #'flyspell-auto-correct-word :exit nil))

(mememacs/local-def
  :states '(normal visual emacs)
  :keymaps '(flyspell-mode-map)
  "sn" #'flyspell-hydra/evil-next-flyspell-error)

(mememacs/local-def
  :states '(normal visual emacs)
  :keymaps '(global-map)
  "r" #'revert-buffer)

;; I press this button accidentally all the time
(general-def 'help-map "h" nil)

(provide 'keybinds)
