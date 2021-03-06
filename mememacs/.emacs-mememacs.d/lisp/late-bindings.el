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

(provide 'late-bindings)
