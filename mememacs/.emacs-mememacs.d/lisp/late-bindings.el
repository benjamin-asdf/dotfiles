(mememacs/comma-def
  "n" '(:ignore t :which-key "n..")
  "nw" #'widen
  "nd" #'narrow-to-defun
  "nr" #'narrow-to-region
  "np" #'narrow-to-page

  "a" '(:ignore t :which-key "a..")
  "al" #'list-processes
  "at" #'helm-timers
  "s" '(:ignore t : which-key "s..")
  "ss" #'helm-swoop-without-pre-input
  "sS" #'helm-swoop

  "fe" #'mememacs/find-init-file

  "re" #'evil-show-registers

  "rp" (defun mm/evil-paste-clipboard ()
	 (interactive)
	 (evil-paste-from-register ?*))

  ;; "x" '(ctl-x-map :which-key "c-x-map")

  )



(provide 'late-bindings)
