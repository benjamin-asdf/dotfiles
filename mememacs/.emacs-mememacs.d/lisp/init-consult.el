(require 'patch-consult)

(mememacs/local-def
  "SPC" #'consult-mode-command)
(mememacs/comma-def
  "ss" #'consult-line
  "sS" #'consult-line-multi
  "sk" #'consult-keep-lines
  "si" #'consult-isearch
  ;; "so" #'consult-oc
  "sf" #'consult-focus-lines
  "g/" #'consult-git-grep

  "fl" #'consult-locate
  "ff" #'consult-find
  "fo" #'consult-file-externally
  "hw" #'consult-man
  "M" #'consult-minor-mode-menu)

(general-def
  'minibuffer-mode-map
  "M-h" #'consult-history
  "M-i" #'completion-at-point)

(general-def
  :prefix
  "H-m"
  "M" #'consult-register-store
  "m" #'consult-register
  "b" #'consult-bookmark)

(mememacs/leader-def
  "SPC" #'execute-extended-command
  "bb" #'consult-buffer
  "bB" #'consult-buffer-other-window
  "s" '(:ignore t :which-key "search")
  "ss" #'consult-line
  "sS" #'consult-line-multi
  "ff" #'find-file
  "fr" #'consult-recent-file

  "ji" #'consult-imenu
  "jI" #'consult-imenu-multi
  ;; info
  ;; pass
  "m" #'consult-global-mark

  "jL" #'consult-goto-line
  "jo" #'consult-org-heading
  "jO" #'consult-outline
  ;; org-agenda
  "sb" #'consult-multi-occur

  ":" #'consult-complex-command

  "ha" #'consult-apropos

  "e" nil
  "en" #'consult-compile-eror
  ;; flycheck
  ;; "ef" #'consult-flymake

  ;; todo ripgrep
  "/" #'consult-ripgrep
  )
 ;; (consult-grep)


 ;; (consult-imenu)

 ;; (consult-ripgrep)
 ;; (consult-buffer)
 ;; (consult-yank-from-kill-ring "fo")

(general-def
  "H-SPC" #'consult-line
  "H-m ." (lambda () (interactive) (push-mark)))

(general-def
  :keymap vertico-map
  "M-y" #'consult-yank-pop)

(consult-customize
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-bookmark consult--source-recent-file
 consult--source-project-recent-file
 :preview-key (kbd "M-."))

(provide 'init-consult)
