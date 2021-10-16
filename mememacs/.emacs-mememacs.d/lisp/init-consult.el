(require 'patch-consult)

(general-def
   :states '(insert)
   "C-j" #'company-manual-begin)

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
  'vertico-map
  "M-h" #'consult-history
  "M-i" #'completion-at-point)

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

  "jL" #'consult-goto-line
  "jo" #'consult-org-heading
  "jO" #'consult-outline
  ;; org-agenda
  "sb" #'consult-multi-occur

  "mM" #'consult-register-store
  "mm" #'consult-register
  "mb" #'consult-bookmark
  ":" #'consult-complex-command

  "ha" #'consult-apropos

  "e" nil
  "en" #'consult-compile-eror
  ;; flycheck
  ;; "ef" #'consult-flymake
  )
 ;; (consult-grep)


 ;; oh bois this is good
 ;; (consult-line)
 ;; (consult-imenu)

 ;; (consult-ripgrep)
 ;; (consult-buffer)
 ;; (consult-yank-from-kill-ring "fo")

(general-def
  :keymap vertico-map
  "M-y" #'consult-yank-pop)

(provide 'init-consult)
