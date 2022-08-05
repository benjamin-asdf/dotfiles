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


(provide 'init-general)
