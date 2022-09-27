;;;  -*- lexical-binding: t; -*-

(mememacs/comma-def
  :keymaps
  '(emacs-lisp-mode-map
    lisp-interaction-mode-map)
  "e" nil
  "ed" #'eval-defun
  "ee" #'mememacs/eval-last-sexp-dwim)

(general-def
  :states '(normal)
  :keymap
  'emacs-lisp-mode-map
  "K" #'helpful-at-point
  "gr" #'revert-buffer)

(mememacs/comma-def
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "hh" #'helpful-at-point)

(provide 'init-emacs-lisp)
