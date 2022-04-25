;;;  -*- lexical-binding: t; -*-

(mememacs/comma-def
  :keymaps '(emacs-lisp-mode-map
	     lisp-interaction-mode-map)
  "e"
  `(,(let ((map (make-sparse-keymap "emacs-lisp")))
       (general-def
	 map
	 "l" #'mememacs/lispy-eval-line
	 "d" #'eval-defun
	 "b" #'eval-buffer
	 "D" #'edebug-defun
	 "e" #'mememacs/eval-last-sexp-dwim)
       map)
    :which-key "emacs lisp"))


(general-def
  :states '(normal)
  :keymap
  'emacs-lisp-mode-map
  "K"
  #'helpful-at-point)

(mememacs/comma-def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "hh" #'helpful-at-point)

(provide 'init-emacs-lisp)
