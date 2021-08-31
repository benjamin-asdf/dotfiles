
(setq cider-repl-display-help-banner nil
      cider-repl-require-ns-on-set t)

(add-hook
 'mememacs/escape-functions
 (defun mm/cider-macroexpand-undo ()
   (when (in major-mode 'cider-mode 'cider-repl-mode)
     (cider-macroexpand-undo))))

(defun mememacs/cider-macroexpand-at-place ()
  (interactive)
  (lispy-forward 1)
  (forward-line 1)
  (cider-macroexpand-1))



(mememacs/comma-def
  :keymaps
  '(clojure-mode-map cider-repl-mode clojurescript-mode)
  "m" #'mememacs/cider-macroexpand-at-place

  "e" '(cider-eval-commands-map
	:which-key "eval")
  "h," #'cider-drink-a-sip)

(general-def
  'cider-eval-commands-map
  "L" #'cider-eval-sexp-at-point
  "l" #'mememacs/lispy-eval-line)


(provide 'init-cider)
