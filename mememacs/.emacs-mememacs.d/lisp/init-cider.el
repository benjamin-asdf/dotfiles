
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

  "l" '(cider-doc-map :which-key "cider doc")
  "h," #'cider-drink-a-sip
  ;; "c"

  "hh" #'cider-clojuredocs)

(general-def
  :keymaps '(cider-mode-map
	     cider-repl-mode-mapl
	     cider-macroexpansion-mode-map)
  :states '(normal motion)
  "gd" (lambda (&optional arg)
	 (interactive "P")
	 (cider-find-var (not arg))))


(general-def
  'cider-eval-commands-map
  "L" #'cider-eval-sexp-at-point
  "l" #'mememacs/lispy-eval-line)


(defadvice lispy-eval
    (around cider-lispy-eval (arg) activate)
  (if (memq major-mode lispy-clojure-modes)
      (cider-eval-last-sexp (not arg))
    ad-do-it))

;; todo convert all lispy eval and stuff to cider
;; so we do not use lispy clojure at all




(provide 'init-cider)



;; (defun my-cider-eval-last-sexp-and-kill ()
;;   "Evaluate the expression preceding point."
;;   (interactive)
;;   (cider-interactive-eval nil
;; 			  (my-cider-eval-kill-handler)
;;                           (cider-last-sexp 'bounds)
;;                           (cider--nrepl-pr-request-map)))


;; (defun my-cider-eval-kill-handler ()
;;   (nrepl-make-response-handler
;;    (current-buffer)
;;    (lambda (buffer value)
;;      (message "Copied %s as kill" value)
;;      (kill-new value))
;;    (lambda (_buffer out)
;;      (cider-emit-interactive-eval-output out))
;;    (lambda (_buffer err)
;;      (cider-emit-interactive-eval-err-output err))
;;    '()))



;; (defadvice
;;     cider--display-interactive-eval-result
;;     (before my-cider-kill-result-adv (value &optional point) activate)
;;   (kill-new value))
