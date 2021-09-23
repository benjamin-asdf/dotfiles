
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

(defvar mm/cider-mode-maps
  '(cider-mode-map
    cider-repl-mode-mapl
    cider-macroexpansion-mode-map))


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
  :keymaps mm/cider-mode-maps
  :states '(normal motion)
  "gd" (lambda (&optional arg)
	 (interactive "P")
	 (cider-find-var (not arg))))


(general-def
  'cider-eval-commands-map
  "L" #'cider-eval-sexp-at-point
  "l" #'mememacs/lispy-eval-line)


(defadvice lispy-eval (around cider-lispy-eval (&optional arg) activate)
  (if (memq
       major-mode
       lispy-clojure-modes)
      (if (eq arg 1)
	  (cider-eval-last-sexp nil)
	(cider-eval-last-sexp t))
    ad-do-it))

(defadvice lispy-eval-and-insert (around cider-lispy-eval (&optional arg) activate)
  (if (memq
       major-mode
       lispy-clojure-modes)
      (if arg
	  (cider-pprint-eval-last-sexp-to-comment nil)
	(cider-pprint-eval-last-sexp nil))
    ad-do-it))



(mememacs/local-def
  :keymaps mm/cider-mode-maps
  "r"
  '(clojure-refactor-map
    :which-key "refactor")
  "m"
  #'cider-macroexpand-1-inplace
  "s"
  '(:ignore t
	    :which-key "show etc")
  "sl"
  #'cider-inspect-last-result)


(with-eval-after-load 'flycheck
  (mememacs/local-def
    :keymaps mm/cider-mode-maps

    "e" `(,(let ((map flycheck-command-map))
	     (define-key map "N" #'cider-jump-to-compilation-error)
	     map)
	  :which-key "flycheck")))

;; todo convert all lispy eval and stuff to cider
;; so we do not use lispy clojure at all


(defun mememacs/bb-server ()
  (interactive)
  (start-process
   "*bb-server*"
   "*bb-server*"
   "bb"
   "nrepl-server"
   "1667"))


(defun mememacs/bb-cider ()
  (interactive)
  (cider-connect '(:host "localhost" :port 1667)))


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
