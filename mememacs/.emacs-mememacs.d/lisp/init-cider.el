
(setq cider-repl-display-help-banner nil
      cider-repl-require-ns-on-set t
      clojure-toplevel-inside-comment-form t
      cider-scratch-initial-message ";; It's not funny, it's powerfull"
      cider-clojure-cli-aliases
      "lib/hotload")

(add-hook
 'mememacs/escape-functions
 (defun mm/cider-macroexpand-undo ()
   (when (in major-mode 'cider-mode 'cider-repl-mode)
     (cider-macroexpand-undo))))

(defvar mm/cider-mode-maps
  '(cider-mode-map
    cider-repl-mode-mapl
    cider-macroexpansion-mode-map))

(when
    (require 'neil "~/repos/clojure/neil/neil.el" 'no-err)
  (mememacs/comma-def
    :keymaps
    '(clojure-mode-map cider-repl-mode clojurescript-mode)
    "nn" #'neil-find-clojure-package))

(mememacs/comma-def
  :keymaps
  '(clojure-mode-map cider-repl-mode clojurescript-mode)
  "m" #'macrostep-expand

  "e" '(cider-eval-commands-map
	:which-key "eval")

  "l" '(cider-doc-map :which-key "cider doc")
  "h," #'cider-drink-a-sip
  "k" #'cider-load-buffer
  "hh" #'cider-clojuredocs)

(general-def
  :keymaps mm/cider-mode-maps
  :states '(normal motion)
  "gd" (lambda (&optional arg)
	 (interactive "P")
	 (cider-find-var (not arg)))
  "H-k" #'cider-load-buffer)


(general-def
  'cider-eval-commands-map
  "L" #'cider-eval-sexp-at-point
  "l" #'mememacs/lispy-eval-line)

(defadvice lispy-eval (around cider-lispy-eval (&optional arg) activate)
  (if (memq
       major-mode
       lispy-clojure-modes)
      (save-excursion
	(goto-char (cdr (lispy--bounds-dwim)))
	(if (eq arg 1)
	    (cider-eval-last-sexp nil)
	  (cider-eval-last-sexp t)))
    ad-do-it))

(defadvice lispy-eval-and-insert (around cider-lispy-eval (&optional arg) activate)
  (if (memq
       major-mode
       lispy-clojure-modes)
      (save-excursion
	(goto-char
	 (cdr (lispy--bounds-dwim)))
	(if arg
	    (cider-pprint-eval-last-sexp-to-comment
	     nil)
	  (cider-pprint-eval-last-sexp
	   nil)))
    ad-do-it))

;; --------------------------------------------

(defun lein-deps-to-deps (beg end)
  (interactive "r")
  (replace-regexp-in-region
   "\\[\\(.+?\\)\\s-\\(\".+?\"\\)\\]"
   "\\1 {:mvn/version \\2}"
   beg
   end)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp-in-region
     "[^/]\\(\\<\\w+\\>\\) {:mvn/version \\(.+\\)}"
     "\\1/\\1 {:mvn/version \\2}")))

(mememacs/local-def
  :keymaps mm/cider-mode-maps
  "r" '(clojure-refactor-map
	:which-key "refactor")
  "m" #'cider-macroexpand-1-inplace
  "s" '(:ignore t
		:which-key "show etc")
  "sl" #'cider-inspect-last-result
  "sd" #'cider-inspect-defun-at-point)

(general-def
  :states '(insert normal)
  :keymaps '(cider-inspector-mode-map)
  "d" #'cider-inspector-def-current-val)

(with-eval-after-load 'flycheck
  (mememacs/local-def
    :keymaps mm/cider-mode-maps

    "e" `(,(let ((map flycheck-command-map))
	     (define-key map "N" #'cider-jump-to-compilation-error)
	     map)
	  :which-key "flycheck")))


;; patch for nbb

(cider-register-cljs-repl-type 'nbb "(+ 1 2 3)")

(defun mm/cider-connected-hook ()
  (when (eq 'nbb cider-cljs-repl-type)
    (setq-local cider-show-error-buffer nil)
    (cider-set-repl-type 'cljs)))
(add-hook 'cider-connected-hook #'mm/cider-connected-hook)


;; ---- functions



;;  --- portal

(defun cider-portal/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(require 'portal.api) (portal.api/tap) (portal.api/open)"))

(defun cider-portal/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun cider-portal/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(defun cider-portal/last-result ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(tap> *1)"))

(provide 'init-cider)
