
(setq cider-repl-display-help-banner nil
      cider-repl-require-ns-on-set t
      clojure-toplevel-inside-comment-form t)

(add-hook
 'mememacs/escape-functions
 (defun mm/cider-macroexpand-undo ()
   (when (in major-mode 'cider-mode 'cider-repl-mode)
     (cider-macroexpand-undo))))

(add-hook
 'cider-connected-hook
 (lambda ()
   (message
    (funcall
     cider-connection-message-fn))))

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

;; (with-eval-after-load 'lispy
;;   (setf
;;    cider-jack-in-dependencies
;;    (delete-dups
;;     (append
;;      cider-jack-in-dependencies
;;      (assoc-delete-all "nrepl" lispy-cider-jack-in-dependencies #'equal)))))

;; jet -------------------

(defun jet-on-region (beg end)
  (interactive "r")
  (let ((s (buffer-substring beg end)))
    (with-current-buffer-window
	"jet-edn"
	nil nil
      (clojure-mode)
      (process-send-string
       (start-process
	"jet"
	(current-buffer)
	"jet"
	"--from"
	"json"
	"--pretty"
	"-k"
	"-")
       s))))

(defun jet-send-query ()
  (interactive)
  (let ((in-file (buffer-file-name)))
    (pop-to-buffer
     (process-buffer
      (start-process-shell-command
       "jet"
       (get-buffer-create "jet")
       (format
	"jet -k -i json < %s -q '%s'"
	in-file
	(with-current-buffer
	    (get-buffer-create "jet-query")
	  (clojure-mode)
	  (pop-to-buffer
	   (current-buffer))
	  (buffer-string))))))))

;; --------------------------------------------


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


;; ---- functions


(defun mememacs/cider-A-build ()
  (interactive)
  (let ((cider-clojure-cli-global-options "-A:build"))
    (cider)))


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
