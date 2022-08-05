;; -*- lexical-binding: t; -*-

(setq cider-repl-display-help-banner nil
      cider-repl-display-in-current-window t
      cider-babashka-parameters "nrepl-server 0"
      cider-repl-require-ns-on-set t
      cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow"
      clojure-toplevel-inside-comment-form t
      cider-scratch-initial-message ";; It's not funny, it's powerfull"
      cider-eldoc-display-context-dependent-info t
      cider-clojure-cli-aliases "lib/hotload")

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

  "e" 'cider-eval-commands-map

  "l" 'cider-doc-map
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

(defun mm/cider-emit-into-popup-buffer (out)
  (cider-emit-into-popup-buffer
   (cider-popup-buffer
    "*mm-lispy-result*"
    nil
    major-mode
    'ancillary)
   (ansi-color-apply out)
   nil
   t))

(defadvice lispy-eval (around cider-lispy-eval (&optional arg) activate)
  (if (memq
       major-mode
       lispy-clojure-modes)
    (if (eq arg 1)
	(cider-eval-last-sexp nil)
      (progn (lispy-newline-and-indent-plain)
	     (cider-eval-last-sexp t)))
    (if (eq arg 4)
      (lispy-eval-and-insert 'insert)
      ad-do-it)))

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
    (if arg
	ad-do-it
      (mm/cider-emit-into-popup-buffer (lispy--eval-dwim)))))

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
  "r" 'clojure-refactor-map
  "m" #'cider-macroexpand-1-inplace
  "s" nil
  "sl" #'cider-inspect-last-result
  "sd" #'cider-inspect-defun-at-point)

(general-def
  :states '(insert normal)
  :keymaps '(cider-inspector-mode-map)
  "d" #'cider-inspector-def-current-val)

(with-eval-after-load 'flycheck
  (mememacs/local-def
    :keymaps mm/cider-mode-maps
    "e"
    (let ((map flycheck-command-map))
      (define-key map "N" #'cider-jump-to-compilation-error)
      map)))


;; patch for nbb

(cider-register-cljs-repl-type 'nbb "(+ 1 2 3)")

(defun mm/cider-connected-hook ()
  (when (eq 'nbb cider-cljs-repl-type)
    (setq-local cider-show-error-buffer nil)
    (cider-set-repl-type 'cljs)))
(add-hook 'cider-connected-hook #'mm/cider-connected-hook)

;; thank you corgi

(defun corgi/cider-jack-in-babashka (&optional force)
  "Start a utility CIDER REPL backed by Babashka, not related to a
specific project."
  (interactive)
  (when-let ((cur (get-buffer "*babashka-scratch*")))
    (if (or force (y-or-n-p "Kill current bb scratch? "))
	(kill-buffer cur)
	(user-error "There is already a bb scratch.")))
  (let ((project-dir "~/scratch/"))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buffer)
       (cider-nrepl-connect
        (list :repl-buffer server-buffer
              :repl-type 'clj
              :host (plist-get nrepl-endpoint :host)
              :port (plist-get nrepl-endpoint :port)
              :project-dir project-dir
              :session-name "babashka"
              :repl-init-function (lambda ()
                                    (setq-local cljr-suppress-no-project-warning t
                                                cljr-suppress-middleware-warnings t)
                                    (rename-buffer "*babashka-scratch*"))))))))



(defun mememacs/babashka-scratch (&optional arg)
  (interactive)
  (with-current-buffer
      (mm/scratch arg "clj")
    (corgi/cider-jack-in-babashka)))

(mememacs/leader-def "cb" #'mememacs/babashka-scratch)

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

(defun mm/cider-barf-unless-connected (&rest _)
  (unless (cider-connected-p)
    (user-error "Cider not connected")))

(advice-add 'cider-interactive-eval :before #'mm/cider-barf-unless-connected)
(advice-add 'cider-load-buffer :before #'mm/cider-barf-unless-connected)


;; ---

(defun mm/bb (form)
  (car (read-from-string
	(shell-command-to-string
	 (format
	  "bb -e '%s'"
	  (princ form))))))

(defun mm/cider-jack-in-with-an-alias-from-deps ()
  (interactive)
  (if-let* ((root (project-root (project-current t)))
	    (_ (file-exists-p (expand-file-name "deps.edn" root))))
      (let* ((default-directory root)
	     (my-alias
	      (completing-read
	       "Cider jack in with alias: "
	       (read-from-string
		(shell-command-to-string
		 (format "bb -e '%s'" (princ '(->> (slurp "\"deps.edn\"") read-string :aliases keys (map name))))))))
	     (cider-clojure-cli-aliases
	      (concat
	       cider-clojure-cli-aliases ":" my-alias)))
	(call-interactively #'cider-jack-in-clj))
    (user-error "no deps.edn file in project")))


(defun clerk-show ()
  (interactive)
  (save-buffer)
  (when-let
      ((filename
        (buffer-file-name)))
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(mememacs/local-def
  :keymaps mm/cider-mode-maps
  "c" #'clerk-show)




;; I think this functionality is ultra silly
;; as if I run out of buffers to create
(defun mm/cleanup-cider-repls-and-do-not-reuse (&rest _)
  (let ((repls (seq-filter
		(lambda (b)
		  (with-current-buffer
		      b
		    (and (derived-mode-p
			  'cider-repl-mode)
			 (not (process-live-p
			       (get-buffer-process b))))))
		(buffer-list))))
    (mapc #'kill-buffer repls)
    '()))

(advice-add #'cider--choose-reusable-repl-buffer :override #'mm/cleanup-cider-repls-and-do-not-reuse)

(defun mm/cider-connect-arcadia ()
  (interactive)
  (let ((port
	 (or
	  (when-let*
	      ((d (project-root (project-current)))
	       (default-directory d)
	       (f (expand-file-name "configuration.edn"))
	       (f (when (file-exists-p f) f)))
	    (mm/bb `(-> (slurp ,(format "\"%s\"" f)) read-string :nrepl)))
	  3722)))
    (cider-connect-clj `(:host "localhost" :port ,port))))

(use-package gdscript-mode
  :config
  (setf gdscript-godot-executable "godot-mono" gdscript-docs-use-eww nil))

(provide 'init-cider)
