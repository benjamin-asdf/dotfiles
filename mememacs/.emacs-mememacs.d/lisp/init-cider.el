;; -*- lexical-binding: t; -*-

(setq cider-repl-display-help-banner nil
      cider-repl-display-in-current-window t
      cider-font-lock-reader-conditionals nil
      cider-babashka-parameters "nrepl-server 0"
      cider-repl-require-ns-on-set t
      cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow"
      clojure-toplevel-inside-comment-form t
      cider-scratch-initial-message ";; It's not funny, it's powerfull"
      cider-eldoc-display-context-dependent-info t
      cider-clojure-cli-aliases ":lib/tools-deps+slf4j-nop:trace/flowstorm"
      ;; cider-merge-sessions nil
            cider-merge-sessions 'project

      cider-auto-jump-to-error nil
      cider-show-error-buffer nil
      cider-use-overlays t)

(defvar mm/cider-mode-maps
  '(cider-mode-map
    cider-repl-mode-mapl
    cider-macroexpansion-mode-map))

(set-face-attribute
 'cider-result-overlay-face
 nil
 :background "black")

(set-face-attribute
 'cider-error-overlay-face
 nil
 :background "DarkGreen"
 :foreground "white")


(define-key cider-mode-map (kbd "C-c X") #'cider-selector)

(require 'neil "~/repos/clojure/neil/neil.el" 'no-err)

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

(defun mememacs/babashka-scratch (&optional arg)
  (interactive)
  (with-current-buffer
      (mm/scratch arg "clj")
    (corgi/cider-jack-in-babashka)))

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




(advice-add
 #'cider-eldoc :before-while
 (defun mm/not-when-cider-debug-active (&rest _)
   (not cider--debug-mode)))

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
	       (f (or (expand-file-name "configuration.edn")
		      (expand-file-name "arcadia-configuration.edn")
		      (expand-file-name "Assets/configuration.edn")))
	       (f (when (file-exists-p f) f)))
	    (mm/bb `(-> (slurp ,(format "\"%s\"" f)) read-string :nrepl)))
	  3722)))
    (cider-connect-clj `(:host "localhost" :port ,port))))

(use-package gdscript-mode
  :when nil
  :config
  (setf
   gdscript-godot-executable "godot-mono"
   gdscript-docs-use-eww nil))

(bind-keys
 :map cider-mode-map
 ("C-, m" . macrostep-expand)
 ("C-, k" . cider-doc)
 ("C-, h" . cider-clojuredocs))

;; happens to me all time I get a compilation error for the toplevel.
;; i.e. fo
;; if you eval a compilation error via evel sexp etc, same thing
;; and it jumps to the top
;; even though that does not help me
(advice-add
 #'cider-jump-to
 :before-while
 (defun mm/cider-do-not-jump-if-pos-1-same-buffer
     (buffer &optional pos other-window)
   (or
    (not (equal buffer (current-buffer)))
    (not (eq pos 1)))))

(defun clojure-insert-ns-form ()
  "Insert a namespace form at the beginning of the buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (if
      ;; when I already have a ns form,
      ;; TODO make it replace the ns name
      (re-search-forward "(ns\\s-" nil t)
      (progn
        (insert " "
                (funcall
                 clojure-expected-ns-function)))
    (clojure-insert-ns-form-at-point)))

(defun mm/pop-cider-error ()
  (interactive)
  (if-let ((cider-error (get-buffer "*cider-error*")))
      (pop-to-buffer cider-error)
    (message "no cider error buffer")))

(define-key cider-mode-map (kbd "C-c E") #'mm/pop-cider-error)

(defun mm/clojure-add-libs-snippet ()
  (interactive)
  (insert
   "(comment
 (require '[clojure.tools.deps.alpha.repl :refer [add-libs]]))
  (add-libs
   '{org.sg.get-currency-conversions/get-currency-conversions
     {:local/root \"../get-currency-conversions/\"}})"))

(defun mm/clojure-dir-locals-snippet ()
  (interactive)
  (with-current-buffer
      (find-file (expand-file-name ".dir-locals.el" (project-root (project-current))))
    (insert
     "((nil .
      ((cider-clojure-cli-aliases . \"dev\")
       (cider-preferred-build-tool . clojure-cli))))
")))

;; (defun mm/cider-clojure-load-deps ()
;;   (interactive)
;;   (cider-interactive-eval
;;    (with-current-buffer
;;        (find-file-noselect
;;         (expand-file-name "deps.edn"
;;                           (project-root (project-current))))
;;      (goto-char (point-min))
;;      (let ((m (parseedn-read)))
;;        (defvar foo
;;          (parseedn-print-          (gethash :deps m))))))
;;   (insert
;;    "(comment
;;  (require '[clojure.tools.deps.alpha.repl :refer [add-libs]]))
;;   (add-libs
;;    '{org.sg.get-currency-conversions/get-currency-conversions
;;      {:local/root \"../get-currency-conversions/\"}})"))


;; I want this to always make invisible messages
;; when I run shadow interactively this would end up printing all stderr

(defun cider--display-interactive-eval-result (value &optional point overlay-face)
  "Display the result VALUE of an interactive eval operation.
VALUE is syntax-highlighted and displayed in the echo area.
OVERLAY-FACE is the face applied to the overlay, which defaults to
`cider-result-overlay-face' if nil.
If POINT and `cider-use-overlays' are non-nil, it is also displayed in an
overlay at the end of the line containing POINT.
Note that, while POINT can be a number, it's preferable to be a marker, as
that will better handle some corner cases where the original buffer is not
focused."
  (let* ((font-value (if cider-result-use-clojure-font-lock
                         (cider-font-lock-as-clojure value)
                       value))
         (font-value (string-trim-right font-value))
         (used-overlay (when (and point cider-use-overlays)
                         (cider--make-result-overlay font-value
                           :where point
                           :duration cider-eval-result-duration
                           :prepend-face (or overlay-face 'cider-result-overlay-face)))))
    (message
     "%s"
     (propertize (format "%s%s" cider-eval-result-prefix font-value)
                 ;; The following hides the message from the echo-area, but
                 ;; displays it in the Messages buffer. We only hide the message
                 ;; if the user wants to AND if the overlay succeeded.
                 'invisible t))))


(add-hook 'clojure-mode-hook #'ambrevar/turn-on-delete-trailing-whitespace)

(add-hook
'clojure-mode-hook
(defun mm/add-keywords-to-imenu ()
  (add-to-list 'imenu-generic-expression '(nil "^.?.?\\(:[^ ]+\\).*$" 1) t)))

(with-eval-after-load 'consult

  (defvar cider-consult-repl-buffer-source
    `(:name     "Repl Buffer"
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :action   ,#'consult--buffer-action
                :items
                ,(lambda ()
                   (let ((sessions
                          (or (sesman--linked-sessions 'CIDER 'sort)
                              (sesman--friendly-sessions 'CIDER 'sort))))
                     (mapcar #'buffer-name (mapcar #'cadr sessions)))))
    "Candidate source for `cider-repl-consult'.")

  (defun cider-repl-consult ()
    (interactive)
    (consult-buffer
     (list
      cider-consult-repl-buffer-source))))

(defun cider-popup-eval-handler (&optional buffer)
  "Make a handler for printing evaluation results in popup BUFFER.
This is used by pretty-printing commands."
  ;; NOTE: cider-eval-register behavior is not implemented here for performance reasons.
  ;; See https://github.com/clojure-emacs/cider/pull/3162
  (let ((target-buff (current-buffer))
        (end (line-end-position)))
    (nrepl-make-response-handler
     (or buffer (current-buffer))
     (lambda (buffer value)
       (cider-emit-into-popup-buffer buffer (ansi-color-apply value) nil t)
       (display-buffer buffer))
     (lambda (_buffer out)
       (cider-emit-interactive-eval-output out))
     (lambda (_buffer err)
       (pop-to-buffer target-buff)
       (when (or (not cider-show-error-buffer)
                 (not (cider-connection-has-capability-p 'jvm-compilation-errors)))

         ;; Display errors as temporary overlays
         (let ((cider-result-use-clojure-font-lock nil))
           (cider--display-interactive-eval-result
            err
            end
            'cider-error-overlay-face)))
       
       ;; here
       (cider-emit-interactive-eval-err-output err))
     nil
     nil
     nil
     (lambda (buffer warning)
       (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t)))))

(defun cider--pprint-eval-form (form)
  "Pretty print FORM in popup buffer."
  (let* ((buffer (current-buffer))
         (result-buffer (cider-make-popup-buffer cider-result-buffer 'clojure-mode 'ancillary))
         (handler (cider-popup-eval-handler
                   result-buffer)))
    (with-current-buffer
        buffer
      (cider-interactive-eval
       (when (stringp form) form)
       handler
       (when (consp form) form)
       (cider--nrepl-print-request-map
        fill-column)))))

(defun mm/zprint-region ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "zprint" (buffer-name) t)))

;; isn't the mental surface area of retrieving a symbol from memory a fascinating concept
(defalias 'mm/clojure-pprint-region-fmt-cljfmt-zprint-pretty-clj-print-format-fmt-buffer #'mm/zprint-region)

(provide 'init-cider)
