;; REVIEW: Remove once merged upstream:
;; https://github.com/abo-abo/lispy/pull/574.
(dolist (mode '(slime-repl-mode slime-mrepl-mode sly-mrepl-mode))
  (push `(,mode le-lisp lispy--eval-lisp) lispy-eval-alist))

;; REVIEW: Remove this workaround when upstream has fixed
;; https://github.com/abo-abo/lispy/pull/575.
(push '(sly-mrepl-mode . ("[[:space:]]" "[#`',.@]+" "#[0-9]*" "#[.,Ss+-]" "#[0-9]+[=Aa]"))
      lispy-parens-preceding-syntax-alist)

;; REVIEW: https://github.com/abo-abo/lispy/issues/597
;; #:foo: Space should be inserted before when pressing `"`.
;; #foo: Space should NOT be inserted before when pressing `"`.
(defun lispy-quotes (arg)
  "Insert a pair of quotes around the point.

When the region is active, wrap it in quotes instead.
When inside string, if ARG is nil quotes are quoted,
otherwise the whole string is unquoted."
  (interactive "P")
  (let (bnd)
    (cond ((region-active-p)
           (if arg
               (lispy-unstringify)
             (lispy-stringify)))
          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (if arg
               (lispy-unstringify)
             (if (and lispy-close-quotes-at-end-p (looking-at "\""))
                 (forward-char 1)
                 (progn (insert "\\\"\\\""))
               (backward-char 2))))

          (arg
           (lispy-stringify))

          ((lispy-after-string-p "?\\")
           (self-insert-command 1))

          (t
           (lispy--space-unless "^\\|\\s-\\|\\s(\\|[#][^[:space:]():]*")
           (insert "\"\"")
           (unless (looking-at "\n\\|)\\|}\\|\\]\\|$")
             (just-one-space)
             (backward-char 1))
           (backward-char)))))




;; do not add completion functions

(define-minor-mode lispy-mode
  "Minor mode for navigating and editing LISP dialects.

When `lispy-mode' is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], conditionally call commands instead of
self-inserting. The condition (called special further on) is one
of:

- the point is before \"(\"
- the point is after \")\"
- the region is active

For instance, when special, \"j\" moves down one sexp, otherwise
it inserts itself.

When special, [0-9] call `digit-argument'.

When `lispy-mode' is on, \"[\" and \"]\" move forward and
backward through lists, which is useful to move into special.

\\{lispy-mode-map}"
  :keymap lispy-mode-map
  :group 'lispy
  :lighter " LY"
  (if lispy-mode
      (progn
        (require 'eldoc)
        (eldoc-remove-command 'special-lispy-eval)
        (eldoc-remove-command 'special-lispy-x)
        (eldoc-add-command 'lispy-space)
        (setq lispy-old-outline-settings
              (cons outline-regexp outline-level))
        (setq-local outline-level 'lispy-outline-level)
        (cond ((eq major-mode 'latex-mode)
               (setq-local lispy-outline "^\\(?:%\\*+\\|\\\\\\(?:sub\\)?section{\\)")
               (setq lispy-outline-header "%")
               (setq-local outline-regexp "\\(?:%\\*+\\|\\\\\\(?:sub\\)?section{\\)"))
              ((eq major-mode 'clojure-mode)
               ;; (eval-after-load 'le-clojure
               ;;   '(setq completion-at-point-functions
	       ;; 		'(lispy-clojure-complete-at-point
	       ;; 		  cider-complete-at-point)))
               (setq-local outline-regexp (substring lispy-outline 1)))
              ((eq major-mode 'python-mode)
               (setq-local lispy-outline "^#\\*+")
               (setq lispy-outline-header "#")
               (setq-local outline-regexp "#\\*+")
               (setq-local outline-heading-end-regexp "\n"))
              (t
               (setq-local outline-regexp (substring lispy-outline 1))))
        (when (called-interactively-p 'any)
          (mapc #'lispy-raise-minor-mode
                (cons 'lispy-mode lispy-known-verbs))))
    (when lispy-old-outline-settings
      (setq outline-regexp (car lispy-old-outline-settings))
      (setq outline-level (cdr lispy-old-outline-settings))
      (setq lispy-old-outline-settings nil))))



;; fix find-tag-maker-ring not defined

(defun lispy-goto-symbol (symbol)
  "Go to definition of SYMBOL.
SYMBOL is a string."
  (interactive (list (or (thing-at-point 'symbol t)
                         (lispy--current-function))))
  (lispy--remember)
  (deactivate-mark)
  (let ((narrowedp (buffer-narrowed-p)))
    (when narrowedp
      (widen))
    (cond ((memq major-mode lispy-elisp-modes)
           (lispy-goto-symbol-elisp symbol)
           (when narrowedp
             (lispy-narrow 1)))
          (t
           (let ((handler (cdr (assoc major-mode lispy-goto-symbol-alist)))
                 lib)
             (if (null handler)
                 (error "no handler for %S in `lispy-goto-symbol-alist'" major-mode)
               (when (setq lib (cadr handler))
                 (require lib))
               (funcall (car handler) symbol))))))
  ;; in case it's hidden in an outline
  (lispy--ensure-visible))



;; hippie

(defun mememacs/string-chop-suffix-all (s suffix)
  (if (s-ends-with? suffix s)
      (mememacs/string-chop-suffix-all
       (s-chop-suffix suffix s)
       suffix)
    s))

(defun mememacs/patch-he-lispy (args)
  (if lispy-mode
      `(,(thread-first
	  (car args)
	  (mememacs/string-chop-suffix-all ")")
	  (mememacs/string-chop-suffix-all "]")
	  (mememacs/string-chop-suffix-all "}"))
	,(cadr args))
    args))

(advice-add 'he-substitute-string :filter-args #'mememacs/patch-he-lispy)


(provide 'patch-lispy)
