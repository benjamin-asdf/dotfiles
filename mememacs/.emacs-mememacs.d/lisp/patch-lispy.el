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
                (cons 'lispy-mode lispy-known-verbs)))
	;; outline syntax is unusual in clojure code
	;; also I would need to change the fonts, green blocks are to heavy.
        ;; (font-lock-add-keywords major-mode lispy-font-lock-keywords)
	)
    (when lispy-old-outline-settings
      (setq outline-regexp (car lispy-old-outline-settings))
      (setq outline-level (cdr lispy-old-outline-settings))
      (setq lispy-old-outline-settings nil))
    (font-lock-remove-keywords major-mode lispy-font-lock-keywords)))


;; hippie

(defun mm/he-substitute-string (str &optional trans-case)
  (let ((trans-case (and trans-case
                         case-replace
                         case-fold-search))
        (newpos (point-marker))
        (subst ()))
    (goto-char he-string-beg)
    (setq subst (if trans-case (he-transfer-case he-search-string str) str))
    (setq he-tried-table (cons subst he-tried-table))
    (let ((lispy-delete-at-paren
           (when
               lispy-mode
             (memq (char-after) '(?\( ?\[ ?\{)))))
      (if
          lispy-delete-at-paren
          (lispy-delete 1)
        (delete-region (point) he-string-end))
      (insert subst)
      (goto-char newpos)
      (when lispy-delete-at-paren
        (special-lispy-different)
        (when (looking-at-p "\\w")
          (insert " ")
          (forward-char -1)
          (evil-insert-state 1))))))

(advice-add 'he-substitute-string :override #'mm/he-substitute-string)

(provide 'patch-lispy)
