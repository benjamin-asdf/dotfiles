;; REVIEW: Remove once merged upstream:
;; https://github.com/abo-abo/lispy/pull/574.
(dolist (mode '(slime-repl-mode slime-mrepl-mode sly-mrepl-mode))
  (push `(,mode le-lisp lispy--eval-lisp) lispy-eval-alist))

;; REVIEW: Remove this workaround when upstream has fixed
;; https://github.com/abo-abo/lispy/pull/575.
(push '(sly-mrepl-mode . ("[[:space:]]" "[#`',.@]+" "#[0-9]*" "#[.,Ss+-]" "#[0-9]+[=Aa]"))
      lispy-parens-preceding-syntax-alist)

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
    (let*
	((lispy-delete-at-paren
          (when
              lispy-mode
            (memq (char-after) '(?\( ?\[ ?\{))))
	 ;; keep the hurr when you do (subst hurr)
	 (lispy-only-subst-operator
	  (when lispy-delete-at-paren
	    (<
	     1
	     (let ((bounds (lispy--bounds-dwim)))
	       (save-excursion
		 (goto-char (car bounds))
		 (let ((symbol-count 0))
		   (while
		       (re-search-forward
			"[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)"
			(cdr bounds)
			t)
		     (setf symbol-count (1+ symbol-count)))
		   symbol-count))))))
	 (subst
	  (if lispy-only-subst-operator
	      (let* ((current-bounds (lispy--bounds-dwim))
		     (current (buffer-substring
			       (car current-bounds)
			       (cdr current-bounds)))
		     (new-atom
		      (with-temp-buffer
			(insert subst)
			(goto-char (point-min))
			(re-search-forward "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)")
			(let ((b (lispyville--bounds-atom)))
			  (buffer-substring (car b) (cdr b))))))
		(with-temp-buffer
		  (insert current)
		  (goto-char (point-min))
		  (re-search-forward "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)")
		  (let ((b (lispyville--bounds-atom)))
		    (delete-region (car b) (cdr b))
		    (insert new-atom))
		  (buffer-string)))
	    subst)))
      (insert subst)
      (if
          lispy-delete-at-paren
          (lispy-delete 1)
        (delete-region (point) he-string-end))
      (goto-char newpos)
      (when lispy-delete-at-paren
        (special-lispy-different)
        (when (looking-at-p "\\w")
          (insert " ")
          (forward-char -1)
          (evil-insert-state 1))))))

(advice-add 'he-substitute-string :override #'mm/he-substitute-string)

(provide 'patch-lispy)
