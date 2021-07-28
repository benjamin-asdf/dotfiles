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

(provide 'patch-lispy)
