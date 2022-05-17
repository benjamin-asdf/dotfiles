;; https://github.com/clojure-emacs/cider/issues/3019  -*- lexical-binding: t; -*-
;; basically ciders native completion support needs to be fixed

(defun orderless-prefix-dispatch (component idx _total)
  (and (= idx 0)
       (not (s-starts-with? " " component))
       `(orderless-regexp . ,(concat "^" (regexp-quote component)))))

(defun mm/patch-orderless-style ()
  (setq-local
   orderless-style-dispatchers
   '(orderless-prefix-dispatch)))


;; FIXME: why..
;; completion in region somehow ends up calling cider with blank prefix
;; still in the dark why
(defun cider-complete-at-point ()
  "Complete the symbol at point."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (cider-connected-p)
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic
             (let ((res))
               (lambda (prefix)
                 (or res
                     (setf res
                           (cider-complete prefix))))))
            :annotation-function #'cider-annotate-symbol))))


(add-hook 'cider-mode-hook #'mm/patch-orderless-style)

(provide 'patch-cider-orderless)


;; ;; This is the function that breaks apart the pattern.  To signal that
;; ;; an element is a package prefix, we keep its trailing "/" and return
;; ;; the rest as another pattern.
;; (defun cider-orderless-component-separator (pattern)
;;   (if (cider-connected-p)
;;       (let ((slash-idx (string-match-p "/" pattern)))
;;         (if slash-idx
;;             (append (list (substring pattern 0 (1+ slash-idx)))
;;                     (split-string (substring pattern (1+ slash-idx)) " +" t))
;;           (split-string pattern " +" t)))
;;     (split-string pattern " +" t)))

;; ;; This is the function that takes our package prefix and ensures that
;; ;; it is at the beginning (note the "^" in the regex).
;; (defun cider-orderless-package-prefix (component)
;;   (format "\\(?:^%s\\)" (regexp-quote component)))

;; ;; This is the function that notices that the candidate ends in a "/"
;; ;; and that it should use our `cider-orderless-package-prefix'
;; ;; function for turning the candidate into a regex.
;; (defun cider-package-prefix (pattern _index _total)
;;   (when (and (cider-connected-p) (string-suffix-p "/" pattern))
;;     'cider-orderless-package-prefix))

;; (defun cider-setup-orderless ()
;;   (setq orderless-style-dispatchers '(cider-package-prefix))
;;   (add-to-list 'orderless-matching-styles #'cider-orderless-package-prefix)
;;   (setq orderless-component-separator #'cider-orderless-component-separator))
