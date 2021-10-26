
;; https://github.com/clojure-emacs/cider/issues/3019
;; basically ciders native completion support needs to be fixed

(with-eval-after-load 'orderless
  (defun orderless-prefix-dispatch (component idx _total)
    (and (= idx 0)
	 (not (s-starts-with? " " component))
	 `(orderless-regexp . ,(concat "^" (regexp-quote component)))))

  (setq orderless-style-dispatchers
	'(orderless-prefix-dispatch)
	completion-styles '(orderless))


  (defun mm/patch-orderless-style ()
    (setq-local
     orderless-style-dispatchers
     '(orderless-prefix-dispatch)))

  (add-hook
   'cider-mode-hook
   'mm/patch-orderless-style))

(provide 'patch-cider)
