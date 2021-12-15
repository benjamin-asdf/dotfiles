;; https://github.com/clojure-emacs/cider/issues/3019
;; basically ciders native completion support needs to be fixed
(defun orderless-prefix-dispatch (component idx _total)
  (and (= idx 0)
       (not (s-starts-with? " " component))
       `(orderless-regexp . ,(concat "^" (regexp-quote component)))))

(defun mm/patch-orderless-style ()
  (setq-local
   orderless-style-dispatchers
   '(orderless-prefix-dispatch)))


(defun mememacs/c-completion ()
    (interactive)
    (corfu-quit)
    (let ((completion-in-region-function #'consult-completion-in-region)
	  (minibuffer-setup-hook
	   (if cider-mode
	       (append
		minibuffer-setup-hook
		'(mm/patch-orderless-style))
	     minibuffer-setup-hook)))
      (completion-at-point)))

(provide 'patch-cider-orderless)
