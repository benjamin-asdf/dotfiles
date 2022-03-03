;; taken with love from
;; https://github.com/lambdaisland/corgi-packages

(require 'cider)
(require 'lispy)

;; Show emacs-lisp eval results in an overlay, CIDER style.
;; https://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
;; We rely on CIDER to do the heavy lifting, can't seem to find a general library
;; for doing this style of overlays.
(defun corgi/eval-overlay (value point)
  (cider--make-result-overlay
      (if (stringp value)
	  value
	  (format "%S" value))
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (corgi/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (corgi/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (corgi/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

(advice-add 'special-lispy-eval
	    :filter-return
	    (lambda (r)
	      (corgi/eval-overlay r)))

(defun mm/corgi-overlay (value)
  (when value
    (corgi/eval-overlay value (point))))

(advice-add 'lispy-eval :filter-return #'mm/corgi-overlay)

(provide 'corgi-emacs-lisp)
