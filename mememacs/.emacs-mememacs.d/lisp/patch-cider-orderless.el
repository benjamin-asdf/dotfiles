
(defun mm/cider-complete-at-point ()
  "Complete the symbol at point."
  (when (and (cider-connected-p)
	     (not (cider-in-string-p)))
    (when-let*
	((bounds
	  (bounds-of-thing-at-point
	   'symbol))
	 (beg (car bounds))
	 (end (cdr bounds))
	 (completion
	  (append
	   (cider-complete
	    (buffer-substring beg end))
	   (get-text-property (point) 'cider-locals))))
      (list
       beg
       end
       (completion-table-dynamic
	(lambda (_) completion))
       :annotation-function #'cider-annotate-symbol))))

(advice-add 'cider-complete-at-point :override #'mm/cider-complete-at-point)

(provide 'patch-cider-orderless)
