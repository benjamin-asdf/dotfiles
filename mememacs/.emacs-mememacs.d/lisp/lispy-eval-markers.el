;;; lispy-eval-markers.el --- Remember last lispy eval markers

;;; Commentary:
;;

;;; Code:

(defvar mm/lispy-eval-marker-hist (make-ring 15))

(defun mm/remember-lispy-eval-point (&rest _)
  (ring-insert
   mm/lispy-eval-marker-hist
   (point-marker)))

(advice-add 'lispy-eval :before #'mm/remember-lispy-eval-point)
(advice-add 'cider-eval-last-sexp :before #'mm/remember-lispy-eval-point)
(advice-add 'cider-pprint-eval-last-sexp :before #'mm/remember-lispy-eval-point)


(defun mm/lispy-eval-mark-last-or-consult (&optional arg)
  "Jump to last `lispy-eval` marker.
This pops the marker from the ring.
With prefix ARG select with consult from the last markers."
  (interactive "P")
  (when (ring-empty-p mm/lispy-eval-marker-hist)
    (user-error "No lispy eval marks"))
  (if arg
      (progn
	(consult-mark
	 (ring-elements
	  mm/lispy-eval-marker-hist))
	(evil-insert-state))
    (consult--jump (ring-remove mm/lispy-eval-marker-hist))))

(general-def
  'lispy-mode-map
  :states '(normal visual emacs)
  "ge"
  #'mm/lispy-eval-mark-last-or-consult)

(provide 'lispy-eval-markers)

;;; lispy-eval-markers.el ends here
