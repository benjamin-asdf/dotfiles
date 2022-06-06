;;; lispy-eval-markers.el --- Remember last lispy eval markers

;;; Commentary:
;;

;;; Code:

(defvar mm/lispy-eval-marker-hist (make-ring 5))

(defun mm/remember-lispy-eval-point (&rest _)
  (ring-insert
   mm/lispy-eval-marker-hist
   (point-marker)))

(advice-add 'lispy-eval :before #'mm/remember-lispy-eval-point)

(defun mm/lispy-eval-mark-last-or-consult (&optional arg)
  "Jump to last `lispy-eval` marker.
With prefix ARG select with consult from the last few markers."
  (interactive "P")
  (let ((elms
	 (ring-elements
	  mm/lispy-eval-marker-hist)))
    (unless elms (user-error "No lispy eval marks"))
    (if arg
	(consult-mark elms)
      (consult--jump (car elms))))
  (evil-insert-state))

(general-def
  'lispy-mode-map
  :states '(normal visual emacs)
  "ge"
  #'mm/lispy-eval-mark-last-or-consult)

(provide 'lispy-eval-markers)

;;; lispy-eval-markers.el ends here
