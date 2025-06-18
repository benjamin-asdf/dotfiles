;;; lispy-eval-markers.el --- Remember last lispy eval markers  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defvar-local mm/lispy-eval-marker-hist nil)

;;  I realized I want an ordered set instead

(defun mm/remember-lispy-eval-point (&rest _)
  (let ((marker (point-marker)))
    (unless mm/lispy-eval-marker-hist
      (setf mm/lispy-eval-marker-hist (make-ring 3)))
    (unless (member marker (ring-elements mm/lispy-eval-marker-hist))
      (ring-insert mm/lispy-eval-marker-hist (point-marker)))))

(defun ring-pop (ring)
  "Remove the latest item from RING
 and return the item."
  (let* ((elms  (ring-elements ring))
	 (elm (car elms)))
    (ring-remove ring (length elms))
    elm))

(advice-add 'lispy-eval :before #'mm/remember-lispy-eval-point)
;; (advice-add 'cider-eval-last-sexp :before #'mm/remember-lispy-eval-point)
;; (advice-add 'cider-pprint-eval-last-sexp :before #'mm/remember-lispy-eval-point)

(defun mm/lispy-eval-mark-last-or-consult (&optional arg)
  "Jump to last `lispy-eval` marker.
This pops the marker from the ring.
With prefix ARG select with consult from the last markers."
  (interactive "P")
  (when (and
	 (not arg)
	 (or (not mm/lispy-eval-marker-hist)
	  (ring-empty-p mm/lispy-eval-marker-hist)))
    (user-error "No lispy eval marks"))
  (if arg
      (progn
	(consult-mark
	 (apply
	  #'append
	  (mapcar
	   (lambda (b)
	     (with-current-buffer b
	       (when mm/lispy-eval-marker-hist
		 (ring-elements mm/lispy-eval-marker-hist))))
	   (buffer-list))))
	(evil-insert-state))
    (consult--jump (ring-pop mm/lispy-eval-marker-hist))))

(provide 'lispy-eval-markers)

;;; lispy-eval-markers.el ends here
