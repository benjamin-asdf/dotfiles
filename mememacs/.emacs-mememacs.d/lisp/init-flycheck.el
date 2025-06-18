;; -*- lexical-binding: t; -*-
(defun mememacs/flycheck-disable-err-at-point ()
  (interactive)
  (let ((err-id (flycheck-error-id
		 (car (flycheck-overlay-errors-in
		       (line-beginning-position)
		       (line-end-position))))))
    (pcase
	major-mode
      (shell-script-mode
       (save-excursion
	 (forward-line -1)
	 (end-of-line)
	 (forward-line 1)
	 (insert
	  (format
	   "# shellcheck disable=%s\n"
	   err-id))))
      (user-error
       (format
	"Dont know how to disable errors in "
	major-mode)))))

(provide 'init-flycheck)
