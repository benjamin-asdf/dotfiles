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

(general-def
  flycheck-command-map
  "f" #'flycheck-mode)


(mememacs/local-def
  :keymaps
  '(sh-mode-map)
  "e" flycheck-command-map)

(provide 'init-flycheck)
