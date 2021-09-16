;; this function does not work like this with rg
;; rg does not accept an --ignore option

(defun mememacs/helm-ag-dwm ()
  "Helm version of `projectile-ag'. Search in dir if this is no projectile project."
  (interactive)
  (cond ((projectile-project-p)
	 (let ((current-prefix-arg nil))
	   (helm-do-ag (projectile-project-root)
		       (car (projectile-parse-dirconfig-file)))))
	((buffer-file-name)
	 (helm-do-ag
	  (file-name-directory
	   (expand-file-name
	    (buffer-file-name)))))))


(defalias 'helm-projectile-ag 'mememacs/helm-ag-dwm)



;; helm-ff-switch-to-eshell doesn't exist

(defun helm-projectile-switch-to-eshell (dir)
  (interactive)
  (let* ((projectile-require-project-root nil)
         (helm-ff-default-directory (file-name-directory (projectile-expand-root dir))))
    (helm-ff-switch-to-shell dir)))


(provide 'patch-helm-projectile)
