;;; Patch Helm

(with-eval-after-load
    'helm-projectile
  (defun helm-projectile-switch-to-eshell (dir)
    (interactive)
    (let* ((projectile-require-project-root nil)
           (helm-ff-default-directory (file-name-directory (projectile-expand-root dir))))
      ;;  they fucked up, helm-ff-switch-to-eshell doesn't exist
      (helm-ff-switch-to-shell dir))))


(defun mm/filter-file-exists (lst)
  (-filter #'file-exists-p ret))

(advice-add #'helm-ag--file-visited-buffers :filter-return #'mm/filter-file-exists)

(provide 'patch-helm)
