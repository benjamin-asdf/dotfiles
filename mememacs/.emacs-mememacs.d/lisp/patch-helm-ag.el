(defun mm/filter-file-exists (lst)
  (-filter #'file-exists-p lst))

(advice-add #'helm-ag--file-visited-buffers :filter-return #'mm/filter-file-exists)

(provide 'patch-helm-ag)
