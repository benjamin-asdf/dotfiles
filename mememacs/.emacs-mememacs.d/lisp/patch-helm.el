;;; Patch Helm


(advice-add
 #'helm-eshell-history
 :before
 (defun mm/patch-helm-eshell-pos ()
   (goto-char
    (point-max))
   (insert " ")))


(provide 'patch-helm)
