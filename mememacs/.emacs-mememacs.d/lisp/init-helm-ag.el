;;; init-helm-ag.el ---


(custom-set-variables
 '(helm-ag-base-command "rg --no-heading")
 `(helm-ag-success-exit-status '(0 2))
 '(helm-ag-use-grep-ignore-list 't)
 '(helm-candidate-number-limit 100)
 ;; helm-ag-base-command "rg --color=never --no-heading"
 ;; helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s"
 )


(define-key helm-ag-map (kbd "C-c C-o") #'benj/helm-ag-dwim-kill-selection)

(defun benj/helm-ag-dwim-kill-selection (arg)
  (interactive "P")
  (benj/helm-make-kill-selection-and-quit
    (lambda (el)
    (-last-item
    (s-split-up-to ":" el 2)))
    arg))




(provide 'init-helm-ag)

;;; init-helm-ag.el ends here
