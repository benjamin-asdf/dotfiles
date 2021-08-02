;;; init-helm-swoop.el --- Helm Swoop

(setq helm-swoop-speed-or-color nil)

(defun mememacs/helm-swoop-kill-line-and-quit (arg)
  (interactive "P")
  (benj/helm-make-kill-selection-and-quit
   (lambda (el)
     (with-temp-buffer
       (insert el)
       (->gg)
       (forward-word 1)
       (buffer-substring (point) (point-max))))
   arg))

(defun mememacs/helm-do-ag-file ()
  (interactive)
  (if (buffer-file-name)
      (helm-do-ag
       default-directory
       (list (buffer-file-name)))))


(defadvice helm-swoop (around my/helm-swoop-advice (&rest args) activate)
  (if (> (line-number-at-pos (point-max)) 5000)
      (progn
        (message "Using rg instead of swoop in big buffer.")
        (mememacs/helm-do-ag-file))
    ad-do-it))

(define-key helm-swoop-map (kbd "C-c C-o") #'mememacs/helm-swoop-kill-line-and-quit)

(provide 'init-helm-swoop)
