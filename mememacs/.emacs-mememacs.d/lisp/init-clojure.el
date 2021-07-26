(setq cider-repl-display-help-banner nil)
(setq cider-repl-require-ns-on-set t)

(require 'init-lispy)
(dolist (hook '(clojure-mode-hook cider-repl-mode-hook))
  (add-hook hook #'ambrevar/init-lispy))

(when (fboundp 'rainbow-delimiters-mode)
  (dolist (hook '(clojure-mode-hook cider-repl-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(setq cider-repl-history-file (expand-file-name "cider-history" user-emacs-directory))

(defun ambrevar/cider-switch-to-repl () ; TODO: Replace with `helm-defswitch'.
  (interactive)
  (require 'cider)
  (let ((b (cl-find-if (lambda (b)
                         (with-current-buffer b
                           (eq major-mode 'cider-repl-mode)))
                       (buffer-list))))
    (if b
        (pop-to-buffer b)
      (call-interactively #'cider-jack-in))))

(when (require 'helm-cider nil 'noerror)
  (helm-cider-mode)
  (define-key cider-repl-mode-map (kbd "M-p") #'helm-cider-repl-history))

(when (require 'helm-clojuredocs nil 'noerror)
  (define-key cider-repl-mode-map (kbd "C-c C-d h") #'helm-clojuredocs)
  (define-key cider-repl-mode-map (kbd "C-c C-d C-h") #'helm-clojuredocs-at-point))


;; (evil-define-key 'insert cider-repl-mode-map
;;   (kbd "C-j") 'spacemacs//clj-repl-wrap-c-j
;;   (kbd "C-k") 'spacemacs//clj-repl-wrap-c-k)

(provide 'init-clojure)
