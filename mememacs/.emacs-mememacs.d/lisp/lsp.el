;; -*- lexical-binding: t; -*-

;; I load these manually when I desire an ide for some lang

(use-package
  lsp-mode
  :bind (:map lsp-mode-map
              ("M-<return>" . lsp-execute-code-action))
  :hook ((rust-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init (setq lsp-enable-indentation
              nil)
  (setq lsp-diagnostics-provider
        :flycheck)
  (setq lsp-headerline-breadcrumb-enable
        nil)
  (setq lsp-keymap-prefix "s-;")
  (add-hook
   'lsp-completion-mode-hook
   (lambda ()
     (setf
      (alist-get
       'lsp-capf
       completion-category-defaults)
      '((styles . (orderless flex))))))
  (defun mm/clear-the-lsp-cache-bust-invalidate-chaches-lsp ()
    (interactive)
    (setq lsp--session
          (lsp--load-default-session))))

(use-package lsp-grammarly
  :config
  (setq-default lsp-grammarly-domain "academic")

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'lsp-grammarly--server-command)
    :activation-fn (lambda (&rest _)
                     (or mm/allow-lsp-grammerly
                         (memq major-mode lsp-grammarly-active-modes)))
    :initialization-options
    `((clientId . ,lsp-grammarly-client-id)
      (name . "Grammarly"))
    :major-modes lsp-grammarly-active-modes
    :priority -1
    :add-on? t
    :server-id 'grammarly-ls
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'grammarly-ls callback error-callback))
    :after-open-fn #'lsp-grammarly--init
    :async-request-handlers
    (ht ("$/showError" #'lsp-grammarly--show-error)
        ("$/updateDoc" #'lsp-grammarly--update-document-state)))))

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'lsp-deferred))

(progn
  (use-package rust-mode
    :ensure t :mode "\\.rs\\'"
    :init
    ;; scratchpad for rust
    (setq lsp-rust-clippy-preference "on")
    (use-package rust-playground :ensure t)
    (setq rustic-lsp-client 'lsp))

  (use-package rustic))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))

(use-package elixir-mode :config (add-hook 'elixir-mode-hook #'lsp))

(use-package yaml-mode)
(use-package fsharp-mode)

(use-package glsl-mode)


(progn
  ;; https://github.com/idcrook/.emacs.d/blob/main/lisp/lang-cpp.el
  (use-package cuda-mode)
  (require 'cmake-mode)

  )

;; this preceedes emacs built in tree sitter
;; doesn't support combobulate;; (progn
;;   (use-package tree-sitter)
;;   (use-package tree-sitter-langs)
;;   ;; (add-hook 'rust-mode-hook #'tree-sitter-mode)
;;   ;; (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
;;   (tree-sitter-require 'rust))


(progn
  (add-hook 'python-mode-hook #'lsp-deferred)
  (remove-hook 'python-mode-hook 'lispy-mode)
  (remove-hook 'python-mode-hook 'mm/add-lispy-python-capf))


