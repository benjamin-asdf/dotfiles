;; -*- lexical-binding: t; -*-

;; I load these manually when I desire an ide for some lang

(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("M-<return>" . lsp-execute-code-action))
  :hook ((rust-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-enable-indentation nil)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "s-;")

  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless flex)))))))

(use-package rust-mode
  :ensure t :mode "\\.rs\\'"
  :init
  ;; scratchpad for rust
  (setq lsp-rust-clippy-preference "on")
  (use-package rust-playground :ensure t)
  (setq rustic-lsp-client 'lsp))

(use-package rustic)
(use-package lsp-grammarly)
(use-package typescript-mode
 :config
  (add-hook 'typescript-mode-hook #'lsp-deferred))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))

(use-package elixir-mode :config (add-hook 'elixir-mode-hook #'lsp))

(lsp--load-default-session)

(use-package yaml-mode)

(use-package julia-mode
  :config
  ;; (add-to-list 'load-path "/home/benj/repos/julia-repl/")
  ;; (require 'julia-repl)

  ;; eh that didn't work out of the box
  ;; (add-to-list 'load-path "~/repos/julia-shell-mode/")
  ;; (require 'julia-shell)
  ;; (setf julia-shell-animate-logo nil)

  (use-package lsp-julia
    ;; :config
    ;; (setq lsp-julia-default-environment "~/.julia/environments/v1.7")
    )

  ;; (use-package julia-snail
  ;; :ensure t
  ;; :hook (julia-mode . julia-snail-mode))
  )
