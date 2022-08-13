;; borrowed with love from
;;; https://gitlab.com/ambrevar/dotfiles
;;; see COPYING in the root of this repo

;;; -*- lexical-binding: t; -*-

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))
      gc-cons-percentage 0.1))
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook 'ambrevar/reset-gc-cons-threshold)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook 'ambrevar/reset-file-name-handler-alist)

;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(setf native-comp-async-report-warnings-errors 'silent)

(defvar mememacs/guile-enabled t)
(defvar mememacs/enable-guix nil)

(load
 (expand-file-name "local-before.el" user-emacs-directory) 'no-err)

(straight-use-package 'use-package)

(require 'use-package)

(setf
 straight-vc-git-default-protocol 'https
 straight-use-package-by-default t
 use-package-verbose t
 use-package-always-demand t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(defconst mememacs/config-dir user-emacs-directory)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(defvar user-mail-address "Benjamin.Schwerdtner@gmail.com")

(defvar mememacs/avy-keys '(?a ?d ?f ?j ?k ?l ?o ?p ?h ?g ?m ?b))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

(use-package keychain-environment
    :straight  (:host github :repo "tarsius/keychain-environment")
    :init
    (keychain-refresh-environment)
    (auth-source-pass-enable))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setf undo-tree-auto-save-history nil)
  (remove-hook 'write-file-functions #'undo-tree-save-history-from-hook)
  (remove-hook 'kill-buffer-hook #'undo-tree-save-history-from-hook)
  (remove-hook 'find-file-hook #'undo-tree-load-history-from-hook))

(use-package evil
  :init
  (setq
   evil-want-integration t
   evil-want-keybinding nil
   evil-want-C-u-scroll nil
   evil-want-C-i-jump nil
   evil-move-cursor-back nil
   evil-move-beyond-eol t
   evil-want-fine-undo t)

  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-word)

  (custom-set-variables
   '(evil-undo-system 'undo-tree))

  (define-key evil-normal-state-map "U" #'evil-redo)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (defadvice evil-show-registers
      (after mm/evil-show-registers-adv activate)
    (text-mode)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

(use-package evil-commentary
  :hook (prog-mode . evil-commentary-mode))

(use-package hydra)

(use-package general
  :after evil
  :config (require 'init-general))

;; todo improve
(use-package evil-mc
  :config
  (add-hook 'prog-mode-hook #'evil-mc-initialize)
  (add-hook 'text-mode-hook #'evil-mc-initialize)

  (add-hook
   'mememacs/escape-functions
   #'evil-mc-undo-all-cursors)

  (general-def
    :states '(normal visual motion)
    :keymaps '(evil-mc-key-map)
    "gr" '(evil-mc-cursors-map)
    "M-n" 'evil-mc-make-and-goto-next-cursor
    "M-p" 'evil-mc-make-and-goto-prev-cursor
    "C-n" 'evil-mc-make-and-goto-next-match
    "C-t" 'evil-mc-skip-and-goto-next-match
    "C-p" 'evil-mc-make-and-goto-prev-match)

  (defhydra hydra-evil-mc ()
    "mc"
    ("n" #'evil-mc-make-and-goto-next-match "next match")
    ("j" #'evil-mc-make-cursor-move-next-line "make line")
    ("q" #'evil-mc-undo-all-cursors "undo all")
    ("I" #'evil-mc-make-cursor-in-visual-selection-beg)
    ("a" 'evil-mc-key-map "...")
    ("m" #'evil-mc-make-all-cursors)
    ("k" #'evil-mc-undo-last-added-cursor "undo last")
    ("p" #'evil-mc-find-prev-cursor "prev"))

  (general-def
    :states '(normal visual)
    "gn" #'hydra-evil-mc/body)

  (mememacs/leader-def "gn" '(evil-mc-key-map))

  (defun mememacs/disable-evil-mc-mode ()
    (evil-mc-mode -1))

  (add-hook 'dired-mode-hook #'mememacs/disable-evil-mc-mode)

  (add-hook
   'mememacs/escape-functions
   (defun mm/maybe-delete-mc-cursors ()
     (when (and
	    evil-mc-cursor-state
	    (eq evil-state 'normal))
       (evil-mc-undo-all-cursors)))))

(use-package debug
  :ensure nil
  :config
  (general-def
    debugger-mode-map
    "." #'backtrace-expand-ellipses
    "+" #'backtrace-multi-line
    "-" #'backtrace-single-line))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setf evil-collection-mode-list
	(remove 'lispy evil-collection-mode-list))
  (evil-collection-init)
  (general-def
    :states '(normal visual emacs)
    :keymaps '(dired-mode-map
	       Info-mode-map
	       Man-mode-map
	       help-mode-map)
    "SPC" nil))

(general-def
  "s-h" #'windmove-left
  "s-l" #'windmove-right
  "s-k" #'windmove-up
  "s-j" #'windmove-down)

(require 'functions)
(require 'utils)
(require 'main)
(require 'visual)
(require 'functions-1)
;(require 'mememacs-stumpwm)
(setf initial-buffer-choice (mememacs/latest-scratch "el"))
(require 'init-emacs-lisp)



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :config
  (mememacs/leader-def "hk" #'helpful-key)
  (mememacs/comma-def
    :states '(normal visual motion)
    "hf" #'helpful-callable
    "hv" #'helpful-variable
    "hk" #'helpful-key
    "hF" #'helpful-function
    "hC" #'helpful-command
    "hc" #'describe-char
    "hm" #'describe-mode))

(use-package magit
  :defer t
  :init

  (mememacs/comma-def
    "g" '(:ignore t)
    "gs" #'magit-status
    "gl" #'magit-log
    "gd" #'magit-diff
    "gc" #'magit-clone
    "gu" #'magit-fetch
    "gU" #'magit-pull)

  :config
  (setq auto-revert-mode-text "")
  (setq git-commit-summary-max-length fill-column)
  (require 'init-magit)

  (add-hook 'git-commit-mode-hook
	    (lambda ()
	      (visual-line-mode -1)))
  (general-def 'magit-blob-mode-map "n" nil))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (require 'init-vertico))

(use-package orderless
  :init
  (setq
   completion-styles
   '(orderless)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :after vertico
  :init
  (savehist-mode))

(use-package consult
  :init (recentf-mode)
  (setq completion-in-region-function #'consult-completion-in-region)
  (general-def :states '(insert) "C-j" #'completion-at-point)


  :config
  (require 'init-consult))

(use-package consult-flycheck
  :config
  (mememacs/local-def
    :states '(normal)
    :keymaps '(flycheck-mode-map)
    "e," #'consult-flycheck))

(use-package consult-dir
  :config
  (general-def
    "C-x C-d"
    #'consult-dir)

  (mememacs/comma-def
    "fd" #'consult-dir)

  (general-def
    'vertico-map
    :prefix "C-,"
    "d" #'consult-dir
    "j" #'consult-dir-jump-file))

;; (use-package consult-flycheck)

(use-package marginalia
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :init
  (general-def
    'embark-symbol-map
    "h" #'helpful-symbol)

  (setq prefix-help-command #'embark-prefix-help-command)

  (global-set-key
   (kbd "H-h") #'embark-bindings)

  :config
  (require 'init-embark))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)

(use-package mood-line
  :straight (:host github :repo "benjamin-asdf/mood-line")
  :config
  (setf mood-line-show-cursor-point t)
  (mood-line-mode))

(use-package macrostep
  :config
  (mememacs/comma-def
    :keymaps
    '(emacs-lisp-mode-map lisp-interaction-mode-map lisp-mode-map)
    "m" #'macrostep-expand)
  (add-hook
   'mememacs/escape-functions
   #'macrostep-collapse-all))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"
	slime-contribs
	'(slime-fancy slime-macrostep))
  (defun mm/add-slime-filename-cap ()
    (add-hook 'completion-at-point-functions #'slime-filename-completion 0 'local))
  (defun mm/slime-simple-c-a-p ()
    (setf
     completion-at-point-functions
     '(slime-filename-completion
       slime-simple-completion-at-point)))
  :hook
  (prog-mode . mm/add-slime-filename-cap)
  (slime-mode . mm/slime-simple-c-a-p))

(use-package lispy
  :ensure t
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-interaction-mode . lispy-mode)
  (lisp-data-mode . lispy-mode)
  (lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (common-lisp-mode . lispy-mode)
  (scheme-mode . lispy-mode)
  (clojure-mode . lispy-mode))

(use-package lispyville
  :after lispy
  :config (require 'init-lispyville))
;; maybe remove evil-mc if works well

(use-package multiple-cursors
  :config
  (add-hook
   'mememacs/escape-functions
   (defun mm/mc-remove ()
     (deactivate-mark)
     (mc/remove-fake-cursors))))

(use-package targets
  :straight (:host github :repo "noctuid/targets.el"))

(require 'project nil t)

(use-package consult-project-extra
  :after project
  :straight (consult-project-extra :type git :host github :repo "Qkessler/consult-project-extra")
  :config
  (require 'init-project))

;; https://github.com/magnars/string-edit.el/issues/19
(when
    (require
     'string-edit-at-point
     (expand-file-name "straight/repos/string-edit.el/string-edit.el"
		       mememacs/config-dir)
     t)
  (mememacs/local-def
    :states '(normal insert)
    :keymaps '(prog-mode-map)
    "se" #'string-edit-at-point))

(use-package ace-window
  :config
  (setq aw-keys mememacs/avy-keys
	aw-background nil)
  (general-def 'evil-window-map
    "w" #'ace-window
    "D" #'ace-delete-window))

(use-package cider
  :config
  (require 'init-cider)
  (require 'patch-cider-orderless))

(use-package re-jump
  :straight (:host github :repo "benjamin-asdf/re-jump.el")
  :config
  (mememacs/local-def
    :keymaps mm/cider-mode-maps
    "j" #'re-frame-jump-to-reg))

(use-package flycheck
  :config
  (require 'init-flycheck))

(use-package flycheck-clj-kondo)

(use-package geiser
  :when mememacs/guile-enabled)

(use-package macrostep-geiser
  :config
  (add-hook 'cider-mode-hook #'macrostep-geiser-setup)
  (add-hook 'geiser-mode #'macrostep-geiser-setup))

(use-package geiser-guile
  :when mememacs/guile-enabled
  :config
  (setf
   geiser-scheme-implementation 'guile
   geiser-guile-binary "guile3"
   geiser-guile-load-path
   (list "/lib/guile/3.0")))

(use-package avy
  :config
  (setf avy-timeout-seconds 0.18
	avy-keys mememacs/avy-keys)
  (add-to-list 'avy-ignored-modes 'cider-repl-mode)
  (mememacs/leader-def
    "jj" #'avy-goto-char-timer
    "jw" #'avy-goto-word-1
    "jl" #'avy-goto-line
    "cl" #'avy-copy-line
    "cr" #'avy-copy-region))

(use-package symbol-overlay
  :config
  (add-hook
   'mememacs/escape-functions
   (defun mm/so-remove-all ()
     (call-interactively #'symbol-overlay-remove-all)))

  (mememacs/leader-def
    "so" '(:ignore t)
    "soo" #'symbol-overlay-put)

  (general-def
    'symbol-overlay-map
    "h" nil))

(use-package link-hint
  :config
  (mememacs/leader-def
    "ju" #'link-hint-open-link))

(use-package guix
  :when mememacs/enable-guix
  :defer t
  :init
  (mememacs/leader-def
   "G"  '(:ignore t)
   "Gg" #'guix
   "Gi" #'guix-installed-user-packages
   "GI" #'guix-installed-system-packages
   "Gp" #'guix-packages-by-name
   "GP" #'guix-pull))

(use-package hippie-exp
  :config
  (general-def "M-/" #'hippie-expand))

(use-package flycheck-clj-kondo
  :after cider)

;; figure out guix manifests
;; figure out guix packages for clj kondo etc

;; pretty print
;; c-i and c-o should be more intuitive
;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

(load custom-file t)

(use-package winner
  :config
  (winner-mode)
  (general-def
    evil-window-map
    "u" #'winner-undo
    "r" #'winner-redo))

(general-def
    :states '(normal motion)
    ",da"
    (let ((map (make-sparse-keymap "apropos")))
       (general-def map
	 "v" #'apropos-variable
	 "V" #'apropos-value
	 "l" #'apropos-library
	 "L" #'apropos-local-value
	 "d" #'apropos-documentation
	 "D" #'apropos-documentation-property
	 "f" #'apropos-command
	 "u" #'apropos-user-option)
       map))

(with-eval-after-load
    'sh-script
  (mememacs/comma-def
    :keymaps '(sh-mode-map)
    "1"
    (defun mememacs/execute-script ()
      (interactive)
      (-some->>
	  (buffer-file-name)
	(expand-file-name)
	(shell-command)))))

(use-package org :defer t :config (require 'init-org))
(use-package denote
  :straight (:host github :repo "protesilaos/denote")
  :defer t
  :init
  (mememacs/comma-def "oj"
    (defun mm/denote-load ()
      (interactive)
      (require 'org)
      (require 'init-denote)
      (mm/find-today-journal))))

(use-package markdown-mode)

(use-package backup-each-save
  :hook after-save)

(use-package vterm
  :config
  (mememacs/leader-def "'" #'vterm)
  (general-def "s-<return>"
    (defun mm/vterm-ARG () (interactive)
	   (vterm t))))

(use-package bash-completion
  :init
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            #'bash-completion-dynamic-complete)
  :config
  (defun bash-completion-capf-1 (bol)
    (bash-completion-dynamic-complete-nocomint (funcall bol) (point) t))
  (defun bash-completion-eshell-capf ()
    (bash-completion-capf-1 (lambda () (save-excursion (eshell-bol) (point)))))
  (defun bash-completion-capf ()
    (bash-completion-capf-1 (lambda () (point-at-bol))))
  (add-hook
   'sh-mode-hook
   (defun mm/add-bash-completion ()
     (add-hook 'completion-at-point-functions #'bash-completion-capf nil t))))

(use-package mu4e
  :ensure nil
  :when nil
  :straight nil

  ;; should be added by emacs
  :load-path
  ;; "/usr/share/emacs/site-lisp/mu4e/.."
  "/usr/share/emacs/site-lisp/mu4e/"

  ;; else it syncs on startup
  :defer 60
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval ;; (* 10 60)
	nil)
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/mail")

  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")

  (setq mu4e-maildir-shortcuts
        '(("/Inbox"             . ?i)
          ("/[Gmail]/Sent Mail" . ?s)
          ("/[Gmail]/Trash"     . ?t)
          ("/[Gmail]/Drafts"    . ?d)
          ("/[Gmail]/All Mail"  . ?a))))

(add-hook 'artist-mode-hook #'artist-select-op-rectangle)

(require 'keybinds)

;; elp
;; memory-use-counts
;; instrument package
;; epl results

;; todo org

;; figure out where the code is for guix packages

; pprint
