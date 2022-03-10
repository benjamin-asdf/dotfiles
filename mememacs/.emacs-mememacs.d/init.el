;;; borrowed with love from
;;; https://gitlab.com/ambrevar/dotfiles
;;; see COPYING in the root of this repo

;;; -*- lexical-binding: t; -*-

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'ambrevar/reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook 'ambrevar/reset-file-name-handler-alist)

;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)

;;; Store additional config in a 'lisp' subfolder and add it to the load path so
;;; that `require' can find the files.
;;; This must be done before moving `user-emacs-directory'.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))


;; (when (require 'package nil t)
;;   ;; TODO: MELPA's https sometimes return
;;   ;;   emacs melpa invalid: certificate host does not match hostname
;;   ;; Try the following:
;;   ;;   (setq tls-checktrust nil)
;;   ;; Different Emacs version have different byte code.  If a versioned ELPA
;;   ;; directory is found, use it.
;;   (let ((versioned-dir (format "elpa-%s.%s" emacs-major-version emacs-minor-version)))
;;     (when (member versioned-dir (directory-files (expand-file-name ".." package-user-dir)))
;;       (setq package-user-dir (expand-file-name (concat "../" versioned-dir) package-user-dir))))
;;   (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/"))
;;   (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;   (package-initialize))


(defvar mememacs/use-exwm t)
(defvar mememacs/guile-enabled t)
(defvar mememacs/enable-guix nil)

(load
 (expand-file-name "local-before.el" user-emacs-directory) 'no-err)

(straight-use-package 'use-package)

(require 'use-package)
(setf
 ;; straight-vc-git-default-protocol 'ssh
 straight-vc-git-default-protocol 'https
 straight-use-package-by-default t
 use-package-verbose t
 use-package-always-demand t)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


(setq user-mail-address "Benjamin.Schwerdtner@gmail.com")

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
  (global-undo-tree-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  (setq
   evil-move-cursor-back nil
   evil-move-beyond-eol t
   evil-want-fine-undo t)

  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-word)

  (custom-set-variables
   '(evil-undo-system
     'undo-tree))

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

  (mememacs/leader-def
    "gn"
    '(evil-mc-key-map :which-key "mc"))

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
    :states 'normal
    :keymaps '(dired-mode-map
	       Info-mode-map
	       Man-mode-map
	       help-mode-map)
    "SPC" nil))

(use-package exwm
  :when mememacs/use-exwm
  :ensure nil
  :config
  (require 'init-exwm-1))

(unless mememacs/use-exwm
  (general-def
    "s-h" #'windmove-left
    "s-l" #'windmove-right
    "s-k" #'windmove-up
    "s-j" #'windmove-down))

(require 'functions)
(require 'utils)
(require 'main)
(require 'visual)
(require 'functions-1)



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :init (require 'patch-helpful)
  :config
  (mememacs/comma-def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "hh" #'helpful-at-point)
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
    "g" '(:ignore t :which-key "git")
    "gs" #'magit-status
    "gl" #'magit-log
    "gd" #'magit-diff
    "gC" #'magit-clone
    "gu" #'magit-fetch
    "gU" #'magit-pull)


  :config
  (setq auto-revert-mode-text "")
  (setq git-commit-summary-max-length fill-column)
  (require 'init-magit)

  (add-hook 'git-commit-mode-hook
	    (lambda ()
	      (visual-line-mode -1)))
  (general-def
    'magit-blob-mode-map
    "n" nil))

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
  (advice-add
   #'completing-read-multiple
   :override #'consult-completing-read-multiple)

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

(use-package corfu
  :init (corfu-global-mode)
  :config
  (require 'patch-cider-orderless)

  (add-hook
   'cider-mode-hook
   'mm/patch-orderless-style)

  (dolist
      (hook
       '(eshell-mode-hook
	 emacs-lisp-mode-hook
	 shell-mode-hook))
    (add-hook hook (lambda () (corfu-mode -1))))

  (setf
   corfu-cycle t
   corfu-auto t
   corfu-quit-at-boundary t
   corfu-quit-no-match t
   corfu-auto-prefix 2
   corfu-auto-delay 0.18)

  (general-def
    :states '(insert)
    :keymap 'corfu-map
    "C-b" #'beginning-of-buffer
    "C-f" #'end-of-buffer
    "C-l" #'corfu-insert
    "C-n" #'corfu-next
    "C-/" #'mememacs/c-completion)

  (general-def
    :states '(insert)
    "C-j" #'completion-at-point))

(use-package mood-line
  :straight (:host github :repo "benjamin-asdf/mood-line")
  :config
  (setf mood-line-show-cursor-point t)
  (mood-line-mode))

;; TODO
;; (nconc package-selected-packages '(exwm helm-exwm))
;; (nconc package-selected-packages '(pulseaudio-control))

(with-eval-after-load 'pulseaudio-control
  ;; REVIEW: Upstream should set path dynamically.
  ;; https://github.com/flexibeast/pulseaudio-control/issues/7
  (setq pulseaudio-control-pactl-path (executable-find "pactl")))

(use-package macrostep
  :config
  (mememacs/comma-def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "m" #'macrostep-expand)
  (add-hook
   'mememacs/escape-functions
   #'macrostep-collapse-all))

(use-package lispy
  :ensure t
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-interaction-mode . lispy-mode)
  (lisp-data-mode . lispy-mode)
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

(use-package projectile
  :config
  (projectile-mode)

  (let ((cmd "fd --hidden --exclude=.git --type=f . --print0"))
    (setf
     projectile-indexing-method 'alien
     projectile-git-command cmd
     projectile-generic-command cmd
     projectile-completion-system 'default))

  (defun mememacs/projectile-todo ()
    (interactive)
    (-some->>
	(projectile-project-root)
      (expand-file-name "")
      (find-file "TODOs.org")))

  (mememacs/leader-def
    "p" 'projectile-command-map
    "pO" #'mememacs/projectile-todo))

(use-package string-edit
  :config
  (mememacs/local-def
    :states '(normal insert)
    :keymaps '(prog-mode-map)
    "e" #'string-edit-at-point))

;; TODO
;; add emacs-dir/backups to known projects

(use-package ace-window
  :config
  (setq aw-keys '(?k ?j ?h ?n ?i ?a ?s ?d ?l ?e ?r ?t)
	aw-background nil)
  (general-def 'evil-window-map
      "w" #'ace-window
      "D" #'ace-delete-window))

(use-package cider
  :config
  (setq clojure-toplevel-inside-comment-form t)
  (require 'init-cider))

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

;; todo binds
(use-package geiser
  :when mememacs/guile-enabled)

(use-package geiser-guile
  :when mememacs/guile-enabled
  :config
  (setf
   geiser-scheme-implementation 'guile
   geiser-guile-binary "guile3"
   geiser-guile-load-path
   (list "/lib/guile/3.0")))

(use-package
  avy
  :config (setf avy-timeout-seconds 0.18)
  (mememacs/leader-def
    "jj" #'avy-goto-char-timer
    "jw" #'avy-goto-word-1
    "jl" #'avy-goto-line))

(use-package symbol-overlay
  :config
  (add-hook
   'mememacs/escape-functions
   (defun mm/so-remove-all ()
       (call-interactively #'symbol-overlay-remove-all)))

  (mememacs/leader-def
    "so" '(:ignore t :which-key "symbol overlay")
    "soo" #'symbol-overlay-put
    "son" #'symbol-overlay-switch-forward
    "sop" #'symbol-overlay-switch-backward
    "som" #'symbol-overlay-mode
    "soh" (defun show-symbol-overlay-map ()
	    (interactive)
	    (which-key-show-keymap 'symbol-overlay-map)))

  (general-def
    'symbol-overlay-map
    "h" nil))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode)))

(use-package link-hint
  :config
  (mememacs/leader-def
    "ju" #'link-hint-open-link))

(use-package guix
  :when mememacs/enable-guix
  :defer t
  :init
  (mememacs/leader-def
   "G"  '(:ignore t :which-key "Guix")
   "Gg" '(guix :which-key "Guix")
   "Gi" '(guix-installed-user-packages :which-key "user packages")
   "GI" '(guix-installed-system-packages :which-key "system packages")
   "Gp" '(guix-packages-by-name :which-key "search packages")
   "GP" '(guix-pull :which-key "pull")))

(use-package hippie-exp)

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
    `(,(let ((map (make-sparse-keymap "apropos")))
	 (general-def map
	   "v" #'apropos-variable
	   "V" #'apropos-value
	   "l" #'apropos-library
	   "L" #'apropos-local-value
	   "d" #'apropos-documentation
	   "D" #'apropos-documentation-property
	   "f" #'apropos-command
	   "u" #'apropos-user-option)
	 map)
     :which-key "apropos"))

;; (use-package yasnippet
;;   :defer 20
;;   :config
;;   (add-to-list
;;    'yas-snippet-dirs
;;    (concat mememacs/config-dir "snippets"))
;;   (add-hook
;;    'prog-mode-hook
;;    #'yas-minor-mode-on)
;;   (add-to-list
;;    'hippie-expand-try-functions-list
;;    #'yas-expand-from-trigger-key))

;; (use-package yasnippet-snippets
;;   :after yasnippet
;;   :config
;;   (yasnippet-snippets-initialize))


(with-eval-after-load
    'sh-script
  (mememacs/comma-def
    :keymaps '(shell-script-mode)
    "1"
    (defun mememacs/execute-script ()
      (interactive)
      (-some->>
	  (buffer-file-name)
	(expand-file-name)
	(shell-command)))))

(use-package org
  :defer t)

;; (use-package org-jira
;;   :defer t
;;   :config (require 'init-org-jira))

(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :config (require 'init-org-roam))

(use-package markdown-mode)
;; try vc-backup
;; and then replace every other backup file system we have
(use-package backup-each-save)

(use-package restclient
  :defer t
  :config
  (require 'patch-restclient))

(use-package vterm
  :config
  (mememacs/leader-def
    "'" #'vterm
    "p'" #'projectile-run-vterm))

(use-package bash-completion
  ;; :straight (:host github :repo "szermatt/emacs-bash-completion")
  :init
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

(use-package mu4e
  :ensure nil
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

(require 'late-bindings)

;; elp
;; memory-use-counts
;; instrument package
;; epl results

;; todo org

;; (use-package org-projectile)

;; todo company remove icons

;; figure out where the code is for guix packages

;; (general-def)


;; (use-package jdee)

;; fix helm ag "command attempted to use minibuffer"

;; todo nyxt auto clone github page
					;; (use-package slime
;;   (setq inferior-lisp-program "sbcl"))

; pprint
