;;; borrowed with love from
;;; https://gitlab.com/ambrevar/dotfiles
;;; see COPYING in the root of this repo


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


(straight-use-package 'use-package)

(require 'use-package)
(setf
 straight-vc-git-default-protocol 'ssh
 straight-use-package-by-default t
 use-package-verbose t
 use-package-always-demand t)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))



;; ;;; Local config.  See below for an example usage.
;; (load "local-before" t)

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
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-word)

  (custom-set-variables
   '(evil-undo-system
     'undo-tree))

  (define-key evil-normal-state-map "U" #'evil-redo)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package hydra
  :config
  (defhydra hydra-buffer ()
    "buffer"
    ("d" #'kill-this-buffer)
    ("k" #'previous-buffer)
    ("j" #'previous-buffer)
    ("a" #'mark-whole-buffer)))

;; todo config backtrace here so we get better debug init

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

(use-package general
  :after evil
  :config
  (general-create-definer
    mememacs/leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer
    mememacs/comma-def
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "C-,")

  (mememacs/leader-def
    "SPC" #'helm-M-x
    "t" '(:ignore t)
    "n" #'line-number-mode

    "b" '(:ignore t :which-key "b..")
    "bd" #'kill-this-buffer
    "bb" #'helm-mini
    "b." #'hydra-buffer/body

    "f" '(:ignore t :which-key "f..")
    "fd" #'delete-file
    "fs" #'save-buffer
    "ff" #'helm-find-files
    "fr" #'helm-recentf

    "w" '(evil-window-map :which-key "window")
    "wm" #'delete-other-windows
    "wd" #'evil-window-delete

    "s" '(:ignore t :which-key "search")
    "ss" #'helm-swoop-without-pre-input
    "sS" #'helm-swoop

    "j" '(:ignore t)
    "jd" #'dired-jump
    "jD" #'dired-jump-other-window
    "jf" #'find-function

    "/" #'helm-do-grep-ag
    "hc" #'describe-char
    "hm" #'describe-mode
    "hi" #'helm-info-emacs))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1)

  (defhydra hydra-evil-mc ()
    "mc"
    ("n" #'evil-mc-make-and-goto-next-match)
    ("j" #'evil-mc-make-cursor-move-next-line)
    ("q" #'evil-mc-undo-all-cursors)
    ("a" 'evil-mc-key-map)
    ("k" #'evil-mc-undo-last-added-cursor)
    ("p" #'evil-mc-find-prev-cursor))

  (general-def
    :states '(normal visual)
    "gn" #'hydra-evil-mc/body)

  (mememacs/leader-def
    "gn" '(evil-mc-key-map :which-key "mc"))

  (defun mememacs/disable-evil-mc-mode ()
    (evil-mc-mode -1))
  (add-hook 'dired-mode-hook #'mememacs/disable-evil-mc-mode)

  (defadvice evil-force-normal-state (before mm/evil-normal-state-maybe-delete-mc-cursors activ)
    (when (and
	   evil-mc-cursor-state
	   (eq evil-state 'normal))
      (evil-mc-undo-all-cursors))))

(use-package hydra
  :config
  (defhydra hydra-buffer ()
    "buffer"
    ("d" #'kill-this-buffer)
    ("k" #'previous-buffer)
    ("j" #'previous-buffer)
    ("a" #'mark-whole-buffer)))

(use-package debug
  :ensure nil
  :config
  (general-def
    :keymap 'debugger-mode-map
    :state '(normal motion)
    "." #'backtrace-expand-ellipses
    "+" #'backtrace-multi-line
    "-" #'backtrace-single-line))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setf evil-collection-mode-list
	(remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(require 'functions)
(require 'main)
(require 'visual)
(require 'functions-1)



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :config
  (mememacs/leader-def
    "hf" #'helpful-callable
    "hv" #'helpful-variable
    "hk" #'helpful-key
    "hF" #'helpful-function
    "hC" #'helpful-command)

  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

(use-package evil-goggles
  :config
  (setf
   evil-goggles-enable-delete nil
   evil-goggles-enable-change nil)
  (evil-goggles-mode))


(use-package helm
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (require 'init-helm)
  (mememacs/leader-def
    "r" '(:ignore t :which-key "r..")
    "rl" #'helm-resume))

(use-package magit
  :defer t
  :config
  (setq auto-revert-mode-text "")
  (setq git-commit-summary-max-length fill-column)
  (require 'init-magit)

  (mememacs/leader-def
    "g" '(:ignore t :which-key "git")
    "gs" #'magit-status)

  (add-hook 'git-commit-mode-hook
	    (lambda ()
	      (visual-line-mode -1))))


(use-package company
  :config
  (add-hook 'after-init-hook #'global-company-mode)
  ;; (setq company-idle-delay 0)
  )


(use-package
  helm-company
  :after company
  :config
  (define-key company-active-map (kbd "C-/") 'helm-company)
  (dolist (map (list company-active-map company-search-map))
    (define-key map (kbd "C-j") 'company-select-next)
    (define-key map (kbd "C-k") 'company-select-previous)
    (define-key map (kbd "C-l") 'company-complete-selection))
  (defun my-company-manual-begin ()
    (interactive)
    (if (company-tooltip-visible-p)
        (company-select-next)
      (company-manual-begin)))
  (define-key evil-insert-state-map (kbd "C-j") #'my-company-manual-begin))


(use-package mood-line
  :straight (:host github :repo "benjamin-asdf/mood-line")
  :config (mood-line-mode))

;; TODO
;; (nconc package-selected-packages '(exwm helm-exwm))
;; (nconc package-selected-packages '(pulseaudio-control))

(with-eval-after-load 'pulseaudio-control
  ;; REVIEW: Upstream should set path dynamically.
  ;; https://github.com/flexibeast/pulseaudio-control/issues/7
  (setq pulseaudio-control-pactl-path (executable-find "pactl")))

(use-package exwm
  :config
  (require 'init-exwm))

;; (use-package emacs-desktop-environment)

(use-package macrostep
  :config
  (general-def
    :states '(normal motion)
    :keymaps 'emacs-lisp-mode-map
    ",m" #'macrostep-expand))

(use-package lispy
  :ensure t
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-interaction-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (common-lisp-mode . lispy-mode)
  (scheme-mode . lispy-mode)
  (clojure-mode . lispy-mode))

(use-package lispyville
  :after lispy
  :config (require 'init-lispyville)
  )

(use-package which-key
  :config
  (which-key-mode)
  (setf which-key-idle-delay 0.22))


(use-package helm-swoop
  :config (require 'init-helm-swoop))

(use-package helm-ag
  :config (require 'init-helm-ag))

(use-package projectile
  :config
  (projectile-mode)
  (mememacs/leader-def
    "p" 'projectile-command-map)
  (setf projectile-indexing-method 'alien)
  (let ((cmd
	 "fd --hidden --exclude=.git --type=f . --print0"))
    (setf
     projectile-git-command cmd
     projectile-generic-command cmd))
  (setf projectile-completion-system 'helm))

;; TODO
;; add emacs-dir/backups to known projects


(use-package helm-projectile
  :config
  (require 'patch-helm-projectile))


(use-package ace-window
  :config
  (setq aw-keys '(?k ?j ?h ?n ?i ?a ?s ?d ?l ?e ?r ?t)
	aw-background nil)
  (mememacs/leader-def
    "wu" #'ace-window
    "wD" #'ace-delete-window))

;; lispy kill new before lispy delete but only in special

(use-package cider)

(use-package flycheck)

(use-package flycheck-clj-kondo)
(use-package flycheck-clojure)
(use-package flycheck-joker)


;; todo binds
(use-package geiser)

(use-package geiser-guile
  :config
  (setf geiser-scheme-implementation
	'guile)
  (setf geiser-guile-load-path
	(list
	 (expand-file-name
	  "~/.guix-profile/lib/guile/3.0/site-cache"))))


(use-package avy
  :config
  (mememacs/leader-def
    "jj" #'avy-goto-char-timer
    "jw" #'avy-goto-word-1
    "jl" #'avy-goto-line))

(use-package symbol-overlay
  :config


  (defhydra hydra-symbol-overlay ()
    "smbol overlay"
    ("o" #'symbol-overlay-put)
    ("j" #'symbol-overlay-jump-next)
    ("k" #'symbol-overlay-jump-prev)
    ("m" #'symbol-overlay-mode)
    ("a" #'symbol-overlay-maybe-put-temp)
    ("O" #'symbol-overlay-remove-all)
    ("n" #'symbol-overlay-jump-next))

  ;; todo

  ;; (defmacro mm/wrap-symb-ov-hydra (cmd)
  ;;   `(defun ,(symbol-name (concat
  ;; 			   (mememacs/mkstr
  ;; 			    (cadr cmd))
  ;; 			   "-and-hydra"))
  ;; 	 ()
  ;;      (interactive)
  ;;      (call-interactively ,cmd)
  ;;      (hydra-symbol-overlay/body)))

  ;; (mm/wrap-symb-ov-hydra
  ;;  #'symbol-overlay-put)

  (mememacs/leader-def
    "so" #'hydra-symbol-overlay/body))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))


(use-package backup-each-save)

(use-package link-hint
  :config
  (mememacs/leader-def
    "ju" #'link-hint-open-link))

(use-package guix
  :defer t
  :init
  (mememacs/leader-def
   "G"  '(:ignore t :which-key "Guix")
   "Gg" '(guix :which-key "Guix")
   "Gi" '(guix-installed-user-packages :which-key "user packages")
   "GI" '(guix-installed-system-packages :which-key "system packages")
   "Gp" '(guix-packages-by-name :which-key "search packages")
   "GP" '(guix-pull :which-key "pull")))

(use-package hippie-exp
  :config
  (general-def
    :states '(insert)
    "C-/" #'hippie-expand))


;; https://github.com/noctuid/link-hint.el

;; evil undo system
;; redo to U
;; a nice hydra for this instead of visualizer

;; todo get rid of caches in emacs dir
;; everything to ~/tmp


;; (defhydra best-hydra ())

;; todo
;; helm-cider
;; flycheck-clj-kondo
;; flycheck-joker

;; figure out guix manifests
;; figure out guix packages for clj kondo etc


;; org

;; lispy mood line

;; pretty print

;;  nerd commenter
