;;; -*- lexical-binding: t; -*-

;; borrowed with love from
;;; https://gitlab.com/ambrevar/dotfiles
;;; see COPYING in the root of this repo

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

(defvar mememacs/avy-keys '(?a ?d ?f ?j ?k ?l ?o ?p ?h ?g ?b))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

(use-package keychain-environment
    :straight  (:host github :repo "tarsius/keychain-environment")
    :init
    (keychain-refresh-environment)
    (auth-source-pass-enable))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(use-package general
  :after evil
  :config (require 'init-general))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setf undo-tree-auto-save-history nil)
  (remove-hook 'write-file-functions #'undo-tree-save-history-from-hook)
  (remove-hook 'kill-buffer-hook #'undo-tree-save-history-from-hook)
  (remove-hook 'find-file-hook #'undo-tree-load-history-from-hook)
  (general-unbind undo-tree-map "C-/"))

;; (use-package evil
;;   :init
;;   (setq
;;    evil-want-integration t
;;    evil-want-keybinding nil
;;    evil-want-C-u-scroll nil
;;    evil-want-C-i-jump nil
;;    evil-move-cursor-back nil
;;    evil-move-beyond-eol t
;;    evil-want-fine-undo t)

;;   :config
;;   (evil-mode 1)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-word)

;;   (custom-set-variables
;;    '(evil-undo-system 'undo-tree))

;;   (define-key evil-normal-state-map "U" #'evil-redo)

;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
;;   (evil-set-initial-state 'dashboard-mode 'normal)

;;   (defadvice evil-show-registers
;;       (after mm/evil-show-registers-adv activate)
;;     (text-mode)))

;; (use-package evil-surround
;;   :config
;;   (global-evil-surround-mode 1)
;;   (add-hook 'emacs-lisp-mode-hook
;; 	    (lambda ()
;;               (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

;; (use-package evil-commentary
;;   :hook (prog-mode . evil-commentary-mode))

(use-package hydra)



;; todo improve
;; (use-package evil-mc
;;   :config
;;   (add-hook 'prog-mode-hook #'evil-mc-initialize)
;;   (add-hook 'text-mode-hook #'evil-mc-initialize)

;;   (add-hook
;;    'mememacs/escape-functions
;;    #'evil-mc-undo-all-cursors)

;;   (general-def
;;     :states '(normal visual motion)
;;     :keymaps '(evil-mc-key-map)
;;     "gr" '(evil-mc-cursors-map)
;;     "M-n" 'evil-mc-make-and-goto-next-cursor
;;     "M-p" 'evil-mc-make-and-goto-prev-cursor
;;     "C-n" 'evil-mc-make-and-goto-next-match
;;     "C-t" 'evil-mc-skip-and-goto-next-match
;;     "C-p" 'evil-mc-make-and-goto-prev-match)

;;   (defhydra hydra-evil-mc ()
;;     "mc"
;;     ("n" #'evil-mc-make-and-goto-next-match "next match")
;;     ("j" #'evil-mc-make-cursor-move-next-line "make line")
;;     ("q" #'evil-mc-undo-all-cursors "undo all")
;;     ("I" #'evil-mc-make-cursor-in-visual-selection-beg)
;;     ("a" 'evil-mc-key-map "...")
;;     ("m" #'evil-mc-make-all-cursors)
;;     ("k" #'evil-mc-undo-last-added-cursor "undo last")
;;     ("p" #'evil-mc-find-prev-cursor "prev"))

;;   (general-def
;;     :states '(normal visual)
;;     "gn" #'hydra-evil-mc/body)

;;   (mememacs/leader-def "gn" '(evil-mc-key-map))

;;   (defun mememacs/disable-evil-mc-mode ()
;;     (evil-mc-mode -1))

;;   (add-hook 'dired-mode-hook #'mememacs/disable-evil-mc-mode)

;;   (add-hook
;;    'mememacs/escape-functions
;;    (defun mm/maybe-delete-mc-cursors ()
;;      (when (and
;; 	    evil-mc-cursor-state
;; 	    (eq evil-state 'normal))
;;        (evil-mc-undo-all-cursors)))))

(use-package debug
  :ensure nil
  :config
  (general-def
    debugger-mode-map
    "." #'backtrace-expand-ellipses
    "+" #'backtrace-multi-line
    "-" #'backtrace-single-line))

;; I am starting to experience this as bloat
;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (setf
;;    evil-collection-want-find-usages-bindings nil
;;    evil-collection-mode-list
;;    (remove
;;     'go-mode
;;     (remove 'lispy evil-collection-mode-list)))
;;   (evil-collection-init)
;;   (general-def
;;     :states '(normal visual emacs)
;;     :keymaps '(dired-mode-map
;; 	       Info-mode-map
;; 	       Man-mode-map
;; 	       help-mode-map)
;;     "SPC" nil))

(require 'functions)
(require 'utils)
(require 'main)
(require 'visual)
(require 'functions-1)

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
    "gj" #'magit-status
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
	    (defun mm/disable-visual-line-mode ()
	      (visual-line-mode -1))))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (require 'init-vertico))

(use-package orderless)

;; org is early
;; see note 20220826T155813--org-file-name-concat__code_issue.org
(use-package org
  :straight (:host github :repo "emacs-straight/org-mode")
  :config (require 'init-org))

(use-package savehist
  :after vertico
  :init
  (savehist-mode))

(use-package dired
  :straight nil
  :ensure nil
  :config
  (setf dired-listing-switches "-alh"))

(use-package consult
  :after orderless
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

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

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

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  (defun mm/sly-complete-at-point ()
    (when (sly-connected-p)
      (let  ((beg (sly-symbol-start-pos))
             (end (sly-symbol-end-pos)))
	(when-let*
	    ((completion
	      (car (sly--completion-request-completions
		    (buffer-substring beg end)
		    'slynk-completion:flex-completions))))
	  (list
	   beg
	   end
	   (completion-table-dynamic
	    (lambda (_) completion)))))))

  (sly-symbol-completion-mode -1)
  (remove-hook 'sly-mode-hook 'sly--setup-completion)
  (add-hook
   'sly-mode-hook
   (defun mm/setup-sly-completion ()
     (add-hook 'completion-at-point-functions #'mm/sly-complete-at-point nil t))))

(use-package lispy
  :ensure t
  :config
  (defun mm/enable-le-python ()
    (require 'le-python)
    (add-to-list 'completion-at-point-functions 'lispy-python-completion-at-point))
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-interaction-mode . lispy-mode)
  (lisp-data-mode . lispy-mode)
  (lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (common-lisp-mode . lispy-mode)
  (scheme-mode . lispy-mode)
  (clojure-mode . lispy-mode)
  (python-mode . lispy-mode)
  (python-mode . mm/enable-le-python))

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

(when (require 'project nil t)
  (require 'init-project))

;; https://github.com/magnars/string-edit.el/issues/19
(when
    (require
     'string-edit-at-point
     (expand-file-name "straight/repos/string-edit.el/string-edit-at-point.el"
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

(use-package flycheck
  :config
  (require 'init-flycheck))

(use-package flycheck-clj-kondo)

(use-package geiser
  :when mememacs/guile-enabled)

(use-package macrostep-geiser
  :config
  (add-hook 'cider-mode-hook #'macrostep-geiser-setup)
  (add-hook 'geiser-mode-hook #'macrostep-geiser-setup))

(use-package geiser-guile
  :when mememacs/guile-enabled
  :config
  (setf
   geiser-scheme-implementation 'guile
   geiser-guile-binary "guile"
   geiser-guile-load-path
   (list "/lib/guile/2.2/")))

(use-package avy
  :config
  (setf avy-timeout-seconds 0.18
	avy-keys mememacs/avy-keys
	avy-style 'words)
  (add-to-list 'avy-ignored-modes 'cider-repl-mode)
  (mememacs/leader-def
    "jj" #'avy-goto-char-timer
    "jw" #'avy-goto-word-1
    "jl" #'avy-goto-line
    "cl" #'avy-copy-line
    "cr" #'avy-copy-region)
  ;; better not start witch chars from
  ;; avy-dispatch-alist
  (setq
   avy-words
   '("am" "by" "jo" "jl" "jak" "jik"
     "fo" "fa" "fro" "fam" "if" "is" "it" "my" "ox" "up" "em" "eb" "ef"
     "ace" "act" "add" "age" "ago" "aim" "air" "ale" "all" "and" "ant" "any"
     "ape" "apt" "arc" "are" "arm" "art" "ash" "ate" "awe" "axe" "bad" "bag"
     "ban" "bar" "bat" "bay" "bed" "bee" "beg" "bet" "bid" "big" "bit" "bob"
     "bot" "bow" "box" "boy" "but" "cab" "can" "cap" "car" "cat" "cog" "cop"
     "cow" "cry" "cup" "cut" "day" "dew" "did" "die" "dig" "dim" "dip" "dog"
     "dot" "dry" "dub" "dug" "dye" "ear" "eat" "eel" "egg" "ego" "elf" "eve"
     "eye" "fan" "far" "fat" "fax" "fee" "few" "fin" "fit" "fix" "flu" "fly"
     "foe" "fog" "for" "fox" "fry" "fun" "fur" "gag" "gap" "gas" "gel" "gem"
     "get" "gig" "gin" "gnu" "god" "got" "gum" "gun" "gut" "guy" "gym" "had"
     "hag" "ham" "has" "hat" "her" "hid" "him" "hip" "his" "hit" "hop" "hot"
     "how" "hub" "hue" "hug" "hut" "ice" "icy" "imp" "ink" "inn" "ion" "ire"
     "ivy" "jab" "jam" "jar" "jaw" "jet" "job" "jog" "joy" "key" "kid" "kit"
     "lag" "lap" "lay" "let" "lid" "lie" "lip" "lit" "lob" "log" "lot" "low"
     "mad" "man" "map" "mat" "may" "men" "met" "mix" "mob" "mop" "mud" "mug"
     "nag" "nap" "new" "nil" "nod" "nor" "not" "now" "nun" "oak" "odd" "off"
     "oil" "old" "one" "orb" "ore" "ork" "our" "out" "owl" "own" "pad" "pan"
     "par" "pat" "paw" "pay" "pea" "pen" "pet" "pig" "pin" "pit" "pod" "pot"
     "pry" "pub" "pun" "put" "rag" "ram" "ran" "rat" "raw" "ray" "red" "rib"
     "rim" "rip" "rob" "rod" "rot" "row" "rub" "rug" "rum" "run" "sad" "sat"
     "saw" "say" "sea" "see" "sew" "she" "shy" "sin" "sip" "sit" "six" "ski"
     "sky" "sly" "sob" "son" "soy" "spy" "sum" "sun"  "urn" "use" "van"))
  (defun avy-handler-default (char)
    "The default handler for a bad CHAR."
    (let (dispatch)
      (cond ((setq dispatch (assoc char avy-dispatch-alist))
             (setq avy-action (cdr dispatch))
             (throw 'done 'restart))
            ((memq char avy-escape-chars)
             ;; exit silently
             (throw 'done 'abort))
            ((eq char ??)
             (avy-show-dispatch-help)
             (throw 'done 'restart))
            ((mouse-event-p char)
             (signal 'user-error (list "Mouse event not handled" char)))
            (t
             (message "No such candidate: %s, hit `C-g' to quit."
                      (if (characterp char) (string char) char)))))))

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
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line)))


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

(use-package denote
  :straight (:host github :repo "protesilaos/denote")
  :defer t
  :init
  (mememacs/comma-def "oj"
    (defun mm/denote-load ()
      (interactive)
      (require 'init-denote)
      (mm/find-today-journal))))

(use-package markdown-mode)

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
    (bash-completion-capf-1 #'point-at-bol))
  (add-hook
   'sh-mode-hook
   (defun mm/add-bash-completion ()
     (add-hook 'completion-at-point-functions #'bash-completion-capf nil t))))

(use-package backup-each-save
  :config
  (add-hook 'after-save-hook #'backup-each-save)
  (setf make-backup-files nil))

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

(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(use-package shell
  :ensure nil
  :config
  (defun mm/with-current-window-buffer (f &rest args)
    (with-current-buffer
	(window-buffer (car (window-list)))
      (apply f args)))

  (defun mm/shell-via-async-shell-command ()
    (switch-to-buffer
     (window-buffer
      (async-shell-command
       shell-file-name))))

  (advice-add #'mm/shell-via-async-shell-command :around #'mm/with-current-window-buffer)

  (setf shell-kill-buffer-on-exit t)

  (add-hook
   'shell-mode-hook
   (defun mm/shell-dont-mess-with-scroll-conservatively ()
     (setq-local scroll-conservatively 0))))

(use-package iedit
  :after general
  :init
  (setq iedit-toggle-key-default nil)
  :config
  (setq iedit-toggle-key-default (kbd "C-/"))
  (let ((key iedit-toggle-key-default)) (define-key global-map key 'iedit-mode)
       (define-key isearch-mode-map key 'iedit-mode-from-isearch)
       (define-key esc-map key 'iedit-execute-last-modification)
       (define-key help-map key 'iedit-mode-toggle-on-function))
  (mememacs/comma-def "i" #'iedit-mode))


(use-package elfeed
  :defer t
  :config
  (setq
   elfeed-feeds
   '("http://nullprogram.com/feed/"
     "https://planet.emacslife.com/atom.xml"
     "https://vlaaad.github.io/feed.xml"
     "https://blog.michielborkent.nl/atom.xml"
     "https://writepermission.com/rss.xml"
     "https://benjamin-asdf.github.io/faster-than-light-memes/planetclojure.xml"
     "https://benjamin-asdf.github.io/faster-than-light-memes/atom.xml")))


(use-package emacs
  :config
  (setq save-abbrevs 'silently)
  (setf create-lockfiles nil)

  ;; Unify Marks
  (setq global-mark-ring-max 256)
  (setq set-mark-command-repeat-pop 256)

  (defun push-mark (&optional location nomsg activate)
    "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil."
    (when (mark t)
      (let ((old (nth mark-ring-max mark-ring))
            (history-delete-duplicates nil))
        (add-to-history 'mark-ring (copy-marker (mark-marker)) mark-ring-max t)
        (when old
          (set-marker old nil))))
    (set-marker (mark-marker) (or location (point)) (current-buffer))
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history
       'global-mark-ring (copy-marker (mark-marker)) global-mark-ring-max t)
      (when old
        (set-marker old nil)))
    (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
        (message "Mark set"))
    (if (or activate (not transient-mark-mode))
        (set-mark (mark t)))
    nil)

  (mememacs/leader-def ";" #'consult-global-mark)

  (setq async-shell-command-buffer 'new-buffer)

  (defun path-slug (dir)
    "Returns the initials of `dir`s path,
with the last part appended fully

Example:

(path-slug \"/foo/bar/hello\")
=> \"f/b/hello\" "
    (require 'dash)
    (let* ((path (replace-regexp-in-string "\\." "" dir))
	   (path (split-string path "/" t))
	   (path-s (mapconcat
		    (lambda (it)
		      (cl-subseq it 0 1))
		    (-drop-last 1 path)
		    "/"))
	   (path-s (concat
		    path-s
		    "/"
		    (-last-item path))))
      path-s))

  (defun mm/put-command-in-async-buff-name (f &rest args)
    (let* ((path-s (path-slug default-directory))
	   (command (car args))
	   (buffname (concat path-s " " command))
	   (shell-command-buffer-name-async
	    (format
	     "*async-shell-command %s*"
	     (string-trim
	      (substring buffname 0 (min (length buffname) 50))))))
      (apply f args)))

  (advice-add 'shell-command :around #'mm/put-command-in-async-buff-name)

  (add-hook 'comint-mode-hook
	    (defun mm/do-hack-dir-locals (&rest _)
	      (hack-dir-local-variables-non-file-buffer)))

  (advice-add #'start-process-shell-command :before #'mm/do-hack-dir-locals)

  (advice-add 'compile :filter-args
	      (defun mm/always-use-comint-for-compile (args) `(,(car args) t))))

;; elp
;; memory-use-counts
;; instrument package
;; epl results

;; figure out where the code is for guix packages

					; pprint



(use-package meow
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
	  meow-use-cursor-position-hack t)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))




;; (use-package boon
;;   :config
;;   (require 'boon-qwerty)
;;   (boon-mode))


(require 'mememacs-stumpwm)
