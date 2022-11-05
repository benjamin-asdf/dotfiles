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

;; prepare to be liberated from you own inferior genes!
;; soon getting rid of this
;; (bind-keys)
(use-package general
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

(use-package hydra)

(use-package debug
  :ensure nil
  :config
  (general-def
    debugger-mode-map
    "." #'backtrace-expand-ellipses
    "+" #'backtrace-multi-line
    "-" #'backtrace-single-line))

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
  :bind
  (:map vertico-map
	:prefix "C-,"
	("C-, d" . consult-dir)
	("C-, j" . consult-dir-jump-file)))

(use-package marginalia
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (global-set-key (kbd "H-h") #'embark-bindings)
  :config
  (require 'init-embark)
  :bind
  (:map
   embark-symbol-map ("h" . helpful-symbol)))

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

(use-package multiple-cursors
  :config
  (add-hook
   'mememacs/escape-functions
   (defun mm/mc-remove ()
     (deactivate-mark)
     (mc/remove-fake-cursors))))

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
	aw-background nil))

(use-package cider
  :config
  (require 'init-cider)
  (require 'patch-cider-orderless))

(use-package flycheck
  :config
  (require 'init-flycheck))

(use-package flycheck-clj-kondo
  :after cider)

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

  (define-key symbol-overlay-map (kbd "h") nil))

(use-package link-hint)

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
  :config (winner-mode))

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

  (add-hook 'mememacs/escape-functions (defun mm/iedit-quit ()
					 (when iedit-lib-quit-func (iedit--quit)))))

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

(use-package
  meow
  :config (require 'init-meow)
  (meow-global-mode 1))

;; (use-package boon
;;   :config
;;   (require 'boon-qwerty)
;;   (boon-mode))


;; (require 'mememacs-stumpwm)
