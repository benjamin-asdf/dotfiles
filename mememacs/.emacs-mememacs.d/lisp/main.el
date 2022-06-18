;;; Main options

(require 'functions)

;;; Minimal UI. Run early to hide it as soon as possible.
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
;;; `tool-bar-mode' and `scroll-bar-mode' might not be compiled in.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Give some breathing room
(set-fringe-mode 10)
(setq
 inhibit-startup-message t)

(setf initial-scratch-message "")

(defalias 'display-startup-echo-area-message (lambda ()))

(setq frame-inhibit-implied-resize t)
(setq ad-redefinition-action 'accept)
(setq initial-major-mode 'fundamental-mode)

;;; In some cases, Emacs can still decide by itself to use graphical boxes.
;;; Force on using the minibuffer instead.
(setq use-dialog-box nil)

;;; Timeout before echoing the prefix of an unfinished keystroke.
(setq echo-keystrokes 0.5)

;;; Recent files.
(setq recentf-max-saved-items 40)

;;; Save M-: history.
(savehist-mode)

;;; Disable autosave features.
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;;; Place backup files in specific directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/backups/"))))

;;; Default mode
(setq-default major-mode 'fundamental-mode)

;;; Disable suspend key since it is useless on Emacs server.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Make questions less annoying.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Enable all disabled commands.
(setq disabled-command-function nil)

;; (which-function-mode)

;;; Kill whole line including \n.
(setq kill-whole-line t)


(setq
 whitespace-style
 '(face empty indentation space-after-tab space-before-tab tab-mark trailing))
;;; REVIEW: `whitespace-report' will mistakenly always report empty lines at
;;; beginning and end of buffer as long as there is at least one empty line.
;;; `whitespace-cleanup' works properly however.
;;; Reported at http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23740.
;; (setq whitespace-action '(report-on-bogus))

;;; Add formatting functions to the buffer-local `before-save-hook'.
;;; WARNING: this can break some configuration files needing whitespaces at the
;;; end. This can also slow down saving on big files.  Some modes (e.g. lisp) run
;;; `ambrevar/prettify' in their local hook, which is redundant with this.
;; (add-hook 'find-file-hook 'ambrevar/turn-on-prettify-before-save)
(add-hook 'find-file-hook 'ambrevar/turn-on-delete-trailing-whitespace)


;;; Abbreviation is like snippets: annoying at times, especially in
;;; prog-mode.  They are useful in text mode to avoid the sprawling of
;;; abbreviations.
(add-hook 'text-mode-hook 'abbrev-mode)

;;; Auto-fill
(when (getenv "MANWIDTH")
  (setq-default fill-column (string-to-number (getenv "MANWIDTH"))))
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (setq sentence-end-double-space nil)

;; shr
(setq shr-width (string-to-number (or (getenv "MANWIDTH") "80")))

;;; Show matching parenthesis
(show-paren-mode 1)
;;; By default, there’s a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-style 'parenthesis)

;;; Electric Pairs to auto-complete () [] {} "" etc. It works on regions.
(electric-pair-mode)


;;; Compilation bindings and conveniences.
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)
(with-eval-after-load 'compile
  ;; Making `compilation-directory' local only works with `recompile'
  ;; and if `compile' is never used. In such a scenario,
  ;; `compile-command' is not saved by `recompile' itself which adds a
  ;; lot of bookkeeping.
  ;; (make-variable-buffer-local 'compilation-directory)
  ;; (make-variable-buffer-local 'compile-history)
  (make-variable-buffer-local 'compile-command))
;;; Some commands ignore that compilation-mode is a "dumb" terminal and still display colors.
;;; Thus we render those colors.
(require 'ansi-color)
(defun ambrevar/compilation-colorize-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'ambrevar/compilation-colorize-buffer)
(defun ambrevar/compile-last-command ()
  (interactive)
  (compile compile-command))

;;; Comint mode
(setq comint-prompt-read-only t)

;;; Buffer names.
(setq uniquify-buffer-name-style 'forward)

;;; Disable prompt (but leave warning) on git symlink.
(setq vc-follow-symlinks t)

;;; Clipboard and primary selection.
;; (setq select-enable-clipboard t)
;; TODO
;; (setq select-enable-primary t
;;       save-interprogram-paste-before-kill t)

;;; Move mouse away.
;; (mouse-avoidance-mode 'banish)

;;; Sort
(setq sort-fold-case t)

;;; Replace not-so-useful comment-dwim binding.
(global-set-key (kbd "M-;") 'comment-line)

;;; Replace `kill-buffer' binding by `kill-this-buffer'.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;; Ediff
;;; TODO: Ediff does not seem to auto-refine.  Bug?  Compare daemon and no-daemon.
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;; Frame title
(setq frame-title-format
      (concat "mememacs %b"
	      (unless (daemonp) " [serverless]")))

;;; Support for Emacs pinentry.
;;; Required for eshell/sudo and everything relying on GPG queries.
(setq epa-pinentry-mode 'loopback) ; This will fail if gpg>=2.1 is not available.
(when (require 'pinentry nil t)
  (pinentry-start))

(setq woman-fill-column fill-column)

;; scrolling etc
(setq jit-lock-defer-time 0)
(setq redisplay-skip-fontification-on-input t)
(setq fast-but-imprecise-scrolling t)
(setf scroll-conservatively 0)
(setq scroll-step 0)
(setq scroll-error-top-bottom t)

;;  so long

(global-so-long-mode 1)

(setf browse-url-generic-program (or (getenv "BROWSER") "nyxt"))

(add-hook
 'after-save-hook
 (defun check-parens-when-prog-mode ()
   (when (derived-mode-p 'prog-mode)
     (check-parens))))

;; dired
(setf dired-dwim-target t
      delete-by-moving-to-trash nil)

(setf bookmark-set-fringe-mark nil)
(setf shell-file-name "/bin/bash")

(advice-add 'json-pretty-print :before (lambda (&rest _) (read-only-mode -1)))

(provide 'main)
