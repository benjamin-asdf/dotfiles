;;; Main options

(require 'functions)

;;; Minimal UI. Run early to hide it as soon as possible.
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
;;; `tool-bar-mode' and `scroll-bar-mode' might not be compiled in.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))


;; (set-fringe-mode 10)
;; (setq inhibit-startup-message t)
;; (setq visible-bell 1)


;;; In some cases, Emacs can still decide by itself to use graphical boxes.
;;; Force on using the minibuffer instead.
(setq use-dialog-box nil)

;;; Timeout before echoing the prefix of an unfinished keystroke.
(setq echo-keystrokes 0.5)

;;; Remember last cursor position.
(save-place-mode)
;;; When the daemon is killed abruptly, places are not saved. Adding this hook
;;; allows to save places at a strategic moment.
(add-hook 'before-save-hook 'save-place-kill-emacs-hook)

;;; Recent files.
(setq recentf-max-saved-items 40)

;;; Save M-: history.
(savehist-mode)

;;; Disable autosave features.
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;;; Place backup files in specific directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;;; Default mode
(setq-default major-mode 'emacs-lisp-mode)

;;; Disable suspend key since it is useless on Emacs server.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Make questions less annoying.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Enable all disabled commands.
(setq disabled-command-function nil)

;;; Print buffer size in mode line.
(size-indication-mode 1)

;;; Display defun in mode line.
;; (which-function-mode)

;;; No need when we have a status bar.
;; (display-time)
;; (setq display-time-day-and-date t
;;       display-time-24hr-format t
;;       display-time-default-load-average nil)

;;; Just like time, no need when we have a status bar.
;; (display-battery-mode)
;;; TODO: Battery status (%b) does not work properly.
;; (setq battery-mode-line-format "[%p%%%b %t]")

;;; Line numbers
;;; Adding to `find-file-hook' ensures it will work for every file, regardless of
;;; the mode, but it won't work for buffers without files nor on mode change.
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'ambrevar/turn-on-column-number-mode)
  (add-hook hook 'ambrevar/turn-off-line-number-mode)
  (add-hook hook 'display-line-numbers-mode))
(setq display-line-numbers-type 'visual)
;;; Emacs-nox does not display a fringe after the linum: Setting linum-format in
;;; linum-before-numbering-hook is not the right approach as it will change the
;;; type of linum-format in the middle. See linum-update-window.
;;; See http://stackoverflow.com/questions/3626632/right-align-line-numbers-with-linum-mode
;;; and http://stackoverflow.com/questions/3626632/right-align-line-numbers-with-linum-mode.
;;; The complexity is not worth the benefit.

;;; Alternative scrolling
(setq scroll-error-top-bottom t)

;;; Kill whole line including \n.
(setq kill-whole-line t)

;;; Indentation
(setq-default tab-width 2)
(defvaralias 'standard-indent 'tab-width)
(setq-default indent-tabs-mode t)

;;; Line by line scrolling
(setq scroll-step 1)

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

;;; Cycle spacing instead of just-one-space.  This frees M-\.
(global-set-key [remap just-one-space] 'cycle-spacing)

;;; Hippie expand
;; (global-set-key (kbd "M-/") 'hippie-expand)

;;; Abbreviation is like snippets: annoying at times, especially in
;;; prog-mode.  They are useful in text mode to avoid the sprawling of
;;; abbreviations.
(add-hook 'text-mode-hook 'abbrev-mode)

;;; Auto-fill
(when (getenv "MANWIDTH")
  (setq-default fill-column (string-to-number (getenv "MANWIDTH"))))
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (setq sentence-end-double-space nil)

;;; Enforce horizontal splitting. 140 means that the window is large enough to
;;; hold 2 other windows of 70 columns.
(setq split-height-threshold nil
      split-width-threshold 140)

;;; Windmove mode
;;; By default, it allows easy window switching with Shift+arrows. I like to
;;; stick to the home-row, but to avoid shadowing other binding I exceptionaly use
;;; 'super' (normally reserved to the WM).
;; (when (fboundp 'windmove-default-keybindings)
;;   (ambrevar/global-set-keys
;;    "s-h" 'windmove-left
;;    "s-j" 'windmove-down
;;    "s-k" 'windmove-up
;;    "s-l" 'windmove-right))
;; (ambrevar/global-set-keys
;;  "s-o" 'delete-other-windows
;;  ;; "s-w" 'other-window
;;  "s-d" 'delete-window)


;; shr
(setq shr-width (string-to-number (or (getenv "MANWIDTH") "80")))

;;; Show matching parenthesis
(show-paren-mode 1)
;;; By default, there’s a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)

(set-face-foreground 'show-paren-match "White")
(set-face-underline 'show-paren-match "White")
(setq show-paren-style 'parenthesis)


;;; Electric Pairs to auto-complete () [] {} "" etc. It works on regions.
(electric-pair-mode)

;;; Spawn terminal shortcut: WM's binding is s+<return>.
(global-set-key (kbd "C-x M-<return>") 'spawn-terminal)


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
(ambrevar/define-keys prog-mode-map
                      "C-<f6>" 'compile
                      ;; Do not use `recompile' since we want to change the compilation folder for the current buffer.
                      "<f6>" 'ambrevar/compile-last-command)

;;; REVIEW: Bug 26658 reports that cc-modes mistakenly does not make use of prog-mode-map.
;;; The following line is a suggested work-around.
;;; This should be fixed in Emacs 26.
(eval-after-load 'cc-mode '(set-keymap-parent c-mode-base-map prog-mode-map))

;;; Comint mode
(setq comint-prompt-read-only t)

;;; Buffer names.
(setq uniquify-buffer-name-style 'forward)

;;; Skeleton settings
;;; Do not expand abbrevs in skeletons.
(setq-default skeleton-further-elements '((abbrev-mode nil)))
(ambrevar/turn-on-skeleton-markers)
(ambrevar/global-set-keys
 "C->" 'skeleton-next-position
 "C-<" 'skeleton-previous-position)

;;; Disable prompt (but leave warning) on git symlink.
(setq vc-follow-symlinks t)

;;; Clipboard and primary selection.
;; (setq select-enable-clipboard t)
(setq select-enable-primary t
      save-interprogram-paste-before-kill t)

;;; Move mouse away.
(mouse-avoidance-mode 'banish)
;;; That binding is not very useful and gets in the way of C-<mouse-1>.
(global-unset-key (kbd "C-<down-mouse-1>"))

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

;;; Trash
(setq delete-by-moving-to-trash t)

;;; Frame title
(setq frame-title-format (concat "%b" (unless (daemonp) " [serverless]")))

;;; Initial scratch buffer message.
(require 'functions) ; For `ambrevar/fortune-scratch-message'.
(let ((fortune (ambrevar/fortune-scratch-message)))
  (when fortune
    (setq initial-scratch-message fortune)))

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

;;  so long

(global-so-long-mode 1)



(provide 'main)
