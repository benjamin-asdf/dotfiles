(setq vertico-cycle t)
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  (setq enable-recursive-minibuffers t)

(general-def
  :keymap vertico-map
  "C-k" #'previous-line
  "M-k" #'backward-paragraph
  "M-j"  #'forward-paragraph)

(add-to-list
 'load-path
 (concat mememacs/config-dir
	 "/straight/repos/vertico/extensions/"))

(require 'vertico-directory)

(general-def
  :keymap vertico-map
  "RET"  #'vertico-directory-enter
  "DEL"  #'vertico-directory-delete-char
  "M-DEL" #'vertico-directory-delete-word)

(add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)

(require 'vertico-quick)

(general-def
  :keymap 'vertico-map
  "M-q" #'vertico-quick-insert
  "M-," #'vertico-quick-exit)

(mememacs/leader-def
  "bb" #'consult-buffer)

(require 'vertico-repeat)

(mememacs/comma-def
  "rl" #'vertico-repeat)

(provide 'init-vertico)
