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
  "M-k" #'backward-paragraph
  "M-j"  #'forward-paragraph
  "M-f" nil
  "M-f g" #'beginning-of-buffer
  "M-f G" #'end-of-buffer)

(add-to-list
 'load-path
 (concat mememacs/config-dir
	 "/straight/repos/vertico/extensions/"))

(require 'vertico-directory)

(general-def
 'vertico-map
  "RET"  #'vertico-directory-enter
  "DEL"  #'vertico-directory-delete-char
  "M-DEL" #'vertico-directory-delete-word)

(add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)

(require 'vertico-quick)

(general-def
  'vertico-map
  "C-k" #'vertico-quick-exit
  "M-q" #'vertico-quick-insert
  "M-a" #'vertico-quick-jump)

(setq vertico-quick1
      "adf"
      vertico-quick2
      "jkl")

(mememacs/leader-def
  "bb" #'consult-buffer)

(require 'vertico-repeat)


(mememacs/comma-def
  "rl" #'vertico-repeat)

(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(when nil
  (defun mememacs/vertico-select-when-single (&rest args)
    (interactive)
    (ignore args)
    (when
	(and
	 (eq 1 (length vertico--candidates))
	 (eq 0 vertico--index))
      (minibuffer-force-complete-and-exit)))

  (advice-add
   #'vertico--update-candidates
   :after
   #'mememacs/vertico-select-when-single))

(provide 'init-vertico)
