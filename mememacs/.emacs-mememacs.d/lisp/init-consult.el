(setq-default completion-in-region-function #'consult-completion-in-region)

(general-def
  isearch-mode-map
  "H-/" #'consult-line)

(mememacs/local-def
  "SPC" #'consult-mode-command)

(mememacs/comma-def
  "ss" #'consult-line
  "sS" #'consult-line-multi

  "sk" #'consult-keep-lines

  "sf" #'consult-focus-lines
  "g/" #'consult-git-grep

  "fl" #'consult-locate
  "ff" #'consult-find
  "fo" #'consult-file-externally
  "hw" #'consult-man
  "M" #'consult-minor-mode-menu)

(general-def
  'minibuffer-mode-map
  "M-h" #'consult-history
  "M-i" #'completion-at-point)

(general-def
  :prefix
  "H-m"
  "M" #'consult-register-store
  "m" #'consult-register
  "b" #'consult-bookmark)

(mememacs/leader-def
  "SPC" #'execute-extended-command
  "bb" #'consult-buffer
  "bB" #'consult-buffer-other-window

  "sk" #'consult-keep-lines

  "sf" #'consult-focus-lines

  "ss" #'consult-line
  "sS" #'consult-line-multi
  "ff" #'find-file
  "fr" #'consult-recent-file

  "ji" #'consult-imenu
  "jI" #'consult-imenu-multi
  ;; info?
  "m" #'consult-global-mark

  "jL" #'consult-goto-line
  "jo" #'consult-org-heading
  "jO" #'consult-outline
  ;; org-agenda
  "sb" #'consult-multi-occur

  ":" #'consult-complex-command

  "ha" #'consult-apropos

  "e" nil
  "en" #'consult-compile-eror
  ;; flycheck
  ;; "ef" #'consult-flymake

  "/" #'consult-ripgrep)

(general-def
  "H-SPC" #'consult-line
  "H-m ." (lambda () (interactive) (push-mark)))

(general-def
  "M-y" #'yank-pop
  [remap yank-pop] #'consult-yank-pop)

(consult-customize
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-bookmark consult--source-recent-file
 consult--source-project-recent-file
 consult-buffer
 :preview-key (kbd "M-."))

(defun mm/dired-find-file-after-consult (old-fn pos)
  (funcall old-fn pos)
  (when (and pos (eq major-mode 'dired-mode))
    (call-interactively #'dired-find-file)))

(advice-add 'consult--jump :around  #'mm/dired-find-file-after-consult)


(defun mm/consult-grep-dir-prompt-advice (args)
  (pcase args
    (`(,s (4)) `(,s ,default-directory))
    (_ args)))

(declare (mm/consult-grep-dir-prompt-advice '("f" (4)))
	 (mm/consult-grep-dir-prompt-advice '("f" (1))))

(advice-add #'consult--directory-prompt :filter-args #'mm/consult-grep-dir-prompt-advice)

(defun mm/consult-completing-read-add-one-space ()
  (insert " "))

(advice-add
 #'consult-completion-in-region
 :around
 (defun mememacs/advice-consult-completion-in-region (f &rest args)
   (let ((minibuffer-setup-hook
	  (append
	   minibuffer-setup-hook (list #'mm/consult-completing-read-add-one-space))))
     (apply f args))))

(advice-add
 #'consult-yank-pop
 :before
 (defun mm/remove-whitespace-only-from-kill-ring (&rest args)
   (setf kill-ring (cl-remove-if #'s-blank-str? kill-ring))))

(provide 'init-consult)
