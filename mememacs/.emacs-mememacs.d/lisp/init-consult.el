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

(advice-add #'consult--directory-prompt :filter-args #'mm/consult-grep-dir-prompt-advice)

(advice-add
 #'consult-yank-pop
 :before
 (defun mm/remove-whitespace-only-from-kill-ring (&rest args)
   (setf kill-ring (cl-remove-if #'s-blank-str? kill-ring))))



;; https://github.com/minad/consult/wiki#minads-orderless-configuration
(defvar +orderless-dispatch-alist
  '((?% . char-fold-to-regexp)
    (?! . orderless-without-literal)
    (?` . orderless-initialism)
    (?= . orderless-literal)
    (?~ . orderless-flex)))

(defun +orderless--suffix-regexp ()
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
	      consult--tofu-char
	      (+ consult--tofu-char consult--tofu-range -1))
    "$"))

;; Recognizes the following patterns:
;; * ~flex flex~
;; * =literal literal=
;; * %char-fold char-fold%
;; * `initialism initialism`
;; * !without-literal without-literal!
;; * .ext (file extension)
;; * regexp$ (regexp matching at end)
(defun +orderless-dispatch (word _index _total)
  (cond
   ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
   ;; File extensions
   ((string-match-p "\\`\\.." word)
    `(orderless-regexp . ,(concat (substring word 1) (+orderless--suffix-regexp))))
   ;; Ignore single !
   ((equal "!" word) `(orderless-literal . ""))
   ;; Prefix and suffix
   ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
	(cons (cdr x) (substring word 1))
      (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
	(cons (cdr x) (substring word 0 -1)))))))

(declare
 (string-match-p "\\`\\.." ".foo")
 (+orderless-dispatch "!foo" nil nil)
 (+orderless-dispatch ".foo" nil nil))

;; Define orderless style with initialism by default
(orderless-define-completion-style +orderless-with-initialism
  (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

;; You may want to combine the `orderless` style with `substring` and/or `basic`.
;; There are many details to consider, but the following configurations all work well.
;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
;; special styles for special completion categories, e.g., partial-completion for files.
;;
;; 1. (setq completion-styles '(orderless))
;; This configuration results in a very coherent completion experience,
;; since orderless is used always and exclusively. But it may not work
;; in all scenarios. Prefix expansion with TAB is not possible.
;;
;; 2. (setq completion-styles '(substring orderless))
;; By trying substring before orderless, TAB expansion is possible.
;; The downside is that you can observe the switch from substring to orderless
;; during completion, less coherent.
;;
;; 3. (setq completion-styles '(orderless basic))
;; Certain dynamic completion tables (completion-table-dynamic)
;; do not work properly with orderless. One can add basic as a fallback.
;; Basic will only be used when orderless fails, which happens only for
;; these special tables.
;;
;; 4. (setq completion-styles '(substring orderless basic))
;; Combine substring, orderless and basic.
;;
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
      ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
      completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
				      ;; enable initialism by default for symbols
				      (command (styles +orderless-with-initialism))
				      (variable (styles +orderless-with-initialism))
				      (symbol (styles +orderless-with-initialism)))
      orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
      orderless-style-dispatchers '(+orderless-dispatch))

(provide 'init-consult)
