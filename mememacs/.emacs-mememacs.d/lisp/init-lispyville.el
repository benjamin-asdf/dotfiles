;; Lispy  -*- lexical-binding: t; -*-

(require 'lispy)

(defun mememacs/lispy-insert ()
  "Call `lispy-space' with prefix arg 4 if special, `lispy-meta-return' otherwise"
  (interactive)
  (if (lispyville--special-p)
      (lispy-space 4)
    (lispy-meta-return)))

(setf
 lispy-eval-display-style 'overlay
 lispyville-motions-put-into-special t
 lispyville-commands-put-into-special t
 lispy-no-permanent-semantic t
  lispy-completion-method 'deafult
  ;; todo lispy occur
  ;; or figure out consult line narrowing
 lispy-occur-backend 'ivy
 lispy-teleport-global t
 lispy-avy-keys mememacs/avy-keys
 lispy-x-default-verbosity 1)

(lispyville--define-key 'insert
  (kbd "<backspace>") 'lispy-delete-backward
  (kbd "M-<backspace>") 'lispyville-delete-backward-word
  ";" 'lispy-comment
  "(" 'lispy-parens
  ")" 'lispy-right-nostring

  (kbd "C-h") #'mm/lispy-delete-and-blanks

  (kbd "M-(") #'lispy-wrap-round
  (kbd "M-{") #'lispy-wrap-braces
  (kbd "M-[") #'lispy-wrap-brackets

  (kbd "M-l") #'lispyville-next-opening
  (kbd "M-K") #'lispy-move-left
  (kbd "M-J") #'lispy-move-right
  (kbd "<M-return>") #'mememacs/lispy-insert
  (kbd "C-k") #'lispy-kill-at-point
  (kbd "C-<return>") #'lispy-alt-line

  "P" #'special-lispy-eval-other-window
  "p" #'special-lispy-paste
  (kbd "H-y")
  (defun mm/lispy-comment-and-clone-dwim ()
    (interactive)
    (lispy-clone 1)
    (lispy-comment 1)))

(defun mm/lispy-delete-and-blanks (arg)
  (interactive "p")
  (save-excursion
    (forward-char -1)
    (delete-blank-lines))
  (lispy-delete-backward arg))

(lispyville-set-key-theme
 '(
   ;; operators set manualy below
   c-w
   prettify
   slurp/barf-lispy
   additional
   additional-motions
   atom-movement
   commentary
   additional-wrap
   additional-insert
   mark-toggle
   slurp/barf-lispy))

;; copied from lispyville.el
;; I do not want the substitute part because of evil-sourround
(lispyville--define-key nil
  [remap evil-yank] #'lispyville-yank
  [remap evil-delete] #'lispyville-delete
  [remap evil-change] #'lispyville-change
  [remap evil-yank-line] #'lispyville-yank-line
  [remap evil-delete-line] #'lispyville-delete-line
  [remap evil-change-line] #'lispyville-change-line
  [remap evil-delete-char] #'lispyville-delete-char-or-splice
  [remap evil-delete-backward-char]
  #'lispyville-delete-char-or-splice-backwards
  [remap evil-change-whole-line] #'lispyville-change-whole-line
  [remap evil-join] #'lispyville-join)

(lispyville--define-key '(motion normal)
  "q" 'lispy-ace-paren
  "Y" 'lispy-new-copy
  (kbd "S-<return>") 'lispy-eval-other-window

  "D" 'lispy-kill)



(lispyville--define-key '(motion normal visual)
  (kbd "^") #'lispy-left
  (kbd "M-h") (lispyville-wrap-command lispyville-previous-opening special)
  (kbd "M-l") (lispyville-wrap-command lispyville-next-opening special)
  (kbd "M-j") (lispyville-wrap-command lispy-down special)
  (kbd "M-k") (lispyville-wrap-command lispy-up special)

  (kbd "C-j") #'lispyville-drag-forward
  (kbd "C-k") #'lispyville-drag-backward

  (kbd "C-p") #'lispy-kill-at-point

  (kbd "s-<backspace>") #'lispyville-delete-whole-line

  (kbd "M-L") #'lispy-move-right
  (kbd "S-C-<return>") #'lispy-join


  (kbd "C-1") #'lispy-describe-inline
  (kbd "C-2") #'lispy-arglist-inline

  (kbd "C-4") #'lispy-x
  (kbd "gd") #'lispy-goto-symbol

  ;; (kbd "C-l") (lispyville-wrap-command lispy-forward special)
  (kbd "C-f") (lispyville-wrap-command lispy-flow special)

  (kbd "C-m") (lispyville-wrap-command lispy-mark-symbol special)

  (kbd "C-3") #'lispyville-up-list
  "=" #'lispyville-prettify

  (kbd "M-m") (lispyville-wrap-command lispy-mark-symbol special))


(with-eval-after-load 'targets
  (mememacs/init-lispy-targets))

(defun mememacs/init-lispy-targets ()
  (setq targets-text-objects nil)
  (targets-setup)
  (targets-define-to lispyville-comment 'lispyville-comment nil object
                     :last-key nil
                     :bind t :keys "c")
  (targets-define-to lispyville-atom 'lispyville-atom nil object
                     :last-key nil
                     :bind t :keys "m")
  (targets-define-to lispyville-list 'lispyville-list nil object
                     :last-key nil
                     :bind t :keys "k")
  (targets-define-to lispyville-sexp 'lispyville-sexp nil object
                     :last-key nil
                     :bind t :keys "x")
  (targets-define-to lispyville-function 'lispyville-function nil object
                     :last-key nil
                     :bind t :keys "f")
  (targets-define-to lispyville-string 'lispyville-string nil object
                     :last-key nil
                     :bind t :keys "s")
  (general-def 'evil-inner-text-objects-map
    "S" 'evil-inner-sentence)
  (general-def 'evil-outer-text-objects-map
    "S" 'evil-a-sentence))

(with-eval-after-load 'lispy
  (require 'patch-lispy nil :noerror))

(defun mm/add-lispy-to-incompatible-minor-modes ()
  (unless (boundp 'evil-mc-incompatible-minor-modes)
    (evil-mc-define-vars))
  (setf
   evil-mc-incompatible-minor-modes
   (delete-dups
    (append
     '(lispy-mode)
     evil-mc-incompatible-minor-modes))))

(with-eval-after-load
    'evil-mc
  (add-hook
   'evil-mc-mode-hook
   #'mm/add-lispy-to-incompatible-minor-modes))

(if (require 'slime nil 'noerror)
    ;; REVIEW: Fix SLIME REPL issue with "goto".
    ;; See https://github.com/abo-abo/lispy/issues/182.
    ;; Remove once Guix package is updated.
    (progn
      (add-to-list 'lispy-goto-symbol-alist
                   '(slime-repl-mode lispy-goto-symbol-lisp le-lisp))
      (add-to-list 'lispy-goto-symbol-alist
                   '(slime-mrepl-mode lispy-goto-symbol-lisp le-lisp)))
  (progn
    (add-to-list 'lispy-goto-symbol-alist
                 '(sly-mrepl-mode lispy-goto-symbol-lisp le-lisp))
    (setq lispy-use-sly t)))

(set-face-foreground 'lispy-face-hint "#FF00FF")
(add-hook 'lispy-mode-hook 'lispyville-mode)


;; todo swap prefix arg
;; (defadvice lispy-ace-paren ())
;; (defalias 'lispy--remember #'evil--jumps-push)

(defun mm/lispyville-out-and-eval ()
  (interactive)
  (lispyville-escape nil)
  (lispyville-end-of-defun)
  (evil-insert 0)
  (call-interactively #'special-lispy-eval))

(defun mm/lispyville-forward-and-eval (&optional arg)
  (interactive "p")
  (lispy-forward 1)
  (lispy-eval arg))

(let ((mark-fn (lispyville-wrap-command lispy-mark-symbol special)))
  (defun mm/lispy-goto-toplevel-form ()
    (interactive "P")
    (lispyville-beginning-of-defun)
    (call-interactively #'lispyville-escape)
    (forward-char 1)
    (lispyville-forward-sexp)
    (lispyville-forward-sexp)
    (if (not arg)
	(funcall-interactively mark-fn)
      (embark-act))))

;; (advice-add 'lispy-parens-down)

(mememacs/local-def
  :keymaps '(lispy-mode-map)
  "b" #'lispy-back
  "l" #'mm/lispyville-out-and-eval
  "L" (lispyville-wrap-command lispyville-end-of-defun special)
  "," #'lispy-kill-at-point
  "g" (lispyville-wrap-command lispy-beginning-of-defun special)
  "G" (lispyville-wrap-command lispyville-end-of-defun special)
  "f" #'mm/lispy-goto-toplevel-form)

(general-def
  :states '(normal visual emacs insert)
  :keymaps '(lispy-mode-map lispyville-mode-map)
  "C-e" (lispyville-wrap-command lispyville-end-of-defun special)
  "C-l" #'mm/lispyville-forward-and-eval)

(defun mememacs/lispy-occur-consult ()
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (consult-line)
    (widen)))

(defalias #'lispy-occur #'mememacs/lispy-occur-consult)

(defun mememacs/lispy-set-faces ()
  (if
      (and (eq evil-state 'insert)
	   (lispyville--special-p))
      (set-face-attribute
       'show-paren-match
       nil
       :foreground mindsape/heliotrope
       :underline t)
    (set-face-attribute
     'show-paren-match
     nil
     :foreground mindsape/mint-bright-2
     :underline t)))

(setf show-paren-style 'parenthesis)

(add-hook 'evil-insert-state-entry-hook #'mememacs/lispy-set-faces)
(add-hook 'evil-normal-state-entry-hook #'mememacs/lispy-set-faces)

(defun mm/lispy-advice-print-length (f r)
  "This is so you do not get '...' all the time
when formatting with lispy."
  (let ((print-length 2000)
	(print-level nil))
    (funcall f r)))

(advice-add
 #'lispy--insert
 :around
 #'mm/lispy-advice-print-length)

(add-hook 'mememacs/escape-functions #'lispy--cleanup-overlay)


(provide 'init-lispyville)
