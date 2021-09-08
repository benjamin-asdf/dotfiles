;; Lispy

(require 'lispy)
(defun ambrevar/lispy-eval (&optional replace)
  "Like `lispy-eval' but if called with a prefix argument,
replace the expression with its result."
  (interactive "P")
  (if replace
      (lispy-eval-and-replace)
    (call-interactively #'lispy-eval)))

(setf
 lispyville-motions-put-into-special t
 lispyville-commands-put-into-special t
 lispy-no-permanent-semantic t
 ;; lispy-occur-backend 'helm
 ;; todo patch `lispy--occur-update-input'
 lispy-occur-backend 'ivy
 lispy-x-default-verbosity 1)

(lispyville--define-key 'insert
  (kbd "<backspace>") 'lispy-delete-backward
  (kbd "M-<backspace>") 'lispyville-delete-backward-word
  ";" 'lispy-comment
  ;; ":" 'lispy-colon ; The colon is not always used to delimit keys.
  "(" 'lispy-parens
  ")" 'lispy-right-nostring

  (kbd "C-h") #'lispy-delete-backward
  (kbd "M-K") #'lispy-move-left
  (kbd "M-J") #'lispy-move-right)

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
  ;; "f" 'lispy-ace-paren
  ;; "Q" 'lispy-ace-symbol
  ;; "t" 'lispy-ace-char
  "Y" 'lispy-new-copy
  (kbd "S-<return>") 'lispy-eval-other-window
  ;; "p" 'lispy-paste

  "D" 'lispy-kill)

(lispyville--define-key '(motion normal visual)
  (kbd "^") #'lispy-left
  (kbd "M-h") #'lispyville-previous-opening
  (kbd "M-l") #'lispyville-next-opening
  (kbd "M-j") #'lispy-down
  (kbd "M-k") #'lispy-up

  (kbd "C-j") #'lispyville-drag-forward
  (kbd "C-k") #'lispyville-drag-backward

  (kbd "C-d") #'lispy-kill-at-point


  (kbd "M-L") #'lispy-move-right
  (kbd "C-x C-e") #'ambrevar/lispy-eval
  ;; (kbd "C-<return>") #'lispy-split
  (kbd "S-C-<return>") #'lispy-join


  (kbd "C-1") #'lispy-describe-inline
  (kbd "C-2") #'lispy-arglist-inline

;;;;;;;;
  (kbd "C-4") #'lispy-x
  (kbd "gd") #'lispy-goto-symbol
  ;; (kbd "M-<backspace>") 'lispyville-delete-backward-word

  ;; (kbd "/") #'lispy-occur
  ;; (kbd "M-;") #'lispy-comment ; This conflicts with `iedit-toggle-selection' default binding.
  ;; TODO: lispy-eval-and-replace
  ")" #'lispy-right
  (kbd "C-3") #'lispyville-up-list
  "=" #'lispyville-prettify)


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
                     :bind t :keys "S"))


(with-eval-after-load 'lispy
  (require 'patch-lispy nil :noerror))

(with-eval-after-load
    'evil-mc
  (when (boundp 'evil-mc-incompatible-minor-modes)
    (add-to-list 'evil-mc-incompatible-minor-modes 'lispy-mode)))

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



(with-eval-after-load 'evil-goggles
  (setq evil-goggles--commands
        (append evil-goggles--commands
                '((lispyville-delete :face evil-goggles-delete-face :switch evil-goggles-enable-delete :advice evil-goggles--generic-blocking-advice)
                  (lispyville-delete-line :face evil-goggles-delete-face :switch evil-goggles-enable-delete :advice evil-goggles--delete-line-advice)
                  (lispyville-yank :face evil-goggles-yank-face :switch evil-goggles-enable-yank :advice evil-goggles--generic-async-advice)
                  (lispyville-yank-line :face evil-goggles-yank-face :switch evil-goggles-enable-yank :advice evil-goggles--generic-async-advice)
                  (lispyville-change :face evil-goggles-change-face :switch evil-goggles-enable-change :advice evil-goggles--generic-blocking-advice)
                  (lispyville-change-line :face evil-goggles-change-face :switch evil-goggles-enable-change :advice evil-goggles--generic-blocking-advice)
                  (lispyville-change-whole-line :face evil-goggles-change-face :switch evil-goggles-enable-change :advice evil-goggles--generic-blocking-advice)
                  (lispyville-join :face evil-goggles-join-face :switch evil-goggles-enable-join :advice evil-goggles--join-advice)
                  (lispy-fill :face evil-goggles-fill-and-move-face :switch evil-goggles-enable-fill-and-move :advice evil-goggles--generic-async-advice))))
  (evil-goggles-mode))

(with-eval-after-load 'cider
  (setf
   cider-jack-in-dependencies
   (delete-dups
    (append
     cider-jack-in-dependencies
     lispy-cider-jack-in-dependencies))))

(defalias 'lispy--remember #'evil--jumps-push)

(general-def
  :keymaps '(lispy-mode-map)
  :states '(normal motion)
  :prefix "SPC"
  "kf" #'lispy-flow
  "kF" #'lispyville-end-of-defun
  "kl" #'lispy-forward
  "km" (lispyville-wrap-command lispy-mark-symbol special)

  )


;; (defun lispy-clojure-complete-at-point () ())

(defadvice lispy-clojure-complete-at-point (around mm/lispy-complete-advice activate)
  (unless (or (lispy--in-string-p) (lispy--in-comment-p))
    ad-do-it))


;; todo mode line

(provide 'init-lispyville)
