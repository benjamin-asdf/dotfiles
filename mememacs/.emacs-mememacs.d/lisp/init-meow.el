(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
      meow-use-cursor-position-hack t
      meow--kbd-undo "C-_"
      meow-use-clipboard t)

(meow--setup-which-key nil)
(setq meow-keypad-describe-keymap-function nil)

(add-hook 'mememacs/escape-functions #'meow-cancel-selection)

(setf meow-keypad-start-keys
      '((?c . ?c)
	(?h . ?h)
	(?x . ?x)
	(?j . ?x)))

(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("l" . meow-right)
 '("h" . meow-left)
 '("," . meow-keypad)
 '("/" . isearch-forward)
 '("<escape>" . ignore))

(meow-leader-define-key
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
 '("bb" . consult-buffer)
 '("bh" . meow-last-buffer)
 (cons "bd"  (defun mm/kill-this-buffer ()
	       (interactive)
	       (kill-buffer (current-buffer))))
 '("bD" . hydra-buffer/kill-current-buffer)
 '("bk" . hydra-buffer/previous-buffer)
 '("bj" . hydra-buffer/next-buffer)
 '("bs" . mm/scratch-el)
 '("bS" . mm/scratch)
 '("br" . revert-buffer)
 '("by" . mememacs/kill-buffer-name)

 '("wd" . delete-window)
 '("wh" . split-window-vertically)
 '("ws" . split-window-below)
 '("wv" . split-window-right)
 '("wD" . ace-delete-window)
 '("ww" . ace-window)
 '("wm" . delete-other-windows)

 '("fe" . mememacs/find-init-file)
 '("fy" . mememacs/copy-file-name-dwim)
 '("fj" . save-buffer)
 '("ff" . consult-find)
 '("l" . consult-line)
 '("L" . mm/consult-line-stay-in-dired)
 '("s" . meow-visit)
 '("/" . meow-keypad-describe-key)
 '("!" . flycheck-mode)
 '("?" . meow-cheatsheet)
 '("ag" . consult-git-grep)
 '("aj" . avy-goto-char-timer)
 '("aw" . avy-goto-word-1)
 '("e" . string-edit-at-point)
 '("&" . mm/shell-command-on-file)
 '("u" . vundo)
 '("r" . vertico-repeat-last)
 '("R" . vertico-repeat-select)

 '("fr" . display-line-numbers-mode))

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

 '("," . meow-keypad)

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

 '("\\" . meow-next-word)
 '("|" . meow-next-symbol)

 '("e" . special-lispy-eval)
 '("E" . special-lispy-eval-and-insert)

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
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("s" . meow-kill)
 '("t" . meow-till)
 '("u" . meow-undo)
 '("U" . undo-redo)
 '("M-u" . meow-undo-in-selection)
 '("v" . meow-inner-of-thing)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("x" . meow-line)
 '("X" . meow-goto-line)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection)
 '("C-z" . recenter)
 '("'" . repeat)
 '("<escape>" . ignore)
 '("/" . isearch-forward))

(meow-define-keys 'insert
  '("C-j" . completion-at-point)
  '("M-u" . mm/copy-word-above))

;; -------------------------------------
;; lispy-eval

;; https://github.com/abo-abo/lispy/issues/639
;; do not fuck around, when the buffer changes while I eval
(defun lispy-eval (arg &optional e-str)
  "Eval the current sexp and display the result.
When ARG is 2, insert the result as a comment.
When at an outline, eval the outline."
  (interactive "p")
  (setq lispy-eval-output nil)
  (condition-case e
      (let ((buff (current-buffer)))
        (cond ((eq arg 2)
               (lispy-eval-and-comment))
              ((and (looking-at lispy-outline)
                    (looking-at lispy-outline-header))
               (lispy-eval-outline))
              (t
               (let ((res (lispy--eval e-str)))
                 (when (memq major-mode lispy-clojure-modes)
                   (setq res (lispy--clojure-pretty-string res)))
                 (when lispy-eval-output
                   (setq res (concat lispy-eval-output res)))
                 (cond ((eq lispy-eval-display-style 'message)
                        (lispy-message res))
                       ((or (fboundp 'cider--display-interactive-eval-result)
                            (require 'cider nil t))
                        (when (equal buff (current-buffer))
                          (cider--display-interactive-eval-result
                           res (cdr (lispy--bounds-dwim)))))
                       ((or (fboundp 'eros--eval-overlay)
                            (require 'eros nil t))
                        (eros--eval-overlay
                         res (cdr (lispy--bounds-dwim))))
                       (t
                        (error "Please install CIDER >= 0.10 or eros to display overlay")))))))
    (eval-error
     (lispy-message (cdr e)))))

;; e without prefix arg is the most important command, the quick-eval
;; e with prefix arg inserts into buffer
(defun mm/lispy--eval (&optional arg)
  (interactive "p")
  (cond ((not lispy-mode)
         (call-interactively
          #'self-insert-command))
        ((memq
          major-mode
          lispy-clojure-modes)
         (if (eq arg 1)
             (cider-eval-last-sexp nil)
           (progn
             (lispy-newline-and-indent-plain)
             (cider-eval-last-sexp t))))
        (t
         (if (eq arg 4)
             (lispy-eval-and-insert)
           (lispy-eval arg)))))

(defun mm/cider-emit-into-popup-buffer (out)
  (cider-emit-into-popup-buffer
   (cider-popup-buffer
    "*mm-lispy-result*"
    nil
    major-mode
    'ancillary)
   (ansi-color-apply out)
   nil
   t))

;; E pops a buffer
;; with prefix arg, I insert
(defun mm/lispy--eval-and-insert (&optional arg)
  (interactive "p")
  (cond ((not lispy-mode)
         (call-interactively
          #'self-insert-command))
        ((memq major-mode lispy-clojure-modes)
         (save-excursion
	   (goto-char
	    (cdr (lispy--bounds-dwim)))
	   (if (eq arg 4)
	       (cider-pprint-eval-last-sexp-to-comment nil)
	     (cider-pprint-eval-last-sexp nil))))
        (t
         (if (eq arg 4)
	     (lispy-eval-and-insert)
           (mm/cider-emit-into-popup-buffer (lispy--eval-dwim))))))

(lispy-define-key meow-insert-state-keymap (kbd "e") 'mm/lispy--eval)
(lispy-define-key meow-insert-state-keymap (kbd "E") 'mm/lispy--eval-and-insert)

;; lispy-eval end


;; thanks https://github.com/noctuid/lispyville
(defun lispyville-end-of-defun ()
  "This is the evil motion equivalent of `end-of-defun'.
This won't jump to the end of the buffer if there is no paren there."
  (interactive)
  (when (<= (- (line-end-position)
	       (point))
            1)
    (forward-line))
  (end-of-defun 1)
  (re-search-backward lispy-right nil t)
  (meow-append))

(defun mm/clear-whitespace-end-of-paren-stack ()
  (interactive)
  (save-excursion
    (let ((end (progn
                 (end-of-defun 1)
                 (re-search-backward lispy-right nil t)
                 (point-marker))))
      (skip-chars-backward " \t\n()[]{}")
      (while (and
              (< (point) end)
              (lispy--in-string-or-comment-p))
        (forward-char 1))
      (while (re-search-forward "[ \t\n]+" end t)
        (replace-match ""))
      (lisp-indent-line))))

(defun mm/c-l ()
  (interactive)
  (if (region-active-p)
      (if (meow--direction-forward-p)
	  (progn (avy-goto-line-below) (end-of-line))
	(progn (avy-goto-line-above) (beginning-of-line)))
    (progn
      (lispyville-end-of-defun)
      (mm/clear-whitespace-end-of-paren-stack))))

(meow-normal-define-key '("C-l" . mm/c-l))
(meow-define-keys 'insert '("C-l" . mm/c-l))

(defun mm/meow-insert (&rest _) (meow-insert))
(advice-add #'lispy-right-nostring :after #'mm/meow-insert)

(advice-add #'lispy-left-maybe :after #'mm/meow-insert)
(define-key lispy-mode-map-lispy (kbd "(") #'lispy-parens)
(define-key lispy-mode-map-lispy (kbd "C-w") #'lispy-kill-at-point)

(defun mm/join-below ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'join-line)
    (join-line 'below)))

(bind-keys
 :map lispy-mode-map-lispy
 ((kbd "(") . lispy-parens)
 ((kbd "C-j") . join-line))

(defun mm/embark-meow-keypad-desribe ()
  (interactive)
  (let ((kmap (meow--keypad-get-keymap-for-describe)))
    (meow-keyboard-quit)
    (embark-bindings-in-keymap kmap)))

(define-key meow-keypad-state-keymap (kbd "?") #'mm/embark-meow-keypad-desribe)

(defun mm/lispy-back-or-lispy-pair (arg)
  (interactive "P")
  (if (region-active-p)
      (lispy-parens arg)
    (lispy-backward (or arg 1))
    (meow-insert)))

(meow-normal-define-key '("(" . mm/lispy-back-or-lispy-pair))

(defun mm/lispy-ace-symbol-window ()
  "Like `lispy-ace-symbol` but from the whole screen.
This is the power I desired."
  (interactive)
  (let ((avy-keys lispy-avy-keys)
        res)
    (avy-with lispy-ace-symbol
      (let ((avy--overlay-offset (if (eq lispy-avy-style-symbol 'at) -1 0)))
        (setq res (lispy--avy-do
                   "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)"
		   (cons (window-start) (window-end))
		   (lambda ()
                     (not (save-excursion
                            (forward-char -1)
                            (lispy--in-string-or-comment-p))))
                   lispy-avy-style-symbol))))
    (unless (memq res '(t nil))
      (unless (or (eq (char-after) ?\")
                  (looking-at ". "))
        (forward-char 1))
      (lispy-mark-symbol))))

(defvar mm/spc-map (let ((m (make-sparse-keymap)))
		     (define-key m (kbd "f")
		       #'find-file)
		     (define-key m (kbd "b")
		       #'consult-buffer)
		     (define-key m (kbd "s")
		       #'magit-status)
		     (define-key m (kbd "p")
		       project-prefix-map)
		     (define-key m (kbd "/")
		       #'consult-ripgrep)
		     (define-key m (kbd "wd")
		       #'delete-window)
		     (define-key m (kbd "wu")
		       #'winner-undo)
		     (define-key m (kbd "wU")
		       #'winner-redo)
		     (define-key m (kbd "d") #'consult-dir)
		     (define-key m (kbd "z") #'recenter)
		     (define-key m (kbd "ju") #'link-hint-open-link)
		     (define-key m (kbd "jl") #'avy-goto-line)
		     (define-key m (kbd "jj") #'avy-goto-char-timer)
		     (define-key m (kbd "ja") #'mm/lispy-ace-symbol-window)
		     (define-key m (kbd "ns") #'mememacs/create-script)
		     (define-key m (kbd "nS") #'mememacs/create-bb-script)
		     m))

(define-key meow-normal-state-keymap (kbd "SPC") mm/spc-map)
(define-key meow-motion-state-keymap (kbd "SPC") mm/spc-map)
(define-key isearch-mode-map (kbd "/") #'isearch-repeat-forward)

(defun mememacs/lispy-occur-consult ()
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (consult-line)
    (widen)))

(defalias #'lispy-occur #'mememacs/lispy-occur-consult)

(defun mm/lispy-advice-print-length (f r)
  "This is so you do not get '...' all the time
when formatting with lispy."
  (let ((print-length 2000)
	(print-level nil))
    (funcall f r)))

(advice-add #'lispy--insert :around #'mm/lispy-advice-print-length)

(add-hook 'mememacs/escape-functions #'lispy--cleanup-overlay)


(defvar mm/c-c-c-j-map
  (let ((m (make-sparse-keymap
	    "mememacs j map")))
    (define-key m (kbd "f") #'find-function)
    (define-key m (kbd "l") #'find-library)
    m))

(define-key global-map (kbd "C-c C-j") mm/c-c-c-j-map)

(defun call-C-c-C-c ()
  (interactive)
  (call-interactively (key-binding (kbd "C-c C-c"))))

(defun call-C-c-C-k ()
  (interactive)
  
  (call-interactively (key-binding (kbd "C-c C-k"))))

(meow-leader-define-key
 '("," . call-C-c-C-c)
 '("k" . call-C-c-C-k))

(meow-leader-define-key
 '("dv" . debug-on-variable-change)
 '("dd" . debug-on-entry)
 '("dr" . trace-function)
 '("dt" . toggle-debug-on-error)
 '("dq" . toggle-debug-on-quit)
 '("dx" . mememacs/cancel-debugs))

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "x") #'magit-discard)
  (define-key magit-status-mode-map (kbd "p") #'magit-push))
(define-key meow-normal-state-keymap (kbd "q") #'lispy-ace-paren)


(define-key meow-normal-state-keymap (kbd "Q") #'lispy-ace-char)

;; lispy

(setf
 lispy-eval-display-style 'overlay
 lispy-safe-delete t
 lispy-safe-copy t
 lispy-safe-paste t
 lispy-safe-actions-no-pull-delimiters-into-comments
 t
 lispy-no-permanent-semantic t
 lispy-completion-method 'default
 ;; todo lispy occur
 ;; or figure out consult line narrowing
 lispy-occur-backend 'ivy
 lispy-teleport-global t
 lispy-avy-keys mememacs/avy-keys
 lispy-x-default-verbosity 1
 lispy-use-sly t)

(defun mm/cider-goto-var (&optional arg)
  (interactive "P")
  (cider-find-var (not arg)))

(setf
 lispy-goto-symbol-alist
 '((clojure-mode mm/cider-goto-var)
   (clojurec-mode mm/cider-goto-var)
   (clojurescript-mode lispy-goto-symbol-clojurescript le-clojure)
   (scheme-mode lispy-goto-symbol-scheme le-scheme)
   (geiser-repl-mode lispy-goto-symbol-scheme le-scheme)
   (racket-mode lispy-goto-symbol-racket le-racket)
   (lisp-mode lispy-goto-symbol-lisp le-lisp)
   (slime-repl-mode lispy-goto-symbol-lisp le-lisp)
   (slime-mrepl-mode lispy-goto-symbol-lisp le-lisp)
   (sly-mrepl-mode lispy-goto-symbol-lisp le-lisp)
   (python-mode lispy-goto-symbol-python le-python)))

(defhydra hydra-lispy-x (:exit t
			       :hint nil
			       :columns 3)
  "x"
  ("b" lispy-bind-variable "bind variable")
  ("c" lispy-to-cond "to cond")
  ("C" lispy-cleanup "cleanup")
  ("d" lispy-to-defun "to defun")
  ("D" lispy-extract-defun "extract defun")
  ("e" lispy-edebug "edebug")
  ("f" lispy-flatten "flatten")
  ("F" lispy-let-flatten "let-flatten")
  ("h" lispy-describe "describe")
  ("i" lispy-to-ifs "to ifs")
  ("j" lispy-debug-step-in "debug step in")
  ("k" lispy-extract-block "extract block")
  ("l" lispy-to-lambda "to lambda")
  ("m" lispy-cursor-ace "multi cursor")
  ("n" lispy-cd)
  ("p" lispy-set-python-process "process")
  ("r" lispy-eval-and-replace "eval and replace")
  ("s" lispy-splice)
  ("t" lispy-view-test "view test")
  ("u" lispy-unbind-variable "unbind let-var")
  ("v" lispy-eval-expression "eval")
  ("w" lispy-show-top-level "where")
  ("B" lispy-store-region-and-buffer "store list bounds")
  ("R" lispy-reverse "reverse")
  ("T" lispy-ert "ert")
  (">" lispy-toggle-thread-last "toggle last-threaded form")
  ("" lispy-x-more-verbosity :exit nil)
  ("?" lispy-x-more-verbosity "help" :exit nil))

(advice-add #'lispy-goto-symbol-clojure :override #'cider-find-var)

(defun mm/lispy-meow-symbol-and-insert ()
  (interactive)
  (meow-insert)
  (lispy-mark-symbol))

;; I want to say lispy ret in insert and
;; lispy mark symbol in normal but only in lispy mode

(define-key
 meow-normal-state-keymap
 (kbd "RET")
 (defun mm/meow-lispy-ret-or-mark-symbol ()
   (interactive)
   (if lispy-mode
       (mm/lispy-meow-symbol-and-insert)
     (call-interactively #'meow-open-below))))

(define-key
 meow-normal-state-keymap
 (kbd "q")
 (defun mm/meow-q-or-lispy ()
   (interactive)
   (if lispy-mode
       (call-interactively #'lispy-ace-paren)
     (call-interactively #'meow-quit))))

(defun mm/lispy-forward-and-insert (arg)
  (interactive "p")
  (lispy-forward arg)
  (meow-insert))

(bind-keys
 :map lispy-mode-map-lispy
 ((kbd "M-(") . lispy-wrap-round)
 ((kbd "M-h") . mm/lispy-back-or-lispy-pair)
 ((kbd "M-l") . mm/lispy-forward-and-insert))
;; I hit this key accidentally 10 times per day

(define-key help-map (kbd "h") (defun mm/no-help-file () (interactive) (message "C-h h, lol")))

(delete-selection-mode 1)

(defvar-local mm/moew-last-normal nil)

(add-hook 'meow-insert-mode-hook
	  (defun mm/remember-last-normal-meow ()
	    (setf mm/moew-last-normal (point))))

(meow-leader-define-key
 (cons (kbd ";")
       (defun mm/goto-last-meow-normal ()
	 (interactive)
	 (when mm/moew-last-normal
	   (goto-char mm/moew-last-normal)))))

(defun mememacs/lispy-set-faces ()
  (if
      (and (eq meow--current-state 'insert)
	   (or
	    (lispy-left-p)
	    (lispy-right-p)
	    (and (lispy-bolp)
		 (or (looking-at lispy-outline-header)
		     (looking-at lispy-outline)))))
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

(setf
 show-paren-style 'parenthesis
 show-paren-context-when-offscreen nil
 ;; 'overlay
 show-paren-when-point-in-periphery t
 show-paren-when-point-inside-paren t)
(add-hook 'meow-insert-mode-hook #'mememacs/lispy-set-faces)

(add-hook 'meow-normal-mode-hook #'mememacs/lispy-set-faces)

(bind-keys
 :map emacs-lisp-mode-map
 ("C-, m" . macrostep-expand))

(define-key lispy-mode-map-lispy (kbd "C-,") nil)
(define-key flycheck-mode-map (kbd "C-c ! !") (defun mm/disable-flycheck-mode () (interactive) (flycheck-mode -1)))

(define-key flycheck-mode-map (kbd "C-c ! ,") #'consult-flycheck)
(global-set-key (kbd "/") #'self-insert-command)

(meow-leader-define-key (cons "o" mm/org-dispatch-map))

(defun mm/meow-right-or-avy ()
  (interactive)
  (let ((ra (region-active-p)))
    (if (and ra
	     (not (equal '(expand . char)
			 (meow--selection-type))))
	(if (meow--direction-forward-p)
	    (progn (avy-goto-line-below)
		   (goto-char (point-at-eol)))
	  (call-interactively #'avy-goto-line-above))
      (meow-right))))

(meow-normal-define-key '("l" . mm/meow-right-or-avy))
(global-set-key (kbd "H-n") #'meow-normal-mode)
(global-set-key (kbd "H-j") #'meow-end-or-call-kmacro)

(global-set-key (kbd "H-k") #'meow-start-kmacro-or-insert-counter)
(global-set-key (kbd "C-x C-e") #'mm/lispy-eval-mark-last-or-consult)
(define-key cider-mode-map (kbd "C-c C-e") #'mm/lispy-eval-mark-last-or-consult)

(meow-leader-define-key '("j e" . #'mm/lispy-eval-mark-last-or-consult))

(define-key dired-mode-map (kbd "M-c") #'magit-clone)

(global-set-key
 (kbd "C-c M-l") #'magit-log-buffer-file)

(advice-add #'embark-act :after
            (defun mm/meow-cancel-selection (&rest _)
              (ignore-errors
                (meow-cancel-selection))))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer)

(define-key help-map (kbd "c") #'describe-char)

(global-set-key (kbd "H-<return>") #'save-buffer)
(global-set-key (kbd "s-r") #'delete-other-windows)

(defun meow-start-isearch-with-last-search ()
  "Start an isearch using the last search string from `meow--push-search'."
  (interactive)
  (if-let ((string (car regexp-search-ring)))
      (if string
          (progn
            (when (region-active-p) (meow-cancel-selection))
            (isearch-mode t t)
            (setq isearch-yank-flag t)
            (isearch-process-search-string string string)
            (isearch-beginning-of-buffer)))
    (message
     "No previous search string found")))

(global-set-key (kbd "H-l") #'meow-start-isearch-with-last-search)

(defun mm/= ()
  (interactive)
  (cond
   (cider-mode (cider-format-defun))
   (t nil)))

(meow-normal-define-key '("=" . mm/=))

(provide 'init-meow)
