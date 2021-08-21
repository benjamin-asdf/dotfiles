;;; Functions-1

;;; -*- lexical-binding: t; -*-

(defvar mememacs/config-dir
  (expand-file-name "~/.emacs-mememacs.d/"))

(defun mememacs/find-init-file ()
  "Open current init file."
  (interactive)
  (find-file
   (expand-file-name
    (concat mememacs/config-dir "init.el"))))

(defun mememacs/copy-dir-name-name-as-kill-dwim ()
  ""
  (interactive)
  (kill-new
   (file-name-directory (expand-file-name default-directory))))

(defun mememacs/kill-buffer-name ()
  (interactive)
  (kill-new (buffer-name)))

(mememacs/leader-def
  "by" #'mememacs/kill-buffer-name
  "fe" #'mememacs/find-init-file
  "fD" #'mememacs/copy-dir-name-name-as-kill-dwim)


(defun mememacs/lispy-eval-line ()
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (special-lispy-eval)))

(defun mememacs/cancel-debugs ()
  (interactive)
  (cancel-debug-on-entry)
  (cancel-debug-on-variable-change)
  (untrace-all)
  (-some->>
      (get-buffer "*trace-output*")
    (kill-buffer-and-window))
  (message ""))

(defun mememacs/mkstr (obj)
  (with-output-to-string
    (prin1 obj)))

(defun mememacs/eval-and-set-test-fn (arg)
  "Eval and bind defun to leader-tt. With ARG ask for key "
  (interactive "P")
  (general-define-key
   :keymaps
   '(normal insert visual emacs)
   :prefix "SPC" :global-prefix "C-SPC"
   (concat
    "t"
    (if arg
	(read-from-minibuffer
	 "Test key bind: ")
      "t"))
   (eval-defun nil)))

(defun mememacs/eval-last-sexp-dwim (arg)
  "Eval last sexp.
If it is a quoted symbol, eval symbol value instead.
See `eval-last-sexp'."
  (interactive "P")
  (let ((s (sexp-at-point)))
    (if (eq 'quote (car-safe s))
	(with-temp-buffer
	  (insert (mememacs/mkstr (cadr s)))
	  (goto-char (point-max))
	  (eval-last-sexp arg))
      (eval-last-sexp arg))))

;; local eval
(general-def
  :states '(normal motion)
  "," nil
  ",e" '(:ignore t :which-key "eval")
  ",d" '(:ignore t :which-key "devel")
  ",dv" #'debug-on-variable-change
  ",dd" #'debug-on-entry
  ",dt" #'trace-function
  ",de" #'toggle-debug-on-error
  ",dq" #'toggle-debug-on-quit
  ",dx" #'mememacs/cancel-debugs)

(mememacs/comma-def
  :keymaps '(emacs-lisp-mode-map
	     lisp-interaction-mode-map)
  "e"
  `(,(let ((map (make-sparse-keymap "emacs-lisp")))
       (general-def
	 map
	 "l" #'mememacs/lispy-eval-line
	 "f" #'eval-defun
	 "b" #'eval-buffer
	 "d" #'edebug-defun
	 "e" #'mememacs/eval-last-sexp-dwim
	 "o" #'mememacs/eval-and-set-test-fn)
       map)
    :which-key "emacs lisp"))

(defun mememacs/switch-to-message-buffer ()
  ""
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun mememacs/ghetto-kill-and-open-buffer ()
  "Kill buffer and open again."
  (interactive)
  (when-let ((p (point))
	     (f (buffer-file-name)))
    (kill-this-buffer)
    (find-file f)
    (goto-char p)))

(mememacs/leader-def
  "bm" #'mememacs/switch-to-message-buffer
  "bR" #'mememacs/ghetto-kill-and-open-buffer
  "br" #'revert-buffer)



(defvar mememacs/escape-functions '())
(defun mememacs/escape ()
  "Run `mememacs/escape-functions'"
  (interactive)
  (run-hooks 'mememacs/escape-functions))

(general-def
  "S-<escape>"
  #'mememacs/escape)



(defun mm/toggle-when-unless ()
  (interactive)
  (skip-chars-backward "^(")
  (forward-char -1)
  (when-let* ((lst (sexp-at-point))
	      (lst
	       (cond
		((eq (car-safe lst) 'when)
		 (pop lst)
		 `(unless ,(cadar lst) ,@(cdr lst)))
		((eq (car-safe lst) 'unless)
		 (pop lst)
		 `(when (not ,(car lst)) ,@(cdr lst))))))
    (delete-region
     (point)
     (save-excursion
       (forward-list)))
    (insert (mememacs/mkstr lst))))


;; (defvar mememacs/lisp-map
;;   (make-sparse-keymap "lisp"))

;; (general-create-definer
;;   mememacs/lisp-def
;;   :keymaps '(normal insert visual emacs)
;;   :prefix ",k"
;;   :global-prefix "C-,k"
;;   mememacs/lisp-map)

(defun mememacs/jump-eshell ()
  (interactive)
  (let* ((dir default-directory)
	 (cd-shell
	  (lambda ()
            (goto-char (point-max))
	    (insert
	     (format "cd %s" (shell-quote-argument dir)))
	    (eshell-send-input))))
    (eshell)
    (funcall cd-shell)))

(mememacs/leader-def "je" #'mememacs/jump-eshell)

(defun mm/magit-kill-origin-url (&optional arg)
  (interactive "p")
  (kill-new
   (magit-git-string
    "remote"
    "get-url"
    (if arg
	(magit-read-remote "kill url from: ")
      "origin"))))

;; thanks john https://github.com/jwiegley/dot-emacs.git

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (when (memq current-mode '(emacs-lisp-mode))
      (funcall current-mode))))

(mememacs/leader-def "bs" #'scratch)

(defun mememacs/process-menu-switch-to-buffer ()
  (interactive)
  (-some->>
      (tabulated-list-get-id)
    (process-buffer)
    (switch-to-buffer)))

(general-def
  :states '(normal motion)
  :keymaps '(process-menu-mode-map)
  "b"
  #'mememacs/process-menu-switch-to-buffer)


(provide 'functions-1)
