;;; Functions-1

;;; -*- lexical-binding: t; -*-

(defvar mememacs/config-dir
  (expand-file-name "~/.emacs-mememacs.d/"))

(defun mememacs/native-compile-config ()
  (interactive)
  (native-compile-async (expand-file-name "lisp" mememacs/config-dir)))

(defun mememacs/find-init-file ()
  "Open current init file."
  (interactive)
  (find-file
   (expand-file-name
    (concat mememacs/config-dir "init.el"))))


(defun mememacs/kill-buffer-name ()
  (interactive)
  (kill-new (buffer-name)))

(mememacs/leader-def
  "by" #'mememacs/kill-buffer-name)

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
  (-some-->
      (get-buffer "*trace-output*")
    (with-current-buffer
	it
	(kill-buffer-and-window)))
  (message ""))

(defun mememacs/mkstr (obj)
  (with-output-to-string
    (print obj)))

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

(general-def
  :states '(normal motion)
  "," nil
  ",e" '(:ignore t :which-key "eval")
  ",d" '(:ignore t :which-key "devel")
  ",dv" #'debug-on-variable-change
  ",dd" #'debug-on-entry
  ",dr" #'trace-function
  ",dt" #'toggle-debug-on-error
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
	 "d" #'eval-defun
	 "b" #'eval-buffer
	 "D" #'edebug-defun
	 "e" #'mememacs/eval-last-sexp-dwim)
       map)
    :which-key "emacs lisp"))

(defun mememacs/ghetto-kill-and-open-buffer ()
  "Kill buffer and open again."
  (interactive)
  (when-let ((p (point))
	     (f (buffer-file-name)))
    (kill-this-buffer)
    (find-file f)
    (goto-char p)))

(mememacs/leader-def
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

(with-eval-after-load 'iedit
  (add-hook
   'mememacs/escape-functions
   (defun mm/iedit-quit-maybe ()
     (when iedit-lib-quit-func
       (iedit--quit)))))

(add-hook 'mememacs/escape-functions #'widen)



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

(mememacs/leader-def "jE" #'mememacs/jump-eshell)

(defun mememacs/eshell-hist ()
  (interactive)
  (goto-char (point-max))
  (insert
   (completing-read
    "hist: "
    (ring-elements
     eshell-history-ring))))

(mememacs/local-def
  :states '(insert normal)
  :keymaps '(eshell-mode-map)
  "h" #'mememacs/eshell-hist)

(defun mememacs/magit-kill-origin-url (&optional arg)
  (interactive "p")
  (-->
   (magit-git-string
    "remote"
    "get-url"
    (if arg
	(magit-read-remote "kill url from: ")
      "origin"))
   (progn
     (message  "Kill %s" it)
     (kill-new it))))

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


(defun mememacs/create-script* (file bang setup)
  (find-file file)
  (insert bang)
  (save-buffer)
  (evil-insert-state)
  (set-file-modes file #o777)
  (funcall setup))

(defun mememacs/create-script (file)
  (interactive "Fnew script: ")
  (mememacs/create-script*
   file
   "#!/bin/sh\n"
   #'shell-script-mode))

(defun mememacs/create-bb-script (file)
  (interactive "Fnew bb: ")
  (mememacs/create-script*
   file
   "#!/usr/bin/env bb\n"
   #'clojure-mode))

(mememacs/comma-def
  :keymaps 'dired-mode-map
  "ns" #'mememacs/create-script
  "nS" #'mememacs/create-bb-script)


(defun mememacs/toggle-debug-on-quit (arg)
  (interactive "P")
  (if arg
    (setf ebug-on-quit
	  (not ebug-on-quit))
    (setf debug-on-quit
	  (not debug-on-quit))))

(general-def "C-x C-q" #'mememacs/toggle-debug-on-quit)

(defun mememacs/copy-file-name-dwim (arg)
  (interactive "P")
  (-->
   (cond ((eq major-mode 'dired-mode)
	  (dired-copy-filename-as-kill
	   (when arg 0))
	  (pop kill-ring))
	 ((or arg (in major-mode 'eshell-mode))
	  default-directory)
	 (t
	  (or
	   buffer-file-name
	   (progn
	     (kill-new
	      (buffer-name))
	     (user-error
	      "Killed %s instead of file name" (buffer-name))))))
   (if arg
       (file-name-directory it)
     it)
   (progn (kill-new it)
	  (message "Copied %s" it))))

(mememacs/comma-def
  :states 'normal
  "fy" #'mememacs/copy-file-name-dwim)

(defhydra hydra-buffer ()
  "buffer"
  ("d" #'kill-current-buffer)
  ("k" #'previous-buffer)
  ("j" #'previous-buffer)
  ("b" #'consult-buffer :exit t)
  ("p" #'projectile-find-dir :exit t)
  ("P" #'projectile-find-dir-other-window :exit t)
  ("s" #'helm-do-ag-buffers)
  ("a" #'mark-whole-buffer)
  ("y" #'mememacs/kill-buffer-name :exit t))

(mememacs/comma-def
  :states '(normal motion)
  "b" #'hydra-buffer/body
  "w" #'evil-window-map)

(defun mm/kill-whole-buffer ()
  (interactive)
  (kill-new (buffer-substring-no-properties (point-min) (point-max)))
  (message "killed whole buffer contents"))

(defhydra outline-hydra ()
  ("c" #'counsel-outline :exit t)
  ("J" #'outline-forward-same-level)
  ("K" #'outline-backward-same-level)
  ("L" #'outline-demote)
  ("H" #'outline-promote)
  ("M-j" #'outline-move-subtree-down)
  ("M-k" #'outline-move-subtree-up)
  ("g" #'outline-back-to-heading)
  ("i" #'outline-cycle)
  ("m" #'outline-hide-other)
  ("o" #'outline-show-all))

(defhydra scroll-hydra
  (:pre (set-cursor-color "Red") :post (set-cursor-color "White"))
  "scroll"
  ("j" (evil-scroll-down 20) "down")
  ("k" (evil-scroll-up 20) "up")
  ("J" (evil-scroll-down 150))
  ("K" (evil-scroll-up 150))
  ("h" #'evil-window-top)
  ("l" #'evil-window-bottom)
  ("z" #'evil-scroll-line-to-center)
  ("H" #'evil-scroll-line-to-top)
  ("L" #'evil-scroll-line-to-bottom)
  ("g" #'evil-goto-first-line)
  ("G" #'evil-goto-line)
  ("a" #'mark-whole-buffer)
  ("o" #'outline-hydra/body "outline" :exit t)
  ("y" #'mm/kill-whole-buffer "kill-whole" :exit t))

(mememacs/comma-def
  "jo" #'outline-hydra/body
  "jj" #'scroll-hydra/body
  "jk" #'scroll-hydra/lambda-k
  "jJ" #'scroll-hydra/lambda-J
  "jK" #'scroll-hydra/lambda-K)

(mememacs/comma-def
  "fr" (defun mm/find-in-repos ()
	 (interactive)
	 (let ((default-directory "~/repos/"))
	   (call-interactively #'find-file)))

  "f" '(:ignore t :which-key "f..")
  "fs" #'save-buffer
  "ff" #'consult-find

  "fh" (defun dired-jump-home ()
	 (interactive)
	 (dired-goto-file "~/")
	 (let ((default-directory "~/")
	       (buffer-file-name nil))
	   (dired-jump)))

  "," (defun call-C-c-C-c ()
	(interactive)
	(call-interactively (key-binding (kbd "C-c C-c"))))

  "k" (defun call-C-c-C-k ()
	(interactive)
	(call-interactively (key-binding (kbd "C-c C-k"))))

  "x"  `(,(key-binding (kbd "C-x")) :which-key "C-x")

  "c" `(,(key-binding (kbd "C-c") :which-key "C-c")))

(defun mememacs/kill-dangling-buffs (&rest args)
  "Kill all buffers that are connected to a file,
where the file does not exist."
  (interactive)
  (let ((bfs (cl-loop for b in (buffer-list)
		      for f = (buffer-file-name b)
		      when (and f (not (file-exists-p f)))
		      collect b)))
    (when bfs
      (message
       "Kill %d buffers"
       (length bfs)))
    (mapc #'kill-buffer bfs)))

(dolist (fn '(dired-internal-do-deletions))
  (advice-add fn :after #'mememacs/kill-dangling-buffs))

(defun mememacs/kill-shell-command ()
  (interactive)
  (kill-new
   (with-temp-buffer
     (shell-command
      (read-shell-command
       "kill cmd: ")
      (current-buffer))
     (buffer-string))))

(general-def
  "C-x k" (defun mememacs/kill-minibuff-contents ()
	    (interactive)
	    (kill-new (minibuffer-contents))
	    (keyboard-quit)))

(general-def
  :keymaps '(emacs-lisp-mode-map)
  "C-c C-k" #'eval-buffer
  "C-c C-c" #'eval-defun)

(general-def
  :keympas '(compilation-mode-map)
  "M-<return>"
  (defun mm/send-y ()
    (interactive)
    (when-let
	((p
	  (get-buffer-process
	   (current-buffer))))
      (process-send-string p "y\n"))))

(defun mm/force-clear-buff ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(provide 'functions-1)
