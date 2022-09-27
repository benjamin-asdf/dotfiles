;; -*- lexical-binding: t; -*-
(general-def
  :states '(normal insert motion emacs)
  "C-." #'embark-act
  "C-;" #'embark-dwim)

(general-def
  :keymap vertico-map
  "C-." #'embark-act
  "C-;" #'embark-dwim)

(general-def :states '(normal motion emacs)
  "C-h B" #'embark-bindings)

(declare (describe-keymap 'vertico-map))

(mememacs/leader-def
  "hM" #'embark-bindings-in-keymap)

(defun mm/embark-eval-identifier-dwim (identifier)
  (let ((s (mm/identifier-unquote
	    identifier)))
    (cond ((memq
	    major-mode
	    lispy-clojure-modes)
	   (cider-interactive-eval
	    s
	    nil
	    (list
	     (- (point) (length identifier))
	     (point))
	    (cider--nrepl-pr-request-map)))
	  (t (lispy--eval s)))))

(general-def
  embark-identifier-map
  "e" #'mm/embark-eval-identifier-dwim)

(add-to-list
 'display-buffer-alist
 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
   nil
   (window-parameters (mode-line-format . none))))

(defmacro my/embark-ace-action (fn)
  `(defun
       ,(intern
	 (concat
	  "my/embark-ace-"
	  (symbol-name fn))) ()
     (interactive)
     (with-demoted-errors
	 "%s"
       (require 'ace-window)
       (let ((aw-dispatch-always t))
	 (aw-switch-to-window
	  (aw-select nil))
	 (call-interactively
	  (symbol-function ',fn))))))

(define-key embark-file-map (kbd "o")
   (my/embark-ace-action
    find-file))
(define-key embark-buffer-map (kbd "o")
   (my/embark-ace-action
    switch-to-buffer))
(define-key embark-bookmark-map (kbd "o")
   (my/embark-ace-action
    bookmark-jump))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))


(defun mememacs/dragon (file)
  (interactive "FDragon drag and drop: ")
  (start-process-shell-command
   "dragon"
   (get-buffer-create "*dragon*")
   (concat
    "dragon-drag-and-drop "
    (expand-file-name file)
    " "
    "--and-exit")))

(general-def
  'embark-file-map
  "S" #'sudo-find-file
  ">" #'mememacs/dragon)

(defun mememacs-find-file-dwim (&optional f)
  "Follow F.
F can be a program name, a file, or a file relative to the project root. "
  (interactive (list
		(read-shell-command "cmd: ")))
  (find-file
   (or (when (file-exists-p f) f)
       (let ((f (string-trim
		 (shell-command-to-string
		  (concat "which " f)))))
	 (when (file-exists-p f) f))
       (let ((f (expand-file-name
		 f
		 (project-root
		  (project-current)))))
	 (when (file-exists-p f) f))
       (let ((f (expand-file-name
		 (concat "resources/" f)
		 (project-root
		  (project-current)))))
	 (when (file-exists-p f) f))
       (user-error
	"%s is neither a file, nor anything I can follow"
	f))))

(general-def
  embark-general-map
  "f" #'mememacs-find-file-dwim)

(defun mememacs/embark-call-symbol (&optional symbol)
  "Insert a call to SYMBOl below the current toplevel form.
Meant to be added to `embark-identifier-map`"
  (interactive "s" (list (symbol-at-point)))
  (lispyville-end-of-defun)
  (forward-line 1)
  (insert
   (format "(%s)" symbol)))

(general-def embark-identifier-map
  "l" #'mememacs/embark-call-symbol)

(general-def embark-variable-map
  "t" #'debug-on-variable-change
  "T" #'cancel-debug-on-variable-change)

(defun ensure-list (e) (if (listp e) e `(,e)))
(defun mm/embark-kill-displayed (strings)
  ""
  (embark-copy-as-kill
   (mapcar
    #'s-trim
    (mapcar
     #'vertico--display-string
     (ensure-list strings)))))

(general-def embark-general-map "C-k" #'mm/embark-kill-displayed)

(embark-define-keymap mm/embark-consult-grep-map
  "For consult grep"
  :parent embark-general-map
  ("w" mm/kill-consult-grep-dwim)
  ("k" #'embark-copy-as-kill))

(defun mm/kill-consult-grep-dwim (s) (kill-new (replace-regexp-in-string ".+?\s+\\(.*\\)" "\\1" s)))

(add-to-list 'embark-keymap-alist '(consult-grep mm/embark-consult-grep-map))

(general-def embark-identifier-map "m" #'lispyville-wrap-lispy-mark-symbol-special)

(defun mm/kill-file-name-relative-to-project (file)
  (interactive "ffile: ")
  (kill-new
   (file-relative-name
    file
    (project-root
     (project-current)))))

(general-def embark-file-map "r" #'mm/kill-file-name-relative-to-project)

(provide 'init-embark)
