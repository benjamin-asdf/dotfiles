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

(add-to-list
 'display-buffer-alist
 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
   nil
   (window-parameters (mode-line-format . none))))

(eval-when-compile
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
	    (symbol-function ',fn)))))))

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


(general-def 'embark-file-map "S" #'sudo-find-file)

(defun mememacs-follow-shell-cmd (&optional cmd)
  "Follow CMD.
If CMD is a symlink follow it."
  (interactive
   (list
    (read-shell-command
     "cmd: ")))
  (find-file
   (string-trim
    (shell-command-to-string
     (concat "which " cmd)))))

(general-def
  embark-general-map
  "f" #'mememacs-follow-shell-cmd)

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



(provide 'init-embark)
