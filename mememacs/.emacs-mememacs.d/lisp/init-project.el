;; -*- lexical-binding: t; -*-

(setf project-switch-use-entire-map t)

(defvar mm/project-command nil)

(advice-add
 #'project--switch-project-command
 :override
 (defun mm/project-command ()
   (or
    mm/project-command
    (let ((completion-styles '(basic)))
      (embark-completing-read-prompter
       project-prefix-map
       nil
       'no-default)))))

(mememacs/leader-def
  "p"
  project-prefix-map
  "pP" (defun mm/project-switch-project-find-file ()
	 (interactive)
	 (let ((mm/project-command #'consult-project-buffer))
	   (call-interactively #'project-switch-project)))
  "p."
  (defun mm/project-list-file ()
    (interactive)
    (find-file project-list-file))
  "pf" #'consult-project-buffer
  "pm" #'magit-status
  "p/" #'consult-ripgrep
  "pG" #'consult-git-grep
  "p\\" #'mememacs/fd-find-file)

(defun mm/cmd->lines (command)
  (with-temp-buffer
    (shell-command
     command
     t
     "*project-files-errors*")
    (let ((shell-output (buffer-substring
			 (point-min)
			 (point-max))))
      (split-string
       (string-trim shell-output)
       "\0"
       t))))

(defun mememacs/fd-files ()
  (mm/cmd->lines "fd --hidden --exclude=.git --type=f . --print0"))

(defun mememacs/git-ls-files ()
  (mm/cmd->lines "git ls-files -z"))

(defun mememacs/fd-find-file ()
  (interactive)
  (find-file
   (consult--read
    (mememacs/fd-files)
    :category 'file
    :prompt "find file: "
    :state (consult--file-preview)
    :history 'file-name-history)))

(general-def "C-\\" #'mememacs/fd-find-file)

(defvar mm/consult-fd-project-files
  `(:name "Project Files fd"
	  :narrow (?d . "fd")
	  :hidden t
	  :category file
	  :face consult-file
	  :history file-name-history
	  :state ,#'consult--file-state
	  :new ,(lambda (file)
		  (consult--file-action
		   (expand-file-name
		    file
		    (consult--project-root))))
	  :enabled ,(lambda ()
		      (and consult-project-function
			   recentf-mode))
	  :items ,(lambda () (mememacs/fd-files))))


(defvar mm/consult-git-ls-files
  `(:name "Project Files git ls files"
	  :narrow (?g . "git-ls-files")
	  :hidden t
	  :category file
	  :face consult-file
	  :history file-name-history
	  :state ,#'consult--file-state
	  :new ,(lambda (file)
		  (consult--file-action
		   (expand-file-name
		    file
		    (consult--project-root))))
	  :enabled ,(lambda ()
		      (and consult-project-function
			   recentf-mode))
	  :items ,(lambda () (mememacs/git-ls-files))))

(setq consult-project-buffer-sources
      (list `(:hidden nil :narrow ?b ,@consult--source-project-buffer)
	    `(:hidden nil :narrow ?f ,@consult--source-project-recent-file)
	    `(:hidden nil :narrow ?d ,@mm/consult-fd-project-files)
	    `(:hidden nil :narrow ?g ,@mm/consult-git-ls-files)))

(provide 'init-project)
