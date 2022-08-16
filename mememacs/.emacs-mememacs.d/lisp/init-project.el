;; -*- lexical-binding: t; -*-

(mememacs/leader-def
  "p"
  project-prefix-map
  "p."
  (defun mm/project-list-file ()
    (interactive)
    (find-file project-list-file))
  "pf" #'consult-project-buffer)

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
   (completing-read
    "find file: "
    (mememacs/fd-files))))

(general-def "C-\\" #'mememacs/fd-find-file)

(setq project-switch-commands
      '((magit-status "Magit" "M")
	(project-dired "Dired")
	(project-find-file "Find file" "f")
	(consult-ripgrep "Find regexp" "/")
	(consult-git-grep "Git grep" "g")
	(project-find-dir "Find directory")
	(mememacs/fd-find-file "Fd file" "\\")
	(project-eshell "Eshell")))

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
