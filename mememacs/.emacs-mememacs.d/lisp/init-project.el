;; -*- lexical-binding: t; -*-

(mememacs/leader-def
  "p"
  project-prefix-map
  "p."
  (defun mm/project-list-file ()
    (interactive)
    (find-file project-list-file))
  "pf" #'consult-project-extra-find
  "po" #'consult-project-extra-find-other-window)

(defun mememacs/fd-find-file ()
  (interactive)
  (find-file
   (completing-read
    "find file: "
    (let ((command "fd --hidden --exclude=.git --type=f . --print0"))
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
	   t)))))))

(general-def "H-\\" #'mememacs/fd-find-file)

(setq project-switch-commands
      '((magit-status "Magit" "M")
	(project-dired "Dired")
	(consult-project-extra-find "Find file" "F")
	(consult-ripgrep "Find regexp" "/")
	(consult-git-grep "Git grep" "g")
	(project-find-dir "Find directory")
	(mememacs/fd-find-file "Fd file" "\\")
	(project-eshell "Eshell")))

(provide 'init-project)
