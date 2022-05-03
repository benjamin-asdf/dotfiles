;; -*- lexical-binding: t; -*-

(mememacs/leader-def
  "p"
  project-prefix-map
  "p."
  (defun mm/project-list-file ()
    (interactive)
    (find-file project-list-file)))

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

(add-to-list
 'project-switch-commands
 '(project-dired "Dired"))

(provide 'init-project)
