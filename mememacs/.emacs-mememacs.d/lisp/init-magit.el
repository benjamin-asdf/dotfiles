;; This file is meant to be loadable without dependencies.
;; This allows external programs to easily access the list of repository directories.

(setq magit-repository-directories '(("~/.password-store")
                                     ("~/common-lisp" . 1)
                                     ("~/projects" . 1)
                                     ("~/.local/share/emacs/site-lisp" . 1)))

(provide 'init-magit)
