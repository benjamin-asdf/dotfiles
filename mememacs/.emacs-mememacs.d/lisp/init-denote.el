;; -*- lexical-binding: t; -*-

(setf denote-file-type 'org
      denote-infer-keywords t
      denote-dired-rename-expert t
      denote-known-keywords
      '("mind"
	"code"
	"flow"
	"world"
	"emacs"
	"clojure"
	"science"
	"journal"
	"biology")
      denote-directory "~/notes/")

(with-eval-after-load
    'org-capture
  (add-to-list
   'org-capture-templates
   '("n" "Denote new"
     plain
     (file denote-last-path)
     #'denote-org-capture
     :no-save t
     :immediate-finish nil
     :kill-buffer t
     :jump-to-captured t)))

(defun mm/denote-journal ()
  "Create an entry tagged 'journal' with the date as its title."
  (interactive)
  (denote (format-time-string "%Y-%m-%d") '("journal")))

(defun mm/denote-current-journal ()
  (car
   (let ((today (format-time-string "%Y-%m-%d__journal")))
     (cl-remove-if-not
      (lambda (f) (string-match-p today f))
      (directory-files
       denote-directory
       t)))))

(mememacs/comma-def
  "oo" (defun mm/denote-dir () (interactive) (dired-jump nil denote-last-path))
  "oJ" #'mm/denote-journal
  "oj" (defun mm/find-today-journal ()
	 (interactive)
	 (if-let
	     ((f (mm/denote-current-journal)))
	     (find-file (expand-file-name f))
	   (mm/denote-journal)))
  "od" #'denote
  "oT" (defun mm/denote-todo () (interactive) (denote "todo"))
  "oc" #'org-capture
  "of" (defun mm/consult-file-notes ()
	 (interactive)
	 (let ((default-directory denote-directory))
	   (call-interactively #'consult-project-buffer))))

(provide 'init-denote)
