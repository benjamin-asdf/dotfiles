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

(defvar mm/org-dispatch-map
  (let ((m (make-sparse-keymap "mm org dispatch")))
    (define-key m (kbd "o") (defun mm/denote-dir () (interactive) (dired-jump nil denote-last-path)))
    (define-key m (kbd "J") #'mm/denote-journal)0
    (define-key m (kbd "j") (defun mm/find-today-journal ()
			      (interactive)
			      (if-let
				  ((f (mm/denote-current-journal)))
				  (find-file (expand-file-name f))
				(mm/denote-journal))))
    (define-key m (kbd "d") #'denote)
    (define-key m (kbd "T") (defun mm/denote-todo () (interactive) (denote "todo")))
    (define-key m (kbd "c") #'org-capture)
    (define-key m (kbd "f") (defun mm/consult-file-notes ()
			      (interactive)
			      (let ((default-directory denote-directory))
				(call-interactively #'consult-project-buffer))))
    (define-key m (kbd "c") #'org-capture)
    (define-key m (kbd "l") #'org-store-link)
    m))

(meow-leader-define-key (cons "o" mm/org-dispatch-map))

(define-key org-mode-map (kbd "C-c t") #'org-todo)


(advice-add
 'org-babel-execute-src-block
 :before
 (defun mm/load-ob-implementations (&rest _)
   (require 'ob-clojure)
   (require 'ob-shell)))

(provide 'init-denote)
