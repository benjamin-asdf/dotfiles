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

(defun mm/scratch-denote (arg)
  (interactive "P")
  (let ((default-directory denote-directory))
    (if arg (denote nil '("scratch"))
      (find-file
       (car
        (cl-remove-if-not
         (lambda (it)
	   (and (string-match-p "scratch" it)
	        (not (string-match-p "#" it))))
         (process-lines "ls" "-A" "-t")))))))

(defvar mm/org-dispatch-map
  (let ((m (make-sparse-keymap "mm org dispatch")))
    (define-key m (kbd "o") (defun mm/denote-dir () (interactive) (dired-jump nil denote-last-path)))
    (define-key m (kbd "j") #'mm/scratch-denote)
    (define-key m (kbd "d") #'denote)
    (define-key m (kbd "T") (defun mm/denote-todo () (interactive) (denote "todo")))
    (define-key m (kbd "c") #'org-capture)
    (define-key m (kbd "f") (defun mm/consult-file-notes ()
			      (interactive)
			      (let ((default-directory denote-directory))
				(call-interactively #'consult-project-buffer))))
    (define-key m (kbd "c") #'org-capture)
    (define-key m (kbd "l") #'org-store-link)
    (define-key m (kbd "RET") #'denote-rename-file)
    (define-key m (kbd "g")
      (defun mm/consult-ripgrep-denote-titles-and-filetags ()
        (interactive)
        (consult-ripgrep denote-directory "\\(\\(+title:\\)\\|\\(+filetags:\\)\\) ")))
    m))

(define-key org-mode-map (kbd "C-c t") #'org-todo)
;; Accidentally hit org-goto, then I was confused about the isearch fn being overriden
;; during the rec edit
(define-key org-mode-map (kbd "C-c C-j") nil)
(define-key org-mode-map (kbd "C-c E") #'org-edit-src-code)





(add-to-list 'org-babel-load-languages '(clojure . t))
(add-to-list 'org-babel-load-languages '(shell . t))

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(setf org-cycle-global-at-bob t)

(add-hook 'org-mode-hook #'ambrevar/turn-on-delete-trailing-whitespace)

(provide 'init-denote)
