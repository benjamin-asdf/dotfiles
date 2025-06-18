;; -*- lexical-binding: t; -*-
(add-hook
   'org-mode-hook
   (defun mm/org-set-electric-pair ()
       (setq-local
	electric-pair-inhibit-predicate
	`(lambda
	   (c)
	   (if (char-equal c ?<)
	       t
	     (,electric-pair-inhibit-predicate
	      c))))))

(bind-keys
 :map org-mode-map
 ("M-P" . org-shiftmetaup)
 ("M-N" . org-shiftmetadown)
 ("C-c C-t"  . nil))

(advice-add
 #'org-babel-execute-src-block-maybe
 :before
 (lambda (&rest _)
   (require 'ob-clojure)))

(with-eval-after-load 'ob-clojure
  (setf org-babel-clojure-backend 'cider
	org-confirm-babel-evaluate nil))

(setf org-export-with-toc nil)

(require 'org-protocol)

(provide 'init-org)
