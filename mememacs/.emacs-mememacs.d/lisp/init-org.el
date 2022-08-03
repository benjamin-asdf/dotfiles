(mememacs/local-def
    :keymaps '(org-mode-map)
    :states '(normal visual motion)
    "t" #'org-todo)

(add-hook
   'org-mode-hook
   (lambda ()
     (setq-local
      electric-pair-inhibit-predicate
      `(lambda
	 (c)
	 (if (char-equal c ?<)
	     t
	   (,electric-pair-inhibit-predicate
	    c))))))

(general-def
  'org-mode-map
  "M-P" #'org-shiftmetaup
  "M-N" #'org-shiftmetadown)

(general-def
  'org-mode-map
  "H-<return>" #'org-edit-src-exit)

(mememacs/local-def
  'org-mode-map
  "h" #'org-shiftmetaleft
  "l" #'org-shiftmetaright
  "e" #'org-edit-src-code)

(mememacs/comma-def
  "ol" #'org-store-link)


(provide 'init-org)
