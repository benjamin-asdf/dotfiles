(setf org-roam-directory "~/org/roam")


(mememacs/comma-def
  "o" '(:ignore t :which-key "org")
  "or" '(:ignore t :which-key "roam")
  "orr" #'org-roam-node-find)

(provide 'init-org-roam)
