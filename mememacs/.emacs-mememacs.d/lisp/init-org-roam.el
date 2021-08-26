(setf org-roam-directory "~/org/roam")


(mememacs/comma-def
  "or" '(:ignore t :which-key "roam")
  "orr" #'org-roam-node-find)

(provide 'init-org-roam)
