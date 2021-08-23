(require 'cl)
(message "")

(unless (file-exists-p "~/.org-jira")
  (make-directory "~/.org-jira"))

(jiralib-login
 "benjamin.schwerdtner@gmail.com"
 (auth-source-pick-first-password
  :host "jira-api-token"))


(defun my/org-jira-read-next-action (&rest args)
  (ignore args)
  (helm
   (helm-build-sync-source
       "jira action"
     :candidates
     '(("Backlog" . "11")
       ("TODO" . "21")
       ("In Progress" . "31")
       ("FINISHED" . "41")
       ("INBOX" ."51" )))))

(defalias 'org-jira-progress-next-action 'my/org-jira-read-next-action)


(setq org-jira-custom-jqls
      '((:jql " project IN (COS) AND status = INBOX AND resolution = Unresolved AND assignee in (currentUser()) order by updated DESC" :filename "inbox")
	(:jql "assignee = currentUser() AND resolution = Unresolved AND status != Backlog order by updated DESC" :filename "no-backlog")))



(provide 'init-org-jira)
