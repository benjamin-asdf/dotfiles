;; Some of this code is from https://gitlab.com/ambrevar/dotfiles.
;; see LICENCE for copying

(in-package :nyxt-user)

(define-configuration buffer
  ((default-modes (append '(nyxt::vi-normal-mode) %slot-default%))))
(define-configuration prompt-buffer
  ((default-modes (append '(nyxt::vi-insert-mode) %slot-default%))))


(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'."
  (let ((s-exps-string (cl-ppcre:regex-replace-all
                        ;; Discard the package prefix.
                        "next-user::?"
                        (write-to-string
                         `(progn ,@s-exps) :case :downcase)
                        "")))
    (log:debug "Sending to Emacs: ~s" s-exps-string)
    (ignore-errors (uiop:run-program
                    (list "emacsclient" "--eval" s-exps-string)))))

(define-command trello-card-dispath ()
  "Call trello card dispatch on emacsclient.
See documentation of `team-trello' package in emacs."
  (eval-in-emacs
   `(team-trello-card-dispatch
     ,(url buffer))))

(define-command org-capture (&optional (buffer (current-buffer)))
  "Org-capture current page."
  (eval-in-emacs
   `(org-link-set-parameters
     "next"
     :store (lambda ()
              (org-store-link-props
               :type "next"
               :link ,(url buffer)
               :description ,(title buffer))))
   `(org-capture)))

(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (uiop:run-program (list "mpv" (render-url (url buffer)))))

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))

(define-configuration buffer
  ((override-map (let ((map (make-keymap "my-override-map")))
                   (define-key map
                     "space space" 'execute-command
                     ", c" 'org-capture
                     ", e" 'trello-card-dispath
                     ", v" 'play-video-in-current-page

                     "space h h" 'help
                     "space h t" 'tutorial
                     "space h r" 'manual
                     "space h v" 'describe-variable
                     "space h f" 'describe-function
                     "space h c" 'describe-command
                     "space h C" 'describe-class
                     "space h s" 'describe-slot
                     "space h k" 'describe-key
                     "space h b" 'describe-bindings

                     "D" 'delete-buffer
                     "d" 'delete-current-buffer


                     "space b u" 'bookmark-url
                     "space b d" 'delete-bookmark
                     ;;  trello dispatch

                     )
                   )
                 map)))

(nyxt::load-lisp "~/.config/nyxt/theme-minimal.lisp")
