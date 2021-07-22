;; Some of this code is from https://gitlab.com/ambrevar/dotfiles.
;; see LICENCE for copying

(in-package :nyxt-user)


(define-configuration buffer
  ((default-modes (append '(nyxt::vi-normal-mode) %slot-default%))))
(define-configuration prompt-buffer
  ((default-modes (append '(nyxt::vi-insert-mode) %slot-default%))))

(defvar *my-keymap* (make-keymap "my-map")
  "Keymap for `my-mode'.")

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))


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

(define-command trello-card-dispath (&optional (buffer (current-buffer)))
  "Call trello card dispatch on emacsclient.
See documentation of `team-trello' package in emacs."
  (eval-in-emacs
   `(team-trello-card-dispatch
     ,(url buffer))))

(define-key *my-keymap* ", e" 'trello-card-dispath)

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

(define-key *my-keymap* ", o" 'org-capture)



(defun play-video-with-url (url)
  "Open mpv with url."
  (uiop:run-program (list "mpv" (render-url url))))

(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (play-video-with-url (url buffer)))

(define-key *my-keymap* ", v" 'play-video-in-current-page)



(defmethod %play-video ((a nyxt/dom:a-element))
  (when
      (current-buffer)
    (play-video-with-url
     (concatenate
      'string
      (url (current-buffer))
      (url a)))))


(define-command play-video-hint ()
  "Use the usual hints and open mpv with the result url."
  (nyxt/web-mode:query-hints
   "Play video"
   (lambda (result) (mapcar #'%play-video result)))
  :multi-selection-p t)

(defun start-markdown-mode-in-emacs ()
  "Make emacs eval command to start markdown mode."
  (eval-in-emacs `(markdown-mode)))

(define-key
    *my-keymap*
  "space"
  (let ((map (make-keymap "benj-leader-map")))
    (define-key
        map
      "space" 'execute-command

      "b" 'switch-buffer

      "h h" 'help
      "h t" 'tutorial
      "h r" 'manual
      "h v" 'describe-variable
      "h f" 'describe-function
      "h c" 'describe-command
      "h C" 'describe-class
      "h s" 'describe-slot
      "h k" 'describe-key
      "h b" 'describe-bindings

      "b u" 'bookmark-url
      "b d" 'delete-bookmark)
    map)

  "d" 'delete-current-buffer
  "D" 'delete-buffer)

(define-configuration buffer
  ((override-map (let ((map (make-keymap "my-override-map")))
                   (define-key map
                     "C-e" 'edit-with-external-editor))
                map)))


(define-configuration (buffer prompt-buffer)
  ((override-map (let ((map (make-keymap "my-override-map")))
                   (define-key map
                     "C-e" 'edit-with-external-editor
                     "C-g" 'nyxt/prompt-buffer-mode:cancel-input))
                 map)))



(define-configuration (buffer web-buffer nosave-buffer)
  ((default-modes (append '(;; dark-mode
                            nyxt/blocker-mode:blocker-mode
                            vi-normal-mode
                            my-mode)
                          %slot-default%))))

(nyxt::load-lisp "~/.config/nyxt/theme-minimal.lisp")


