;; Some of this code is from https://gitlab.com/ambrevar/dotfiles.
; see LICENCE for copying

(in-package :nyxt-user)


;; (load "~/.emacs-mememacs.d/straight/repos/slime/swank-loader.lisp")

;; (swank-loader:init)
;; *swank-port*

;; (define-command start-swank-2 (&optional (swank-port *swank-port*))
;;   "Start a Swank server that can be connected to, for instance, in
;; Emacs via SLIME.

;; Warning: This allows Nyxt to be controlled remotely, that is, to
;; execute arbitrary code with the privileges of the user running Nyxt.
;; Make sure you understand the security risks associated with this
;; before running this command."
;;   (swank:create-server :port swank-port :dont-close t)
;;   (echo "Swank server started at port ~a" swank-port))



(defmethod customize-instance ((buffer web-buffer) &key)
  (nyxt/vi-mode:vi-normal-mode :buffer buffer)
  (nyxt/style-mode:dark-mode :buffer buffer)
  ;; (push
  ;;  (make-instance
  ;;   'search-engine
  ;;   :shortcut "s"
  ;;   :search-url "https://paulgo.io/?q=~a"
  ;;   :fallback-url (quri:uri "https://paulgo.io/")
  ;;   :completion-function
  ;;   (make-search-completion-function
  ;;    :base-url "https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=~a"
  ;;    :processing-function
  ;;    #'identity
  ;;    ;; #'(lambda (results)
  ;;    ;; 	 (alex:when-let*
  ;;    ;; 	     ((results results)
  ;;    ;; 	      (results (decode-json results)))
  ;;    ;; 	   (mapcar
  ;;    ;; 	    #'list
  ;;    ;; 	    (second results)
  ;;    ;; 	    (fourth results))))
  ;;    ))
  ;;  (search-engines buffer))
  )





;; (define-configuration nyxt/blocker-mode:blocker-mode
;;   ((nyxt/blocker-mode:hostlists (append (list *my-blocked-hosts*) %slot-default%))))


;; (define-configuration buffer
;;   ((default-modes (append '(nyxt::vi-normal-mode) %slot-default%))))
;; (define-configuration prompt-buffer
;;   ((default-modes (append '(nyxt::vi-insert-mode) %slot-default%))))

;; (defvar *my-keymap* (make-keymap "my-map")
;;   "Keymap for `my-mode'.")

;; (define-mode my-mode ()
;;   "Dummy mode for the custom key bindings in `*my-keymap*'."
;;   ((keymap-scheme (keymap:make-scheme
;;                    scheme:emacs *my-keymap*
;;                    scheme:vi-normal *my-keymap*))))


;; (defun eval-in-emacs (&rest s-exps)
;;   "Evaluate S-exps with `emacsclient'."
;;   (let ((s-exps-string (cl-ppcre:regex-replace-all
;;                         ;; Discard the package prefix.
;;                         "next-user::?"
;;                         (write-to-string
;;                          `(progn ,@s-exps) :case :downcase)
;;                         "")))
;;     (log:debug "Sending to Emacs: ~s" s-exps-string)
;;     (ignore-errors (uiop:run-program
;;                     (list "emacsclient" "--eval" s-exps-string)))))


;; (define-key *my-keymap* ", e" 'trello-card-dispath)

;; (define-command org-capture (&optional (buffer (current-buffer)))
;;   "Org-capture current page."
;;   (eval-in-emacs
;;    `(org-link-set-parameters
;;      "next"
;;      :store (lambda ()
;;               (org-store-link-props
;;                :type "next"
;;                :link ,(url buffer)
;;                :description ,(title buffer))))
;;    `(org-capture)))

;; (define-key *my-keymap* ", o" 'org-capture)



;; (defun play-video-with-url (url)
;;   "Open mpv with url."
;;   (uiop:run-program (list "mpv" (render-url url))))

;; (define-command play-video-in-current-page (&optional (buffer (current-buffer)))
;;   "Play video in the currently open buffer."
;;   (play-video-with-url (url buffer)))

;; (define-key *my-keymap* ", v" 'play-video-in-current-page)



;; (defmethod %play-video ((a nyxt/dom:a-element))
;;   (when
;;       (current-buffer)
;;     (play-video-with-url
;;      (concatenate
;;       'string
;;       (url (current-buffer))
;;       (url a)))))


;; (define-command play-video-hint ()
;;   "Use the usual hints and open mpv with the result url."
;;   (nyxt/web-mode:query-hints
;;    "Play video"
;;    (lambda (result) (mapcar #'%play-video result)))
;;   :multi-selection-p t)

;; (defun start-markdown-mode-in-emacs ()
;;   (eval-in-emacs `(markdown-mode)))

;; (defparameter magnet-handler
;;   (url-dispatching-handler
;;    'transmission-magnet-links
;;    (match-scheme "magnet")
;;    (lambda (url)
;;      (uiop:launch-program
;;       (list "transmission-remote" "--add"
;;             (quri:render-uri url)))
;;      (echo "Magnet link opened in Transmission.")
;;      nil)))

;; (defvar *my-request-resource-handlers*
;;   (list
;;    magnet-handler))

;; (define-key
;;     *my-keymap*
;;   "space"
;;   (let ((map (make-keymap "benj-leader-map")))
;;     (define-key
;;         map
;;       "space" 'execute-command

;;       "b" 'switch-buffer

;;       "h h" 'help
;;       "h t" 'tutorial
;;       "h r" 'manual
;;       "h v" 'describe-variable
;;       "h f" 'describe-function
;;       "h c" 'describe-command
;;       "h C" 'describe-class
;;       "h s" 'describe-slot
;;       "h k" 'describe-key
;;       "h b" 'describe-bindings

;;       "b u" 'bookmark-url
;;       "b d" 'delete-bookmark)
;;     map)

;;   "d" 'delete-current-buffer
;;   "D" 'delete-buffer)

;; (define-configuration buffer
;;   ((override-map (let ((map (make-keymap "my-override-map")))
;;                    (define-key map
;;                      "C-e" 'edit-with-external-editor))
;;                 map)))


;; (define-configuration (buffer prompt-buffer)
;;   ((override-map (let ((map (make-keymap "my-override-map")))
;;                    (define-key map
;;                      "C-e" 'edit-with-external-editor
;;                      "C-g" 'nyxt/prompt-buffer-mode:cancel-input))
;;                  map)))





;; (define-configuration (buffer web-buffer nosave-buffer)
;;   ((default-modes (append '(;; dark-mode
;;                             nyxt/blocker-mode:blocker-mode
;;                             vi-normal-mode
;;                             my-mode)
;;                           %slot-default%))))

;; (nyxt::load-lisp "~/.config/nyxt/theme-minimal.lisp")
