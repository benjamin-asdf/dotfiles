(in-package #:nyxt-user)

(load "~/.emacs-mememacs.d/straight/repos/slime/swank-loader.lisp")
(swank-loader:init)
(swank:create-server
 :port 4006
 :style swank:*communication-style*
 :dont-close t)

(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

(define-nyxt-user-system-and-load "nyxt-user/search-engines"
  :depends-on (:nx-search-engines) :components ("search-engines.lisp"))

(define-configuration browser
  ;; This is for Nyxt to never prompt me about restoring the previous session.
  ((restore-session-on-startup-p nil)
   (external-editor-program
    (list "emacsclient" "-cn" "-a" "" "-F"
          "((vertical-scroll-bars)(tool-bar-lines) (menu-bar-lines))"))))

;; thanks
;; https://github.com/Gavinok/dotnyxt/blob/master/init.lisp

(define-command youtube-play-current-page ()
  "Watch a Youtube video with mpv"
  (uiop:run-program
   (list "mpv" (render-url (url (current-buffer))))))

(define-configuration (web-buffer)
  ((default-modes
    (append
     '(nyxt/vi-mode:vi-normal-mode)
     %slot-value%))))

(define-configuration (input-buffer)
  ((keyscheme
    nyxt/keyscheme:emacs)))

(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "black"
         :color "white")))))))

(defmacro alter-keyscheme (keyscheme scheme-name &body bindings)
  `(nkeymaps/core:define-keyscheme-map "custom" (list :import ,keyscheme)
     ,scheme-name
     (list ,@bindings)))

(defun benj/keybinds (map)
  (alter-keyscheme
      map
      nyxt/keyscheme:vi-normal
    "M-x" 'execute-command
    "shift-space" 'toggle-mark-backwards
    "; y" 'nyxt/hint-mode:copy-hint-url))

(define-command duplicate-buffer (&key parent-buffer)
  "Duplicate current buffer in a new buffer."
  (nyxt::duplicate-buffer-with-current-modes :modes
				       '(nyxt/document-mode:document-mode
					 nyxt:base-mode)
                                       :parent-buffer parent-buffer))

(define-configuration
    (nyxt/document-mode:document-mode)
  ((keyscheme-map
    (benj/keybinds %slot-value%))))

;; put stuff into kill ring .. ?

;; I want to bind shift-space to the mark thing instead of scroll up






;; (find-class 'nyxt/hint-mode:hint-mode)
;; prompt-mode in emacs mode

;; (default-modes (current-buffer))
;; (mode (current-buffer))
;; (describe-class :class 'prompt-buffer)

;; (define-command-global
;;     say-hi
;;     ()
;;   "say hi"
;;   (echo
;;    (prompt
;;     :prompt "Test prompt"
;;     :sources (make-instance
;; 	      'prompter:source
;; 	      :name "Test"
;; 	      :constructor '("foo" "bar")))))
