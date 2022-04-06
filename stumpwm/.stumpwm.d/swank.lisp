;; swank.lisp -*- Mode: Lisp -*-
(in-package :stumpwm)
;;
;; Load swank
;; (ql:quickload :swank)

(load "~/.emacs-mememacs.d/straight/repos/slime/swank-loader.lisp")

(swank-loader:init)

(let ((server-running nil)
      (first-time t))
  ;;
  (defcommand swank () ()
    "Toggle the swank server on/off."
    (if server-running
        (progn
          (swank:stop-server 4005)
          (echo-string
           (current-screen)
           "Stopping swank.")
          (setf server-running nil))
        (progn
          (swank:create-server :port 4005
                               :style swank:*communication-style*
                               :dont-close t)
	  (if first-time
              (echo-string
               (current-screen)
               "Re-starting swank.")
	      (setf first-time nil))
          (setf server-running t)))))
;; Now call the re-defined command
(swank)
