;; -*- lexical-binding: t; -*-

(require 'dash)

(defvar stumpwm-init-file "~/.stumpwm.d/init.lisp")

(defun mm/stumpwm-eval (form)
  (unless (file-exists-p stumpwm-init-file)
    (user-error "you need %s and slime"
		'stumpwm-init-file))
  (require 'le-lisp)
  (with-current-buffer
     (find-file-noselect stumpwm-init-file)
    (lispy--eval-lisp
     (format "%s" (princ form)))))

(defun mm/stumpwm-windows ()
  (car
   (read-from-string
    (mm/stumpwm-eval
     '(mapcar
       (lambda (w)
	 `(:id ,(window-id w) :title ,(window-title w) :class ,(window-class w)))
       (group-windows (current-group)))))))

(defun mm/stumpwm-window-display->id (s)
  (plist-get (car (read-from-string s)) :ID))

(defun mm/stumpwm-window-display (lst)
  (format "%s" (princ lst)))

(defalias
  'mm/stumpwm-kill-window
  (-compose
   (lambda (w-id) (mm/stumpwm-eval `(kill-windows (list (window-by-id ,w-id)))))
   #'mm/stumpwm-window-display->id))

(defalias
  'mm/stumpwm-open-window
  (-compose
   (lambda (w-id) (mm/stumpwm-eval `(pull-window (window-by-id ,w-id))))
   #'mm/stumpwm-window-display->id))

(defun mm/consult-stumpwm-windows ()
  (interactive)
  (mm/stumpwm-open-window
   (consult--read
    (mapcar
     #'mm/stumpwm-window-display
     (mm/stumpwm-windows))
    :prompt "stumpwm window: "
    :category 'stumpwm-window)))

(add-to-list
 'embark-keymap-alist '(stumpwm-window mm-embark-stumpwm-window-map))

(provide 'mememacs-stumpwm)
