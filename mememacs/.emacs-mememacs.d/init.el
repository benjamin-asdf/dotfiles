;;; borrowed with love from
;;; https://gitlab.com/ambrevar/dotfiles
;;; see COPYING in the root of this repo


;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'ambrevar/reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook 'ambrevar/reset-file-name-handler-alist)


;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)


;;; Store additional config in a 'lisp' subfolder and add it to the load path so
;;; that `require' can find the files.
;;; This must be done before moving `user-emacs-directory'.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))


;; (when (require 'package nil t)
;;   ;; TODO: MELPA's https sometimes return
;;   ;;   emacs melpa invalid: certificate host does not match hostname
;;   ;; Try the following:
;;   ;;   (setq tls-checktrust nil)
;;   ;; Different Emacs version have different byte code.  If a versioned ELPA
;;   ;; directory is found, use it.
;;   (let ((versioned-dir (format "elpa-%s.%s" emacs-major-version emacs-minor-version)))
;;     (when (member versioned-dir (directory-files (expand-file-name ".." package-user-dir)))
;;       (setq package-user-dir (expand-file-name (concat "../" versioned-dir) package-user-dir))))
;;   (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/"))
;;   (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;   (package-initialize))


(straight-use-package 'use-package)
(require 'use-package)
(setf
 straight-use-package-by-default t
 use-package-verbose t
 use-package-always-demand t)

;; ;;; Local config.  See below for an example usage.
;; (load "local-before" t)

(require 'functions)
(require 'main)
(require 'visual)


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (display-line-numbers-mode 0)

(use-package general
  :after evil
  :config

  (general-create-definer mememacs/leader-def
   ;; :keymaps '(normal visual)
   :prefix "SPC"
   :global-prefix "C-SPC")

  (general-create-definer
   mememacs/local-leader-def
   :keymaps '(normal visual)
   :prefix ","
   ;; :global-prefix "C-SPC"
   )

  (mememacs/leader-def
   :states 'normal
   "SPC" #'helm-M-x
   "t" '(:ignore t)
   "n" #'line-number-mode

   "b" '(:ignore t)
   "bd" #'kill-buffer-and-window

   "f" '(:ignore t)
   "fd" #'delete-file
   "fs" #'save-buffer

   "w" '(:ignore t)
   "wd" #'delete-window
   "wh" #'windmove-left
   "wl" #'windmove-right
   "wk" #'windmove-up
   "wj" #'windmove-down
   ;; "s"

   )

  ;; (mememacs/define-key
  ;;  )

  ;; (efs/leader-keys
  ;;  "t"  '(:ignore t :which-key "toggles")
  ;;  "tt" '(counsel-load-theme :which-key "choose theme")
  ;;  "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org"))))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(use-package helm
  :config
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  (require 'init-helm)
  )

(use-package magit
  :defer t
  :config
  (setq auto-revert-mode-text "")
  (setq git-commit-summary-max-length fill-column)
  (require 'init-magit))


(use-package company
  :config
  (add-hook 'after-init-hook #'global-company-mode)
  ;; (setq company-idle-delay 0)

  )

(use-package
  helm-company
  :after company
  :config 
  (define-key company-active-map (kbd "C-/") 'helm-company)
  (dolist (map (list company-active-map company-search-map))
    (define-key map (kbd "C-j") 'company-select-next)
    (define-key map (kbd "C-k") 'company-select-previous)
    (define-key map (kbd "C-l") 'company-complete-selection))
  (defun my-company-manual-begin ()
    (interactive)
    (if (company-tooltip-visible-p)
        (company-select-next)
      (company-manual-begin)))
  (define-key evil-insert-state-map (kbd "C-j") #'my-company-manual-begin))



;; TODO
;; (nconc package-selected-packages '(exwm helm-exwm))
;; (nconc package-selected-packages '(pulseaudio-control))

;; (with-eval-after-load 'pulseaudio-control
;;   ;; REVIEW: Upstream should set path dynamically.
;;   ;; https://github.com/flexibeast/pulseaudio-control/issues/7
;;   (setq pulseaudio-control-pactl-path (executable-find "pactl")))
;; (when (require 'exwm nil t) (require 'init-exwm))



