;;; EXWM

;;; When stating the client from .xinitrc, `save-buffer-kill-terminal' will
;;; force-kill Emacs before it can run through `kill-emacs-hook'.

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;; REVIEW: Pressing "s-a" ('emms-smart-browse) loses the cursor.
;;; Sometimes waiting helps.  Calling emms-smart-browse manually does not trigger the issue.
;;; https://github.com/ch11ng/exwm/issues/366

;;; REVIEW: helm-mini with follow-mode hangs when using EXWM.
;;; https://github.com/emacs-helm/helm/issues/1889

;;; Rename buffer to window title.
(defun ambrevar/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)


;; The following can only apply to EXWM buffers, else it could have unexpected effects.
(push ?\s-  exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-SPC") #'hydra-exwm/body)

;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)
(exwm-input-set-key (kbd "s-d") 'evil-window-map)

(when (require 'functions)
  (exwm-input-set-key (kbd "s-\\") 'ambrevar/toggle-window-split)
  (exwm-input-set-key (kbd "s-H") 'ambrevar/swap-windows-left)
  (exwm-input-set-key (kbd "s-J") 'ambrevar/swap-windows-below)
  (exwm-input-set-key (kbd "s-K") 'ambrevar/swap-windows-above)
  (exwm-input-set-key (kbd "s-L") 'ambrevar/swap-windows-right))


(exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
(exwm-input-set-key (kbd "s-o") #'ambrevar/toggle-single-window)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)


(require 'functions)
(exwm-input-set-key (kbd "s-<tab>") #'ambrevar/switch-to-last-buffer)
(when (require 'evil nil t)
  (exwm-input-set-key (kbd "s-<tab>") #'evil-switch-to-windows-last-buffer)
  (exwm-input-set-key (kbd "C-6") #'evil-switch-to-windows-last-buffer))

;;; Emacs mode shortcuts.
(exwm-input-set-key (kbd "s-t") #'ambrevar/org-switch-agenda-file)
(exwm-input-set-key (kbd "s-T") #'ambrevar/org-switch-agenda-file-other-window)
(exwm-input-set-key (kbd "s-<return>") #'ambrevar/eshell-or-new-session)
(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))
(when (fboundp 'emms-all)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "S-s-<kp-enter>") #'emms-pause)
  (if (fboundp 'helm-emms)
      (exwm-input-set-key (kbd "s-A") #'helm-emms)
    (exwm-input-set-key (kbd "s-A") #'emms)))
(when (or (fboundp 'mu4e)
          (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path)))
  (exwm-input-set-key (kbd "s-m") #'ambrevar/mu4e-headers))
(exwm-input-set-key (kbd "s-n") #'ambrevar/elfeed-switch-back) ; "n" for "news"
(exwm-input-set-key (kbd "s-e") #'ambrevar/eww-switch-back)
(exwm-input-set-key (kbd "s-E") #'eww)

;;; External application shortcuts.
(defun ambrevar/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'ambrevar/exwm-start)
(exwm-input-set-key (kbd "s-r") #'ambrevar/exwm-start)


;;; Lock screen
(defun ambrevar/exwm-start-lock () (interactive) (start-process "slock" nil "slock"))
(exwm-input-set-key (kbd "s-z") #'ambrevar/exwm-start-lock)

;;; Screenshot
(defun ambrevar/exwm-start-screenshot () (interactive) (start-process-shell-command "scrot" nil "scrot ~/temp/screen-%F-%T.png"))
(exwm-input-set-key (kbd "<print>") #'ambrevar/exwm-start-screenshot)

;;; Volume control
(when (require 'pulseaudio-control nil t)
  (exwm-input-set-key (kbd "s-<kp-subtract>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-<kp-add>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-<kp-enter>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "s--") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-=") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-0") #'pulseaudio-control-toggle-current-sink-mute))

;;; Check for start-up errors. See ~/.profile.
(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda () (get-buffer "*Warnings*"))))))

;;; Some programs such as 'emacs' are better off being started in char-mode.
(defun ambrevar/exwm-start-in-char-mode ()
  (when (string-prefix-p "emacs" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'ambrevar/exwm-start-in-char-mode)



(exwm-enable)

(provide 'init-exwm)
