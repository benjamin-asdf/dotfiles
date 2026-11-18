;; clipboard.lisp -- clipboard history picker (clipmenu + dmenu)
;;
;; clipmenud (systemd user unit clipmenud.service, WantedBy
;; stumpwm-session.target) records CLIPBOARD changes. This command opens
;; clipmenu's dmenu picker on the head that currently has focus, the same way
;; mm/select-window-dmenu does for stump-window-select: the dmenu wrapper in
;; ~/.local/bin/pathoverride/dmenu reads DMENU_MONITOR. The chosen clip is put
;; back on CLIPBOARD and PRIMARY; paste it with C-v as usual.

(in-package :stumpwm)

(defparameter *clipboard-history-length* 20
  "Number of lines clipmenu shows in dmenu (CM_HISTLENGTH).")

(defcommand clipboard-history () ()
  "Pick an earlier clipboard entry with clipmenu/dmenu on the current head."
  (if (zerop (length (string-trim '(#\Newline #\Space)
                                  (run-shell-command "command -v clipmenu" t))))
      (message "clipmenu is not installed (sudo pacman -S clipmenu)")
      ;; Not collecting output, so StumpWM does not block while dmenu is up.
      (run-shell-command
       (format nil "DMENU_MONITOR=~d CM_HISTLENGTH=~d clipmenu -i -p clip"
               (head-number (current-head))
               *clipboard-history-length*))))
