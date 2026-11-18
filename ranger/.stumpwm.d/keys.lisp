;; keys.lisp -- discoverability for the key bindings.
;;
;;   which-key: after a prefix key (s-, s-g s-r ..., not plain C-t) the bindings of that
;;   map are shown until the next key.
;;
;;   mm/write-key-bindings: every command reachable from *top-map*, one line
;;   each, for the `keys' pick source (s-, o k): search a binding by key or
;;   command name, RET runs it.
;;
;; Interactive keymaps (mm/group-mode, normie-mode, window-picker) show up as
;; the command that enters them; their inner keys are not listed.

(in-package :stumpwm)

(defun mm/which-key-hook (key key-seq cmd)
  "which-key, except right after the prefix key: C-t C-t and friends are
muscle memory, and the root map popup flashes up before the second key."
  (unless (and (null (rest key-seq))
               (equalp (first key-seq) *escape-key*))
    (which-key-mode-key-press-hook key key-seq cmd)))

;; add-hook adjoins, so a reload does not toggle it off like `which-key-mode'.
(remove-hook *key-press-hook* 'which-key-mode-key-press-hook)
(add-hook *key-press-hook* 'mm/which-key-hook)

(defun mm/binding-kmap (command)
  "The keymap COMMAND stands for, nil when it is a command string."
  (cond ((kmap-p command) command)
        ((and command (symbolp command) (boundp command)
              (kmap-p (symbol-value command)))
         (symbol-value command))))

(defun mm/key-bindings (&optional (kmap *top-map*) prefix seen)
  "List of (KEYS . COMMAND) for every command reachable from KMAP."
  (let ((seen (cons kmap seen))
        (result nil))
    (dolist (b (kmap-bindings kmap) (nreverse result))
      (let* ((command (binding-command b))
             (keys (format nil "~@[~a ~]~a" prefix (print-key (binding-key b))))
             (sub (mm/binding-kmap command)))
        (cond ((and sub (not (member sub seen)))
               (setf result (revappend (mm/key-bindings sub keys seen) result)))
              ((stringp command)
               (push (cons keys command) result)))))))

(defun mm/write-key-bindings (file)
  "Write the bindings to FILE as pick candidates: command<TAB>display<TAB>command."
  (with-open-file (s file :direction :output :if-exists :supersede)
    (loop for (keys . command) in (mm/key-bindings)
          do (format s "command~c~16a ~a~c~a~%" #\Tab keys command #\Tab command))))
