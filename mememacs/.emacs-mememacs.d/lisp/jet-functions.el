;;  https://github.com/borkdude/jet

(defun jet-pretty-edn ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))


(provide 'jet-functions)
