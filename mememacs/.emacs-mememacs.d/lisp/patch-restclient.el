(defun restclient-current-min ()
  (save-excursion
    (re-search-backward restclient-method-url-regexp (point-min) t)
    (point)))


(provide 'patch-restclient)
