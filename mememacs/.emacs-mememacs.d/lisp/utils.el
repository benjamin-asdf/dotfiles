(defmacro in (obj &rest things)
  `(or
    ,@(cl-mapcar
     (lambda (it)
       (list 'eq obj it))
     things)))

(provide 'utils)
