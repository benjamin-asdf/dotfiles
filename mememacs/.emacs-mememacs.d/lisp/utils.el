;; -*- lexical-binding: t; -*-
(defmacro in (obj &rest things)
  `(or
    ,@(cl-mapcar
     (lambda (it)
       (list 'eq obj it))
     things)))

(defmacro aif (test then-form &rest else-forms)
  "If TEST form returns non nil, bind anaphoric it to it, then
eval THEN-FORM and return the return value of THEN-FORM.
Else eval ELSE-FORMS with implicit progn."
  (declare (indent 2) (debug t))
  `(let ((it ,test))
     (if it ,then-form ,@else-forms)))

(provide 'utils)
