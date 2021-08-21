(define-configuration web-buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook *my-request-resource-handlers*
            :initial-value %slot-default%))))
