;;; Shepherd User Services

;; See https://github.com/alezost/shepherd-config.
(load "services.scm")

;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.
(apply register-services
       (append main-services
               display-services
               laptop-services))

;; Send shepherd into the background.
(action 'shepherd 'daemonize)

;; Services to start when shepherd starts:
(for-each start
          display-services)
