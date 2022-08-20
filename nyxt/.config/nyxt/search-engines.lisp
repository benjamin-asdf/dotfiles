(in-package #:nyxt-user)

(engines:define-search-engine arch-wiki
    (:shortcut "wa"
     :base-search-url "https://wiki.archlinux.org/?search=~a"
     :fallback-url (quri:uri "https://wiki.archlinux.org/")))

(engines:define-search-engine searx
    (:shortcut "s"
     :base-search-url "https://searx.be/?q=~a"
     :fallback-url (quri:uri "https://searx.be/")))

(define-configuration (buffer web-buffer)
  ((search-engines
    (list
     (searx)
     (engines:google :shortcut "gmaps"
                     :object :maps)
     (engines:wordnet :shortcut "wn"
                      :show-word-frequencies t)
     (engines:google :shortcut "g"
                     :safe-search nil)
     (engines:duckduckgo :theme :terminal
                         :help-improve-duckduckgo nil
                         :homepage-privacy-tips nil
                         :privacy-newsletter nil
                         :newsletter-reminders nil
                         :install-reminders nil
                         :install-duckduckgo nil)
     (engines:arch-aur :shortcut "aur")
     (arch-wiki :shortcut "wa")
     (engines:github :shortcut "gh")
     (engines:wordnet)
     (engines:wikipedia)))))
