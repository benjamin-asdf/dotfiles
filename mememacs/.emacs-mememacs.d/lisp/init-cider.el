;; -*- lexical-binding: t; -*-

(setq cider-erepl-display-help-banner nil
      cider-repl-pop-to-buffer-on-connect t
      cider-repl-display-in-current-window t
      cider-font-lock-reader-conditionals nil
      cider-babashka-parameters "nrepl-server 0"
      cider-repl-require-ns-on-set t
      cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow"
      clojure-toplevel-inside-comment-form t
      cider-scratch-initial-message ";; It's not funny, it's powerfull"
      cider-eldoc-display-context-dependent-info t
      cider-clojure-cli-aliases ":lib/tools-deps+slf4j-nop:trace/flowstorm"
      cider-merge-sessions nil
      ;; cider-merge-sessions 'project

      cider-auto-jump-to-error nil
      cider-show-error-buffer nil
      cider-use-overlays t

      cider-enrich-classpath t)

(defvar mm/cider-mode-maps
  '(cider-mode-map
    cider-repl-mode-mapl
    cider-macroexpansion-mode-map))

(set-face-attribute
 'cider-result-overlay-face
 nil
 :background "black")

(set-face-attribute
 'cider-error-overlay-face
 nil
 :background "DarkGreen"
 :foreground "white")

(define-key cider-mode-map (kbd "C-c X") #'cider-selector)

(require 'neil "~/repos/clojure/neil/neil.el" 'no-err)

;; --------------------------------------------

(defun lein-deps-to-deps (beg end)
  (interactive "r")
  (replace-regexp-in-region
   "\\[\\(.+?\\)\\s-\\(\".+?\"\\)\\]"
   "\\1 {:mvn/version \\2}"
   beg
   end)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp-in-region
     "[^/]\\(\\<\\w+\\>\\) {:mvn/version \\(.+\\)}"
     "\\1/\\1 {:mvn/version \\2}")))

(defun mememacs/babashka-scratch (&optional arg)
  (interactive)
  (with-current-buffer
      (mm/scratch arg "clj")
    (corgi/cider-jack-in-babashka)))

;;  --- portal

(defun cider-portal/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(require 'portal.api) (portal.api/tap) (portal.api/open)"))

(defun cider-portal/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun cider-portal/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(defun cider-portal/last-result ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(tap> *1)"))

(defun mm/cider-barf-unless-connected (&rest _)
  (unless (cider-connected-p)
    (user-error "Cider not connected")))

(advice-add 'cider-interactive-eval :before #'mm/cider-barf-unless-connected)
(advice-add 'cider-load-buffer :before #'mm/cider-barf-unless-connected)

;; ---

(defun mm/bb (form)
  (car (read-from-string
        (shell-command-to-string
         (format
          "bb -e '%s'"
          (princ form))))))

(defun mm/cider-jack-in-with-an-alias-from-deps ()
  (interactive)
  (if-let* ((root (project-root (project-current t)))
            (_ (file-exists-p (expand-file-name "deps.edn" root))))
      (let* ((default-directory root)
             (my-alias
              (completing-read
               "Cider jack in with alias: "
               (read-from-string
                (shell-command-to-string
                 (format "bb -e '%s'" (princ '(->> (slurp "\"deps.edn\"") read-string :aliases keys (map name))))))))
             (cider-clojure-cli-aliases
              (concat
               cider-clojure-cli-aliases ":" my-alias)))
        (call-interactively #'cider-jack-in-clj))
    (user-error "no deps.edn file in project")))

(defun clerk-show ()
  (interactive)
  (save-buffer)
  (when-let
      ((filename
        (buffer-file-name)))
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))



(advice-add
 #'cider-eldoc :before-while
 (defun mm/not-when-cider-debug-active (&rest _)
   (not cider--debug-mode)))

;; I think this functionality is ultra silly
;; as if I run out of buffers to create
(defun mm/cleanup-cider-repls-and-do-not-reuse (&rest _)
  (let ((repls (seq-filter
                (lambda (b)
                  (with-current-buffer
                      b
                    (and (derived-mode-p
                          'cider-repl-mode)
                         (not (process-live-p
                               (get-buffer-process b))))))
                (buffer-list))))
    (mapc #'kill-buffer repls)
    '()))

(advice-add #'cider--choose-reusable-repl-buffer :override #'mm/cleanup-cider-repls-and-do-not-reuse)

(defun mm/cider-connect-arcadia ()
  (interactive)
  (let ((port
         (or
          (when-let*
              ((d (project-root (project-current)))
               (default-directory d)
               (f (or (expand-file-name "configuration.edn")
                      (expand-file-name "arcadia-configuration.edn")
                      (expand-file-name "Assets/configuration.edn")))
               (f (when (file-exists-p f) f)))
            (mm/bb `(-> (slurp ,(format "\"%s\"" f)) read-string :nrepl)))
          3722)))
    (cider-connect-clj `(:host "localhost" :port ,port))))

(use-package gdscript-mode
  :when nil
  :config
  (setf
   gdscript-godot-executable "godot-mono"
   gdscript-docs-use-eww nil))

(bind-keys
 :map cider-mode-map
 ("C-, m" . macrostep-expand)
 ("C-, k" . cider-doc)
 ("C-, h" . cider-clojuredocs))

;; happens to me all time I get a compilation error for the toplevel.
;; i.e. fo
;; if you eval a compilation error via evel sexp etc, same thing
;; and it jumps to the top
;; even though that does not help me
(advice-add
 #'cider-jump-to
 :before-while
 (defun mm/cider-do-not-jump-if-pos-1-same-buffer
     (buffer &optional pos other-window)
   (or
    (not (equal buffer (current-buffer)))
    (not (eq pos 1)))))

(defun clojure-insert-ns-form ()
  "Insert a namespace form at the beginning of the buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (if
      ;; when I already have a ns form,
      ;; TODO make it replace the ns name
      (re-search-forward "(ns\\s-" nil t)
      (progn
        (insert " "
                (funcall
                 clojure-expected-ns-function)))
    (clojure-insert-ns-form-at-point)))

(defun mm/pop-cider-error ()
  (interactive)
  (if-let ((cider-error (get-buffer "*cider-error*")))
      (pop-to-buffer cider-error)
    (message "no cider error buffer")))

(define-key cider-mode-map (kbd "C-c E") #'mm/pop-cider-error)

(defun mm/clojure-add-libs-snippet ()
  (interactive)
  (insert
   "(comment
 (require '[clojure.tools.deps.alpha.repl :refer [add-libs]]))
  (add-libs
   '{org.sg.get-currency-conversions/get-currency-conversions
     {:local/root \"../get-currency-conversions/\"}})"))

(defun mm/clojure-dir-locals-snippet ()
  (interactive)
  (with-current-buffer
      (find-file (expand-file-name ".dir-locals.el" (project-root (project-current))))
    (insert
     "((nil .
      ((cider-clojure-cli-aliases . \"dev\")
       (cider-preferred-build-tool . clojure-cli))))
")))

;; some of the source code below is from
;; https://github.com/clojure-emacs/cider


;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2023 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;;         Arne Brasseur <arne@arnebraasseur.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.


;; I want this to always make invisible messages
;; when I run shadow interactively this would end up printing all stderr
(defun cider--display-interactive-eval-result (value value-type &optional point overlay-face)
  "Display the result VALUE of an interactive eval operation.
VALUE is syntax-highlighted and displayed in the echo area.
VALUE-TYPE is one of: `value', `error'.
OVERLAY-FACE is the face applied to the overlay, which defaults to
`cider-result-overlay-face' if nil.
If POINT and `cider-use-overlays' are non-nil, it is also displayed in an
overlay at the end of the line containing POINT.
Note that, while POINT can be a number, it's preferable to be a marker, as
that will better handle some corner cases where the original buffer is not
focused."
  (cl-assert (symbolp value-type)) ;; We assert because for avoiding confusion with the optional args.
  (let* ((font-value (if cider-result-use-clojure-font-lock
                         (cider-font-lock-as-clojure value)
                       value))
         (font-value (string-trim-right font-value))
         (used-overlay (when (and point
                                  cider-use-overlays
                                  (if (equal 'error value-type)
                                      t
                                    (not (equal 'errors-only cider-use-overlays))))
                         (cider--make-result-overlay font-value
                           :where point
                           :duration cider-eval-result-duration
                           :prepend-face (or overlay-face 'cider-result-overlay-face)))))
    nil))

(add-hook 'clojure-mode-hook #'ambrevar/turn-on-delete-trailing-whitespace)

(add-hook
'clojure-mode-hook
(defun mm/add-keywords-to-imenu ()
  (add-to-list 'imenu-generic-expression '(nil "^.?.?\\(:[^ ]+\\).*$" 1) t)))

(with-eval-after-load 'consult

  (defvar cider-consult-repl-buffer-source
    `(:name     "Repl Buffer"
                :category buffer
                :face     consult-buffer
                :history  buffer-name-history
                :action   ,#'consult--buffer-action
                :items
                ,(lambda ()
                   (let ((sessions
                          (or (sesman--linked-sessions 'CIDER 'sort)
                              (sesman--friendly-sessions 'CIDER 'sort))))
                     (mapcar #'buffer-name (mapcar #'cadr sessions)))))
    "Candidate source for `cider-repl-consult'.")

  (defun cider-repl-consult ()
    (interactive)
    (let ((start-buff (current-buffer)))
      (consult-buffer
       (list
        cider-consult-repl-buffer-source))
      (unless (equal start-buff (current-buffer))
        (let ((b (current-buffer)))
          (with-current-buffer start-buff (setq-local curr-cider-repls (list b))))))))

(defun cider-eval-print-handler (&optional buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  ;; NOTE: cider-eval-register behavior is not implemented here for performance reasons.
  ;; See https://github.com/clojure-emacs/cider/pull/3162
  (nrepl-make-response-handler
   (or buffer (current-buffer))
   (lambda (buffer value)
     (with-current-buffer buffer
       (insert
        (if (derived-mode-p 'cider-clojure-interaction-mode)
            (format "\n%s\n" value)
          value))))
   (lambda (_buffer out)
     (cider-emit-interactive-eval-output out))
   (lambda (_buffer err)

     ;; I want this in my life
     ;; should somehow be something on the level of
     ;; cider-emit-interactive-eval-err-output, but that
     ;; doesn't know about the buff right now
     ;; so I wawnt to pass the buff...

     (with-current-buffer
         _buffer
       (let ((end (point-at-eol)))
         (let ((cider-result-use-clojure-font-lock nil))
           (cider--display-interactive-eval-result
            err
            'error
            end
            'cider-error-overlay-face))))

     (cider-emit-interactive-eval-err-output err))
   ()))

(defun cider-interactive-eval-handler (&optional buffer place)
  "Make an interactive eval handler for BUFFER.
PLACE is used to display the evaluation result.
If non-nil, it can be the position where the evaluated sexp ends,
or it can be a list with (START END) of the evaluated region.
Update the cider-inspector buffer with the evaluation result
when `cider-auto-inspect-after-eval' is non-nil."

  (let* ((eval-buffer (current-buffer))
         (beg (car-safe place))
         (end (or (car-safe (cdr-safe place)) place))
         (beg (when beg (copy-marker beg)))
         (end (when end (copy-marker end)))
         (fringed nil)
         (res "")
         (error-phase-requested nil)) ;; avoid requesting the phase more than once - can happen if there are errors during the phase nrepl sync request.
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 ;; value handler:
                                 (lambda (_buffer value)
                                   (setq res (concat res value))
                                   (cider--display-interactive-eval-result res 'value end))
                                 ;; stdout handler:
                                 (lambda (_buffer out)
                                   (cider-emit-interactive-eval-output out))
                                 ;; stderr handler:
                                 (lambda (buffer err)
                                   (cider-emit-interactive-eval-err-output err)


                                   ;; ------------------------------        
                                   ;; I wanted this 👈 👈
                                   (when nrepl-err-handler
                                     (funcall nrepl-err-handler))
                                   ;; This is so it populates the stacktrace buffer
                                   ;; ------------------------------

                                   
                                   (let ((phase (if
                                                    error-phase-requested
                                                    nil
                                                  (setq error-phase-requested t)
                                                  (cider--error-phase-of-last-exception buffer))))

                                     (cider--maybe-display-error-as-overlay phase err end)

                                     (cider-handle-compilation-errors err
                                                                      eval-buffer
                                                                      ;; we prevent jumping behavior on compilation errors,
                                                                      ;; because lines tend to be spurious (e.g. 0:0)
                                                                      ;; and because on compilation errors, normally
                                                                      ;; the error is 'right there' in the current line
                                                                      ;; and needs no jumping:
                                                                      phase)))
                                 ;; done handler:
                                 (lambda (buffer)
                                   (if beg
                                       (unless fringed
                                         (cider--make-fringe-overlays-for-region beg end)
                                         (setq fringed t))
                                     (cider--make-fringe-overlay end))
                                   (when (and cider-auto-inspect-after-eval
                                              (boundp 'cider-inspector-buffer)
                                              (windowp (get-buffer-window cider-inspector-buffer 'visible)))
                                     (cider-inspect-last-result)
                                     (select-window (get-buffer-window buffer)))
                                   (when cider-eval-register
                                     (set-register cider-eval-register res))))))

(defun cider-popup-eval-handler (&optional buffer)
  "Make a handler for printing evaluation results in popup BUFFER.
This is used by pretty-printing commands."
  ;; NOTE: cider-eval-register behavior is not implemented here for performance reasons.
  ;; See https://github.com/clojure-emacs/cider/pull/3162
  (let ((target-buff (current-buffer))
        (end (line-end-position)))
    (nrepl-make-response-handler
     (or buffer (current-buffer))
     (lambda (buffer value)
       (cider-emit-into-popup-buffer buffer (ansi-color-apply value) nil t)
       (with-current-buffer buffer
         (read-only-mode -1))
       (display-buffer buffer))
     (lambda (_buffer out)
       (cider-emit-interactive-eval-output out))
     (lambda (_buffer err)
       (pop-to-buffer target-buff)

       ;; I want his
       (let ((cider-result-use-clojure-font-lock nil))
         (cider--display-interactive-eval-result
          err
          'error
          end
          'cider-error-overlay-face))

       (cider-emit-interactive-eval-err-output err))
     nil
     nil
     nil
     (lambda (buffer warning)
       (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t)))))

(defun cider--pprint-eval-form (form)
  "Pretty print FORM in popup buffer."
  (let* ((buffer (current-buffer))
         (result-buffer (cider-make-popup-buffer cider-result-buffer 'clojure-mode 'ancillary))
         (handler (cider-popup-eval-handler result-buffer)))
    (with-current-buffer
        buffer
      (cider-interactive-eval
       (when (stringp form) form)
       handler
       (when (consp form) form)
       (cider--nrepl-print-request-map
        fill-column)))))

(defun mm/zprint-region ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "zprint" (buffer-name) t)))

;; schon cute irgendwie
;; (global-prettify-symbols-mode 1)
;; (mapc (lambda (m) (add-hook m (lambda () (push '("fn" . ?ƒ) prettify-symbols-alist))))
;;       '(clojure-mode-hook clojurescript-mode-hook))


;; ----

;; edging out some perf
;; See https://faster-than-light-memes.xyz/zappy-cider-eval-overlays.html
(cl-defun cider--make-result-overlay
    (value
     &rest
     props
     &key
     where
     duration
     (type 'result)
     (format
      (concat
       " "
       cider-eval-result-prefix
       "%s "))
     (prepend-face
      'cider-result-overlay-face)
     &allow-other-keys)
  "Place an overlay displaying VALUE at the position determined by WHERE.
      VALUE is used as the overlay's after-string property, meaning it is
      displayed at the end of the overlay.
      Return nil if the overlay was not placed or if it might not be visible, and
      return the overlay otherwise.

      Return the overlay if it was placed successfully, and nil if it failed.

      This function takes some optional keyword arguments:

        If WHERE is a number or a marker, apply the overlay as determined by
        `cider-result-overlay-position'.  If it is a cons cell, the car and cdr
        determine the start and end of the overlay.
        DURATION takes the same possible values as the
        `cider-eval-result-duration' variable.
        TYPE is passed to `cider--make-overlay' (defaults to `result').
        FORMAT is a string passed to `format'.  It should have
        exactly one %s construct (for VALUE).

      All arguments beyond these (PROPS) are properties to be used on the
      overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cdr (cdr props))))
  ;; If the marker points to a dead buffer, don't do anything.
  (when-let
      ((buffer
        (cond ((markerp where)
               (marker-buffer where))
              ((markerp (car-safe where))
               (marker-buffer (car where)))
              (t (current-buffer)))))
    (with-current-buffer
        buffer
      (save-excursion
        (when (number-or-marker-p where)
          (goto-char where))
        ;; Make sure the overlay is actually at the end of the sexp.
        (skip-chars-backward
         "\r\n[:blank:]")
        (let* ((beg (if (consp where)
                        (car where)
                      (save-excursion
                        (clojure-backward-logical-sexp
                         1)
                        (point))))
               (end (if (consp where)
                        (cdr where)
                      (pcase
                          cider-result-overlay-position
                        ('at-eol (line-end-position))
                        ('at-point (point)))))
               ;; Specify `default' face, otherwise unformatted text will
               ;; inherit the face of the following text.
               (display-string (format
                                (propertize
                                 format
                                 'face
                                 'default)
                                value))
               (o nil))
          ;; Remove any overlay at the position we're creating a new one, if it
          ;; exists.
          (remove-overlays
           beg
           end
           'category
           type)
          (funcall
           (if cider-overlays-use-font-lock
               #'font-lock-prepend-text-property
             #'put-text-property)
           0
           (length display-string)
           'face
           prepend-face
           display-string)
          ;; If the display spans multiple lines or is very long, display it at
          ;; the beginning of the next line.
          (when (or (string-match
                     "\n."
                     display-string)
                    (> (length display-string)
                       (-
                        (window-width)
                        (current-column))))
            (setq display-string
                  (concat " \n" display-string)))
          ;; Put the cursor property only once we're done manipulating the
          ;; string, since we want it to be at the first char.
          (put-text-property
           0
           1
           'cursor
           0
           display-string)
          (when (> (length display-string)
                   (window-width))
            (setq display-string
                  (concat
                   (substring
                    display-string
                    0
                    (window-width))
                   (substitute-command-keys
                    "...
Result truncated. Type `\\[cider-inspect-last-result]' to inspect it."))))
          ;; Create the result overlay.
          (setq o
                (apply
                 #'cider--make-overlay
                 beg
                 end
                 type
                 'after-string
                 display-string
                 props))
          (pcase
              duration
            ((pred numberp)
             (run-at-time
              duration
              nil
              #'cider--delete-overlay
              o))
            (`command
             ;; Since the previous overlay was already removed above, we should
             ;; remove the hook to remove all overlays after this function
             ;; ends. Otherwise, we would inadvertently remove the newly created
             ;; overlay too.
             (remove-hook
              'post-command-hook
              'cider--remove-result-overlay
              'local)
             ;; If inside a command-loop, tell `cider--remove-result-overlay'
             ;; to only remove after the *next* command.
             (if this-command
                 (add-hook
                  'post-command-hook
                  #'cider--remove-result-overlay-after-command
                  nil
                  'local)
               (cider--remove-result-overlay-after-command)))
            (`change
             (add-hook
              'after-change-functions
              #'cider--remove-result-overlay
              nil
              'local)))
          (when-let*
              ((win
                (get-buffer-window buffer)))
            ;; Left edge is visible.
            (when (and (<= (window-start win)
                           (point)
                           (window-end win))
                       ;; Right edge is visible. This is a little conservative
                       ;; if the overlay contains line breaks.
                       (or (< (+
                               (current-column)
                               (length value))
                              (window-width win))
                           (not truncate-lines)))
              o)))))))

;; ===================
;; cider-repls
;; ===================
;;
;; 1. For perf, remember the current repl.
;; when I have remote connections, perf is abysmall, because sesman subroutine calls
;; filename or something recursively
;;
;; 2. Only return a single repl, I don't need the dual cljc concept.
;;
;; 3. I select a cider repl by making the desired connection buffer (repl) active,
;; then I revert-buffer to clear curr-cider-repls
;; viola - curr-cider-repls will be set to my desired repl buffer.

;; this scheme has served me well.
;; The downside is that there is a local state, buffer local, curr-cider-repls
;; Which you sort of need to be aware of.
;; The benefit is, I am selecting the repl I want per buffer, with my user intent.
;;

(defvar-local curr-cider-repls nil)

(defun sort-latest-buffer (buffs)
  "Return the latest used buffer from BUFFS."
  (car
   (mapcar #'cdr (sort (mapcan
                        (lambda (b)
                          (with-current-buffer b
                            (if-let ((w (get-buffer-window b)))
                                (list (cons (window-use-time w)
                                            (current-buffer))))))
                        buffs)
                       (lambda (a b) (> (car a) (car b)))))))

(defun cider-repls (&optional type ensure)
  "Return cider REPLs of TYPE from the current session.
If TYPE is nil or multi, return all REPLs.  If TYPE is a list of types,
return only REPLs of type contained in the list.  If ENSURE is non-nil,
throw an error if no linked session exists."
  (if (eq curr-cider-repls 'none)
      nil
    (or (-filter
         #'buffer-live-p
         curr-cider-repls)
        (setf
         curr-cider-repls
         (let ((type (cond ((listp type)
                            (mapcar
                             #'cider-maybe-intern
                             type))
                           ((cider-maybe-intern type))))
               (repls (pcase
                          cider-merge-sessions
                        ('hostv
                         (if ensure
                             (or (cider--extract-connections
                                  (cider--get-sessions-with-same-host
                                   (sesman-current-session 'CIDER)
                                   (sesman-current-sessions
                                    'CIDER)))
                                 (user-error
                                  "No linked %s sessions"
                                  'CIDER))
                           (cider--extract-connections
                            (cider--get-sessions-with-same-host
                             (sesman-current-session 'CIDER)
                             (sesman-current-sessions
                              'CIDER)))))
                        ('project
                         (if ensure
                             (or (cider--extract-connections
                                  (sesman-current-sessions
                                   'CIDER))
                                 (user-error
                                  "No linked %s sessions"
                                  'CIDER))
                           (cider--extract-connections
                            (sesman-current-sessions
                             'CIDER))))
                        (_
                         (cdr (if ensure
                                  (sesman-ensure-session 'CIDER)
                                (sesman-current-session 'CIDER)))))))
           (or (let ((r (seq-filter
                         (lambda (b)
                           (unless (cider-cljs-pending-p b)
                             (cider--match-repl-type type b)))
                         repls)))
                 r
                 ;; (list (sort-latest-buffer r))
                 )
               (when ensure
                 (cider--no-repls-user-error
                  type))))))))


;; -----------------------------------------------------------


;; Also count 'do' as honary ignore toplevel

(defun clojure-beginning-of-defun-function (&optional n)
  "Go to top level form.
Set as `beginning-of-defun-function' so that these generic
operators can be used.  Given a positive N it will do it that
many times."
  (let ((beginning-of-defun-function nil))
    (if (and clojure-toplevel-inside-comment-form
             (or
              (clojure-top-level-form-p "comment")
              (clojure-top-level-form-p "do")) ;; 👈
             )
        (condition-case nil
            (save-match-data
              (let ((original-position (point))
                    clojure-comment-end)
                (beginning-of-defun-raw)
                (end-of-defun)
                (setq clojure-comment-end (point))
                (beginning-of-defun-raw)
                (forward-char 1) ;; skip paren so we start at comment
                (clojure-forward-logical-sexp) ;; skip past the comment form itself
                (if-let ((sexp-start (clojure-find-first (lambda (beg-pos)
                                                           (< beg-pos original-position))
                                                         (clojure-sexp-starts-until-position
                                                          clojure-comment-end))))
                    (progn (goto-char sexp-start) t)
                  (beginning-of-defun-raw n))))
          (scan-error (beginning-of-defun-raw n)))
      (beginning-of-defun-raw n))))


(provide 'init-cider)


