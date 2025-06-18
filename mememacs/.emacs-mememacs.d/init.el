;; -*- lexical-binding: t; -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; borrowed with love from
;;; https://gitlab.com/ambrevar/dotfiles
;;; see COPYING in the root of this repo

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))
        gc-cons-percentage 0.1))
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook 'ambrevar/reset-gc-cons-threshold)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook 'ambrevar/reset-file-name-handler-alist)

;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(setf native-comp-async-report-warnings-errors 'silent)

(straight-use-package 'use-package)
(require 'use-package)

(use-package server
  :ensure nil
  :config
  (unless noninteractive
    (server-start nil t)))

(setf
 straight-vc-git-default-protocol 'https
 straight-use-package-by-default t
 use-package-verbose t
 use-package-always-demand t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))




(defconst mememacs/config-dir user-emacs-directory)

(setf user-mail-address "Benjamin.Schwerdtner@gmail.com")
(setf user-iq 140)

;; 
;; You should make sure that `aw-dispatch-alist' doesn't overlap
;; 
(defvar mememacs/avy-keys
  '(?a ?d ?f ?j ?k ?l ?o ?p ?h ?g ?b))

(use-package hydra)

(use-package debug
  :ensure nil
  :bind
  (:map
    debugger-mode-map
    ("." . backtrace-expand-ellipses)
    ("+" . backtrace-multi-line)
    ("-" . backtrace-single-line)))

(require 'functions)
(require 'utils)
(require 'main)
(require 'visual)
(require 'functions-1)

(setf initial-buffer-choice (mememacs/latest-scratch "el"))



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful)

;; https://github.com/magit/magit/issues/4836
;; straight users fucked, not so nice
;; in clojure we would just not brake you
(use-package compat
  :ensure t
  :straight (:host github :repo "emacs-compat/compat"))

(use-package magit
  :straight (:host github :repo "magit/magit")
  :defer t
  :config
  (setq auto-revert-mode-teaaxt "")
  (setq git-commit-summary-max-length fill-column)
  (with-eval-after-load 'git-commit-mode
    (add-hook 'git-commit-mode-hook
              (defun mm/disable-visual-line-mode ()
                (visual-line-mode -1))))
  (define-key magit-blob-mode-map "n" nil)
  (define-key magit-blob-mode-map (kbd "C-n") nil))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (require 'init-vertico)

  ;; instead of saying 'match required' remove a char
  ;; make it water my friends.
  ;;
  (defun vertico--match-p (input)
    "Return t if INPUT is a valid match."
    (let ((rm minibuffer--require-match))
      (or (memq rm '(nil confirm-after-completion))
          (equal "" input) ;; Null completion, returns default value
          (and (functionp rm) (funcall rm input)) ;; Emacs 29 supports functions
          (test-completion input minibuffer-completion-table minibuffer-completion-predicate)
          (if (eq rm 'confirm) (eq (ignore-errors (read-char "Confirm")) 13)
            (delete-char -1)
            ;; (minibuffer-message "Match required")
            nil)))))

(use-package orderless)

;; org is early
;; see note 20220826T155813--org-file-name-concat__code_issue.org
(use-package org
  :straight (:host github :repo "emacs-straight/org-mode")
  :config (require 'init-org))

(use-package edraw
  :straight (:host github :repo "misohena/el-easydraw")
  :config
  (with-eval-after-load
      'org
    (require 'edraw-org)
    (edraw-org-setup-default))
  (setf
   edraw-default-shape-properties
   `((rect
      (fill . ,edraw-package-default-fill)
      (stroke . ,edraw-package-default-stroke)
      (stroke-width . 2))
     (ellipse
      (fill . ,edraw-package-default-fill)
      (stroke . ,edraw-package-default-stroke)
      (stroke-width . 2))
     (path
      (fill . "none")
      (stroke . ,edraw-package-default-stroke)
      (stroke-width . 2)
      (marker-end . "arrow"))
     (text
      (fill . ,edraw-package-default-stroke)
      ;; Not edraw-package-default-fill
      (font-size . 18)
      (font-family . "sans-serif")
      (text-anchor . "middle"))
     (image)))
  (with-eval-after-load
      "ox"
    (require 'edraw-org)
    (edraw-org-setup-exporter)))

(use-package savehist
  :after vertico
  :init
  (savehist-mode))

(use-package dired
  :straight nil
  :ensure nil
  :config
  (setf dired-listing-switches "-alh")
  (setq-default
   dired-guess-shell-alist-user
   '(("\\.mp4\\'" "mpv")
     ("\\.m4a\\'" "mpv")
     ("\\.pdf\\'" "zathura"))))

(use-package consult
  :after orderless
  :init (recentf-mode)
  (setq completion-in-region-function #'consult-completion-in-region)
  :config
  (require 'init-consult))

(use-package consult-flycheck)

(use-package consult-dir
  :config
  :bind
  (:map
   vertico-map
   ("C-, d" . consult-dir)
   ("C-, j" . consult-dir-jump-file)))

(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (global-set-key (kbd "H-h") #'embark-bindings)
  :config
  (require 'init-embark)
  :bind
  (:map
   embark-symbol-map ("h" . helpful-symbol)))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package wgrep)

(use-package mood-line
  :straight (:host github :repo "benjamin-asdf/mood-line")
  :config
  (setf mood-line-show-cursor-point t)
  (mood-line-mode))

(use-package macrostep
  :config
  (add-hook
   'mememacs/escape-functions
   #'macrostep-collapse-all))

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  (defun mm/sly-complete-at-point ()
    (when (sly-connected-p)
      (let  ((beg (sly-symbol-start-pos))
             (end (sly-symbol-end-pos)))
        (when-let*
            ((completion
              (car (sly--completion-request-completions
                    (buffer-substring beg end)
                    'slynk-completion:flex-completions))))
          (list
           beg
           end
           (completion-table-dynamic
            (lambda (_) completion)))))))

  (sly-symbol-completion-mode -1)
  (remove-hook 'sly-mode-hook 'sly--setup-completion)
  (add-hook
   'sly-mode-hook
   (defun mm/setup-sly-completion ()
     (add-hook 'completion-at-point-functions #'mm/sly-complete-at-point nil t))))

(use-package lispy
  :ensure t
  :config
  (with-eval-after-load 'le-python
    (add-hook
     'python-mode-hook
     (defun mm/add-lispy-python-capf ()
       (add-hook 'completion-at-point-functions #'lispy-python-completion-at-point nil t)))
    (with-eval-after-load 'python
      (advice-add #'python-shell-get-process :after-until
                  #'lispy--python-proc)))
  (defvar *1 nil)
  (defvar *2 nil)
  (defvar *3 nil)

  (advice-add
   #'lispy--eval-elisp-form
   :filter-return
   (defun mm/def-lispy-eval-out (r)
     (setq *3 *2)
     (setq *2 *1)
     (setq *1 r)
     r))

  (require 'lispy-eval-markers)
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-interaction-mode . lispy-mode)
  (lisp-data-mode . lispy-mode)
  (lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (common-lisp-mode . lispy-mode)
  (scheme-mode . lispy-mode)
  (clojure-mode . lispy-mode)
  ;; (python-mode . lispy-mode)
  (racket-mode . lispy-mode)
  ;; (python-mode . mm/enable-le-python)
  )

(use-package multiple-cursors
  :config
  (add-hook
   'mememacs/escape-functions
   (defun mm/mc-remove ()
     (deactivate-mark)
     (mc/remove-fake-cursors))))

(when (require 'project nil t)
  (require 'init-project))

(use-package string-edit-at-point)


(use-package ace-window
  :config
  (setq aw-keys mememacs/avy-keys
        aw-background nil)
  (setf aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?J aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?e aw-execute-command-other-window "Execute Command Other Window")
      (?F aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?B aw-split-window-horz "Split Horz Window")
      (?O delete-other-windows "Delete Other Windows")
      (?T aw-transpose-frame "Transpose Frame")
      ;; ?i ?r ?t are used by hyperbole.el
      (?? aw-show-dispatch-help))))

(use-package cider
  :config
  (require 'init-cider)
  (require 'patch-cider-orderless))

(use-package flycheck
  :config
  (require 'init-flycheck))

(use-package flycheck-clj-kondo
  :after cider)

(use-package geiser
  :config
  (with-eval-after-load 'geiser-mode
    (bind-keys :map geiser-mode-map ("C-."))))

(use-package macrostep-geiser
  :config
  (add-hook 'cider-mode-hook #'macrostep-geiser-setup)
  (add-hook 'geiser-mode-hook #'macrostep-geiser-setup))

;; (use-package geiser-guile
;;   :when mememacs/guile-enabled
;;   :config
;;   (setf
;;    geiser-scheme-implementation 'guile
;;    geiser-guile-binary "guile"
;;    geiser-guile-load-path
;;    (list "/lib/guile/2.2/")))

(use-package geiser-racket
  :config
  (setf geiser-scheme-implementation 'racket)
  (add-hook
   'scheme-mode-hook
   (defun mm/set-racket-impl ()
     (setq-local geiser-impl--implementation 'racket))))

(use-package avy
  :config
  (setf avy-timeout-seconds 0.18
        avy-keys mememacs/avy-keys
        avy-style 'words)
  (add-to-list 'avy-ignored-modes 'cider-repl-mode)
  ;; better not start witch chars from
  ;; avy-dispatch-alist
  (setq
   avy-words
   '("am" "by" "jo" "jl" "jak" "jik"
     "fro" "if" "is" "it" "my" "ox" "up" "em" "eb" "ef"
     "ace" "act" "add" "age" "ago" "aim" "air" "ale" "all" "and" "ant" "any"
     "ape" "apt" "arc" "are" "arm" "art" "ash" "ate" "awe" "axe" "bad" "bag"
     "ban" "bar" "bat" "bay" "bed" "bee" "beg" "bet" "bid" "big" "bit" "bob"
     "bot" "bow" "box" "boy" "but" "cab" "can" "cap" "car" "cat" "cog" "cop"
     "cow" "cry" "cup" "cut" "day" "dew" "did" "die" "dig" "dim" "dip" "dog"
     "dot" "dry" "dub" "dug" "dye" "ear" "eat" "eel" "egg" "ego" "elf" "eve"
     "eye" "fan" "far" "fat" "fax" "fee" "few" "fin" "fit" "fix" "flu" "fly"
     "foe" "fog" "for" "fox" "fry" "fun" "fur" "gag" "gap" "gas" "gel" "gem"
     "get" "gig" "gin" "gnu" "god" "got" "gum" "gun" "gut" "guy" "gym" "had"
     "hag" "ham" "has" "hat" "her" "hid" "him" "hip" "his" "hit" "hop" "hot"
     "how" "hub" "hue" "hug" "hut" "ice" "icy" "imp" "ink" "inn" "ion" "ire"
     "ivy" "jab" "jam" "jar" "jaw" "jet" "job" "jog" "joy" "key" "kid" "kit"
     "lag" "lap" "lay" "let" "lid" "lie" "lip" "lit" "lob" "log" "lot" "low"
     "mad" "man" "map" "mat" "may" "men" "met" "mix" "mob" "mop" "mud" "mug"
     "nag" "nap" "new" "nil" "nod" "nor" "not" "now" "nun" "oak" "odd" "off"
     "oil" "old" "one" "orb" "ore" "ork" "our" "out" "owl" "own" "pad" "pan"
     "par" "pat" "paw" "pay" "pea" "pen" "pet" "pig" "pin" "pit" "pod" "pot"
     "pry" "pub" "pun" "put" "rag" "ram" "ran" "rat" "raw" "ray" "red" "rib"
     "rim" "rip" "rob" "rod" "rot" "row" "rub" "rug" "rum" "run" "sad" "sat"
     "saw" "say" "sea" "see" "sew" "she" "shy" "sin" "sip" "sit" "six" "ski"
     "sky" "sly" "sob" "son" "soy" "spy" "sum" "sun"  "urn" "use" "van"))
  (defun avy-handler-default (char)
    "The default handler for a bad CHAR."
    (let (dispatch)
      (cond ((setq dispatch (assoc char avy-dispatch-alist))
             (setq avy-action (cdr dispatch))
             (throw 'done 'restart))
            ((memq char avy-escape-chars)
             ;; exit silently
             (throw 'done 'abort))
            ((eq char ??)
             (avy-show-dispatch-help)
             (throw 'done 'restart))
            ((mouse-event-p char)
             (signal 'user-error (list "Mouse event not handled" char)))
            (t
             (message "No such candidate: %s, hit `C-g' to quit."
                      (if (characterp char) (string char) char))))))



  ;; temp test if this is better
  (defun avy--line-cands (&optional arg beg end bottom-up)
    "Get candidates for selecting a line.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When BOTTOM-UP is non-nil, display avy candidates from top to bottom"
    (let (candidates)
      (avy-dowindows arg
        (let ((ws (or beg (window-start))))
          (save-excursion
            (save-restriction
              (narrow-to-region ws (or end (window-end (selected-window) t)))
              (goto-char (point-min))
              (while (< (point) (point-max))
                (when (member (get-char-property
                               (max (1- (point)) ws) 'invisible) '(nil org-link))
                  (push (cons
                         (if (eq avy-style 'post)
                             (line-end-position)
                           (save-excursion
                             (when avy-indent-line-overlay
                               (skip-chars-forward " \t"))
                             (point)))
                         (selected-window)) candidates))
                ;; ---------------
                ;; I commented this out - Benjamin
                ;; (if visual-line-mode
                ;;     (line-move-visual 1 t)
                ;;   (forward-line 1))
                ;; ---------------------
                (forward-line 1)
                )))))
      (if bottom-up
          candidates
        (nreverse candidates))))

  (keymap-set isearch-mode-map "C-j" #'avy-isearch))

(use-package symbol-overlay
  :config
  (add-hook
   'mememacs/escape-functions
   (defun mm/so-remove-all ()
     (call-interactively #'symbol-overlay-remove-all)))

  (define-key symbol-overlay-map (kbd "h") nil))

(use-package link-hint)

;; (use-package guix
;;   :when mememacs/enable-guix
;;   :defer t)

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
  :config

  ;; this is when you have :foo in the buffer and you start type
  ;; {:keys [fo]} I wanted this badly
  ;; TODO:
  (defun mm/hippy-lisp-keywords
      (old)
    "Try to expand keyword \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
    (let ((expansion ()))
      (when (not old)
        (he-init-string (he-dabbrev-beg) (point))
        (set-marker he-search-loc he-string-beg)
        (setq he-search-bw t))
      (when (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))
            ;; Try looking backward unless inhibited.
            (if he-search-bw
                (progn
                  (setq expansion
                        (or
                         (let ((he-search-string (concat ":" he-search-string)))
                           (progn
                             (goto-char he-search-loc)
                             (when-let ((exp (he-dabbrev-search he-search-string t)))
                               (cl-subseq exp 1))))
                         (let ((he-search-string (concat "::" he-search-string)))
                           (progn
                             (goto-char he-search-loc)
                             (when-let ((exp (he-dabbrev-search he-search-string t)))
                               (cl-subseq exp 2))))))
                  (set-marker he-search-loc (point))
                  (if (not expansion)
                      (progn
                        (set-marker he-search-loc he-string-end)
                        (setq he-search-bw ())))))

            (if (not expansion)         ; Then look forward.
                (progn
                  (setq expansion
                        (or
                         (let ((he-search-string (concat ":" he-search-string)))
                           (progn
                             (goto-char he-search-loc)
                             (when-let ((exp (he-dabbrev-search he-search-string)))
                               (cl-subseq exp 1))))
                         (let ((he-search-string (concat "::" he-search-string)))
                           (progn
                             (goto-char he-search-loc)
                             (when-let ((exp (he-dabbrev-search he-search-string)))
                               (cl-subseq exp 2))))))
                  (set-marker he-search-loc (point)))))))
      (if (not expansion)
          (progn
            (if old (he-reset-string))
            ())
        (progn
          (he-substitute-string expansion t)
          t))))


  ;; you have banana in the buffer and you type :ba|
  ;; -> :banana
  (defun mm/hippy-lisp-keywords-other-way-around
      (old)
    (let ((expansion ()))
      (when (not old)
        (he-init-string (he-dabbrev-beg) (point))
        (set-marker he-search-loc he-string-beg)
        (setq he-search-bw t))
      (when (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))
            ;; Try looking backward unless inhibited.
            (if he-search-bw
                (progn
                  (setq expansion
                        (or
                         (when-let
                             ((he-search-string
                               (when (string-match-p "::.+" he-search-string)
                                 (cl-subseq he-search-string 2))))
                           (goto-char he-search-loc)
                           (when-let ((s (he-dabbrev-search he-search-string t)))
                             (concat "::" s)))
                         (when-let
                             ((he-search-string
                               (when (string-match-p ":.+" he-search-string)
                                 (cl-subseq he-search-string 1))))
                           (goto-char he-search-loc)
                           (when-let ((s (he-dabbrev-search he-search-string t)))
                             (concat ":" s)))))
                  (set-marker he-search-loc (point))
                  (if (not expansion)
                      (progn
                        (set-marker he-search-loc he-string-end)
                        (setq he-search-bw ())))))

            (if (not expansion)         ; Then look forward.
                (progn
                  (setq expansion
                        (or
                         (when-let
                             ((he-search-string
                               (when (string-match-p "::.+" he-search-string)
                                 (cl-subseq he-search-string 2))))
                           (goto-char he-search-loc)
                           (when-let ((s (he-dabbrev-search he-search-string)))
                             (concat "::" s)))
                         (when-let
                             ((he-search-string
                               (when (string-match-p ":.+" he-search-string)
                                 (cl-subseq he-search-string 1))))
                           (goto-char he-search-loc)
                           (when-let ((s (he-dabbrev-search he-search-string)))
                             (concat ":" s)))))
                  (set-marker he-search-loc (point)))))))
      (if (not expansion)
          (progn
            (if old (he-reset-string))
            ())
        (progn
          (he-substitute-string expansion t)
          t))))


  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          mm/hippy-lisp-keywords
          mm/hippy-lisp-keywords-other-way-around)))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

(load custom-file t)

(use-package winner
  :config (winner-mode))

(use-package denote
  :straight (:host github :repo "protesilaos/denote")
  :config (require 'init-denote))

(use-package markdown-mode)

(use-package bash-completion
  :config
  (bash-completion-setup)
  (setf bash-completion-use-separate-processes t)
  (defun bash-completion-capf-1 (bol)
    (bash-completion-dynamic-complete-nocomint (funcall bol) (point) t))
  (defun bash-completion-eshell-capf ()
    (bash-completion-capf-1 (lambda () (save-excursion (eshell-bol) (point)))))
  (defun bash-completion-capf ()
    (bash-completion-capf-1 #'point-at-bol))
  (add-hook
   'sh-mode-hook
   (defun mm/add-bash-completion ()
     (add-hook 'completion-at-point-functions #'bash-completion-capf nil t)))
  
  (with-eval-after-load
      'comint
    (add-hook
     'comint-mode-hook
     (defun mm/setup-bash-completion-comint ()
       (add-hook
        'completion-at-point-functions
        #'bash-completion-capf
        nil
        t)))
    (define-key comint-mode-map (kbd "TAB")
                #'completion-at-point)))

(use-package backup-each-save
  :config
  (add-hook 'after-save-hook #'backup-each-save)
  (setf make-backup-files nil)

  (defun file-sha256 (filename)
    "Compute the SHA256 of a file called FILENAME."
    (with-temp-buffer
      (call-process "sha256sum" nil t nil filename)
      (car (split-string (buffer-string)))))

  (defun backup-each-save-compute-location (filename)
    (let* ((containing-dir (file-name-directory filename))
           (basename (file-name-nondirectory filename))
           (backup-container
            (format "%s/%s"
                    backup-each-save-mirror-location
                    containing-dir))
           (sha (file-sha256 filename)))
      (when (not (file-exists-p backup-container))
        (make-directory backup-container t))
      (format "%s/%s-%s" backup-container sha basename)))

  (defun mm/jump-to-backup-dir ()
    (interactive)
    (let* ((backup-root-dir (expand-file-name "~/.backups"))
           (buffer-file-path (buffer-file-name))
           (backup-dir (concat
                        backup-root-dir
                        (if buffer-file-path
                            (file-name-directory buffer-file-path)
                          (error "Current buffer is not visiting a file.")))))
      (let ((default-directory backup-dir))
        (if-let
            ((file (car (process-lines "ls" "-t"))))
            (find-file file)
          (if (file-exists-p backup-dir) (dired backup-dir)
            (user-error "Not a file: %s" backup-dir)))))))

(defun load-mu4e ()
  (interactive)
  (use-package mu4e
    :ensure nil
    :straight nil
    :load-path "/usr/share/emacs/site-lisp/mu4e/"

    :config
    ;; This is set to 't' to avoid mail syncing issues when using mbsync
    (setq mu4e-change-filenames-when-moving t)

    ;; Refresh mail using isync every 10 minutes
    (setq mu4e-update-interval ;; (* 10 60)
          nil)
    (setq mu4e-get-mail-command "mbsync -a")
    (setq mu4e-maildir "~/mail")

    (setq mu4e-drafts-folder "/[Gmail]/Drafts")
    (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
    (setq mu4e-refile-folder "/[Gmail]/All Mail")
    (setq mu4e-trash-folder  "/[Gmail]/Trash")

    (setq mu4e-maildir-shortcuts
          '(("/Inbox"             . ?i)
            ("/[Gmail]/Sent Mail" . ?s)
            ("/[Gmail]/Trash"     . ?t)
            ("/[Gmail]/Drafts"    . ?d)
            ("/[Gmail]/All Mail"  . ?a)))))

(use-package artist
  :ensure nil
  :config
  (add-hook 'artist-mode-hook #'artist-select-op-rectangle))

(use-package shell
  :ensure nil
  :config
  (defun mm/with-current-window-buffer (f &rest args)
    (with-current-buffer
        (window-buffer (car (window-list)))
      (apply f args)))

  (defun mm/shell-via-async-shell-command ()
    (let ((display-buffer-alist
           '((".*" display-buffer-same-window))))
      (async-shell-command shell-file-name)))

  (advice-add #'mm/shell-via-async-shell-command :around #'mm/with-current-window-buffer)

  (setf shell-kill-buffer-on-exit t)

  (add-hook
   'shell-mode-hook
   (defun mm/shell-dont-mess-with-scroll-conservatively ()
     (setq-local scroll-conservatively 0))))

(use-package iedit
  :init
  (defvar iedit-toggle-key-default nil)
  :config
  (setq iedit-toggle-key-default (kbd "C-/"))
  (let ((key iedit-toggle-key-default))
    (define-key global-map key 'iedit-mode)
    (define-key isearch-mode-map key 'iedit-mode-from-isearch)
    (define-key esc-map key 'iedit-execute-last-modification)
    (define-key help-map key 'iedit-mode-toggle-on-function))

  (add-hook 'mememacs/escape-functions (defun mm/iedit-quit ()
                                         (when iedit-lib-quit-func (iedit--quit)))))

(use-package elfeed
  :defer t
  :config
  (setq
   elfeed-feeds
   '("https://martinfowler.com/feed.atom"
     "http://gigasquidsoftware.com/atom.xml"
     "http://blog.samaltman.com/posts.atom"
     "http://nullprogram.com/feed/"
     "https://planet.emacslife.com/atom.xml"
     "https://vlaaad.github.io/feed.xml"
     "https://blog.michielborkent.nl/atom.xml"
     "https://writepermission.com/rss.xml"
     "https://planet.clojure.in/atom.xml"
     "https://rigsomelight.com/feed.xml"
     "https://benjamin-asdf.github.io/faster-than-light-memes/planetclojure.xml"
     "https://benjamin-asdf.github.io/faster-than-light-memes/atom.xml")))

(use-package repeat
  :ensure nil
  :config
  (setf repeat-too-dangerous '()))

(use-package
  emacs
  :config (setq save-abbrevs
                'silently
                find-file-suppress-same-file-warnings
                t)
  (setf create-lockfiles nil)
  ;; Unify Marks
  (setq global-mark-ring-max 256)
  (setq set-mark-command-repeat-pop
        256)
  (defun push-mark (&optional location nomsg activate)
    "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil."
    (when (mark t)
      (let ((old (nth mark-ring-max mark-ring))
            (history-delete-duplicates nil))
        (add-to-history
         'mark-ring
         (copy-marker (mark-marker))
         mark-ring-max
         t)
        (when old (set-marker old nil))))
    (set-marker
     (mark-marker)
     (or location (point))
     (current-buffer))
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history
       'global-mark-ring
       (copy-marker (mark-marker))
       global-mark-ring-max
       t)
      (when old (set-marker old nil)))
    (or nomsg
        executing-kbd-macro
        (> (minibuffer-depth) 0)
        (message "Mark set"))
    (if (or activate
            (not transient-mark-mode))
        (set-mark (mark t)))
    nil)
  (setq async-shell-command-buffer
        'new-buffer)
  (defun path-slug (dir)
    "Returns the initials of `dir`s path,
with the last part appended fully

Example:

(path-slug \"/foo/bar/hello\")
=> \"f/b/hello\" "
    (let* ((path (replace-regexp-in-string
                  "\\."
                  ""
                  dir))
           (path (split-string path "/" t))
           (path-s (mapconcat
                    (lambda (it)
                      (cl-subseq it 0 1))
                    (nbutlast
                     (copy-sequence path)
                     1)
                    "/"))
           (path-s (concat
                    path-s
                    "/"
                    (car (last path)))))
      path-s))
  (defun mm/put-command-in-async-buff-name (f &rest args)
    (let* ((path-s (if default-directory
                       (path-slug default-directory)
                     ""))
           (command (car args))
           (buffname (concat path-s " " command))
           (shell-command-buffer-name-async (format
                                             "*async-shell-command %s*"
                                             (string-trim
                                              (substring
                                               buffname
                                               0
                                               (min (length buffname) 75))))))
      (apply f args)))
  (advice-add
   'shell-command
   :around #'mm/put-command-in-async-buff-name)
  ;; do not limit ourselves to a single compilation buffer - what the hell
  (setq-default
   compilation-buffer-name-function
   (defun mm/compilation-buffer-name (_name-of-mode)
     (let* ((path-s (if default-directory
                        (path-slug default-directory)
                      "")))
       (generate-new-buffer-name
        (concat
         "*"
         "compilation -"
         (string-trim
          (substring
           path-s
           0
           (min (length path-s) 75)))
         "* ")))))
  (add-hook
   'comint-mode-hook
   (defun mm/do-hack-dir-locals (&rest _)
     (hack-dir-local-variables-non-file-buffer)))
  ;; it logs a warning when you hack a local
  ;; Making process-environment buffer-local while locally let-bound!
  ;; It is sort of want I want though
  (advice-add
   #'start-process-shell-command
   :before #'mm/do-hack-dir-locals)
  (advice-add
   'compile
   :filter-args (defun mm/always-use-comint-for-compile (args)
                  `(,(car args) t)))
  (add-to-list
   'auto-mode-alist
   '("\\.mjs" . javascript-mode))
  (advice-add
   'yank-pop
   :before (defun mm/add-primary-to-kill-ring ()
             (interactive)
             (setf
              kill-ring
              (append
               kill-ring
               (gui-get-primary-selection)))))
  (setq-default
   compile-command
   "bb ")
  (defalias 'man #'woman))

;; hilarious when you run a spaceship on a super computer and it does silly optimizations from 30 years ago
(use-package bookmark
  :config
  (defun bookmark-time-to-save-p (&rest _) t))

;; elp
;; memory-use-counts
;; instrument package
;; epl results

(use-package
  meow
  :config (require 'init-meow)
  (meow-global-mode 1))

(use-package ligature
  ;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  ;; https://github.com/mickeynp/ligature.el
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures '(prog-mode org-mode)
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" " <~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))

(use-package
  copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config

  (defun rk/copilot-tab ()
    (interactive)
    (when (copilot-accept-completion)
      (indent-for-tab-command)))

  (add-to-list 'warning-suppress-types '((copilot copilot-no-mode-indent)))

  (add-to-list
   'copilot-major-mode-alist
   '("python-mode" . "python"))
  (bind-keys
   :map prog-mode-map
   ("TAB" . rk/copilot-tab)
   ("C-, c" . copilot-mode)
   ("C-, l" . copilot-accept-completion-by-line)
   ("C-, w" . copilot-accept-completion-by-word)
   ("C-, RET" . copilot-accept-completion-by-paragraph)
   ("C-, n" . copilot-next-completion)
   ("C-, p" . copilot-previous-completion)))

(when
    (progn
      (use-package request :ensure t)
      (require 'gemini-quick "/home/benj/repos/gemini-quick.el/gemini-quick.el")
      ;; (require 'gemini-quick-stream "/home/benj/repos/gemini-chat/")
      )
  (meow-leader-define-key
   '(". c" . gemini-quick-chat))
  (setf
   gemini-quick-api-key
   (let ((s))
     (lambda ()
       (or s
           (setf
            s
            (shell-command-to-string
             "pass gai/api-key-2"))))))

  ;; TODO: share
  (defun gemini-quick--stream (input)
    (let* ((default-directory "/home/benj/repos/gemini-chat/")
           (id (s-trim
                (shell-command-to-string
                 "uuidgen")))
           (file (concat
                  "/tmp/gemini-quick--stream-"
                  id))
           (command (format
                     "bb -x gemini-chat/stream-chat --file '%s'"
                     file))
           (shell-command-buffer-name-async (concat "*gemini" "-" id "*")))
      (with-temp-buffer
        (insert input)
        (write-region
         (point-min)
         (point-max)
         file))
      (let ((pb (window-buffer
                 (async-shell-command command))))
        ;; (with-current-buffer
        ;;     pb
        ;;   ;; that's from https://github.com/benjamin-asdf/gemini-quick.el
        ;;   (while (accept-process-output
        ;;           (get-buffer-process
        ;;            (current-buffer)))
        ;;     (sit-for 0.1))
        ;;   (gemini-quick-chat-mode))
        )))
  (defun gemini-quick-chat (arg)
    (interactive "P")
    (let* ((text (if (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning)
                      (region-end))
                   (buffer-substring-no-properties
                    (point-min)
                    (point-max))))
           (text (concat
                  text
                  (when arg
                    (concat
                     "\n"
                     (gemini-quick-read-string))))))
      (gemini-quick--stream text))))

(use-package wolfram-mode
  :config
  (add-to-list
   'auto-mode-alist '("\\.m\\'" . wolfram-mode))
  (add-to-list
   'auto-mode-alist '("\\.wl\\'" . wolfram-mode)))

;; todo
;; https://aur.archlinux.org/packages?O=0&K=jujutsu

;; gems I forget:

;; - avy features
;; - consult-line-multi
