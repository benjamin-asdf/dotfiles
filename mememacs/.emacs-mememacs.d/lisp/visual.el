;;; This file enforces consistency in the visual style:
;;; - doc, here-doc, comments, strings are in the same taint.
;;; - search highlight, search lazy follow the same color code.
;;; - diffs (ediff, smerge, etc.) follow the same color code.

;;; To find the variable associated to a currently used color, place the cursor
;;; on it and call `describe-face'. Or browse the `list-faces-display'.


;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox t))

;; need to set mode line as well
;; and tap seperator

;; https://imagecolorpicker.com/
;; https://external-preview.redd.it/Z9UDrTfwrGa89JgPw-CrtYbgO9WuZulccPJZfZbExC4.jpg?auto=webp&s=cd43fed947bdc62db92383ee75d50c44822493ff

(defconst mindsape/woodsmoke "#17161e")
(defconst mindsape/brown "#543f2f")
(defconst mindsape/sage "#95a178")
(defconst mindsape/dark-kelp "#383d2e")
;; (defconst mindscape)

;; (set-face-foreground 'default "#d1abac")
(set-face-background 'default mindsape/woodsmoke)
(set-face-foreground 'default "#faf7f7")

(set-face-attribute
 'cursor
 nil
 :foreground "white"
 :background "white")


;;; Programmingl

(set-face-foreground 'font-lock-comment-face "#6b6375")
(set-face-foreground 'font-lock-comment-delimiter-face
		     (face-foreground 'font-lock-comment-face))
(set-face-foreground 'font-lock-doc-face "#84892f")
(set-face-foreground 'font-lock-string-face "#abd1d0")

;; https://imagecolorpicker.com/color-code/febf8f
(set-face-foreground 'font-lock-function-name-face "#96fe8f")
(set-face-foreground 'font-lock-constant-face ;; "#f68ffe"
;; "#fe8ffb"
		     "#F689FF"


		     )
(set-face-foreground 'font-lock-keyword-face "#febf8f")
(set-face-foreground 'font-lock-builtin-face "#8fcefe")

(set-face-background 'region "#463c52")


(set-face-background 'mode-line mindsape/brown)
(set-face-background 'mode-line-inactive (face-background 'default))
(set-face-foreground 'mode-line (face-foreground 'default))
(set-face-foreground 'font-lock-type-face "#feb48f")
(set-face-italic 'font-lock-type-face t)
(set-face-bold 'font-lock-type-face t)

(set-face-attribute
 'minibuffer-prompt
 nil
 :foreground "yellow"
 :background mindsape/woodsmoke
 :bold t
 :height 1.2)

(set-face-attribute
 'separator-line
 nil
 :background mindsape/woodsmoke)

(set-face-foreground 'escape-glyph "#8ffe93")

;; #fab481

;; ;; (set-face-foreground 'font-lock-variable-name-face (face-foreground 'default))

(set-face-underline 'link "goldenrod")
(set-face-foreground 'link "goldenrod")
(set-face-bold 'link t)

(set-face-background 'fringe mindsape/woodsmoke)
(set-face-background 'secondary-selection mindsape/brown)
(set-face-foreground 'vertical-border mindsape/brown)
(set-face-foreground 'internal-border mindsape/brown)
(set-face-foreground 'window-divider mindsape/brown)



;;  (shadow                                    (:foreground gruvbox-dark4))



;;; Cursor type: default (box) is visible and practical.
; (setq-default cursor-type 'hollow)
;;; Blinking cursor is on only when Emacs is not daemonized.
;;; show-paren
(with-eval-after-load 'paren
  (set-face-foreground 'show-paren-match "White")
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-underline 'show-paren-match "White"))

(with-eval-after-load 'helm
  (set-face-background 'helm-visible-mark (face-background 'region))
  (set-face-underline 'helm-visible-mark t)
  (set-face-foreground 'helm-visible-mark "white")
  (set-face-attribute
   'helm-selection
   nil
   :height 1.2
   :box t
   :background mindsape/brown
   :foreground (face-foreground 'default))

  (set-face-attribute
   'helm-source-header
   nil
   :height 1.2
   :background mindsape/woodsmoke
   :foreground "#32542f")

  ;; (set-face-attribute
  ;;  'helm-header
  ;;  nil
  ;;  :height 1.2
  ;;  :background mindsape/woodsmoke
  ;;  :foreground "yellow")
  )

(with-eval-after-load 'helm-command
  (set-face-attribute
   'helm-M-x-key
   nil
   :foreground
   (face-foreground
    'font-lock-constant-face)))


(setq-default x-stretch-cursor t)
(setq-default visible-cursor nil)
(blink-cursor-mode -1)


(defun ambrevar/fontify-comment-tag ()
  (font-lock-add-keywords
   nil
   ;; See https://en.wikipedia.org/wiki/Comment_(computer_programming)#Tags.
   (mapcar
    (lambda (keyword) `(,(concat "\\<\\(" keyword "\\):") 1 font-lock-warning-face prepend))
    '("FIXME\\(([^)]+)\\)?" "HACK" "OPTIMIZE\\(([^)]+)\\)?" "REVIEW\\(([^)]+)\\)?" "TODO\\(([^)]+)\\)?" "UNDONE" "UX" "WARNING" "XXX"))))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'ambrevar/fontify-comment-tag))


;;; General
;; (set-face-attribute 'default nil :foreground "white" :background "black")

;;; Font size
(when (fboundp 'tool-bar-mode)
  ;; (set-face-attribute 'default nil :height 100)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12")))

;;; More readable but more space consuming; try on big screens.
;; (setq-default line-spacing 1)

;; (set-face-background 'mode-line "white")
;; ;; (set-face-foreground 'link "#00ffff")
;; (set-face-underline 'link t)
;; (set-face-foreground 'minibuffer-prompt "#00ffff")
;; (set-face-background 'region "gray17")
;; (set-face-attribute 'isearch nil :foreground 'unspecified :background "#2f4f4f" :box "white")
;; (set-face-attribute 'lazy-highlight nil :inherit 'isearch :foreground 'unspecified :background 'unspecified :box nil)
;; ;;; TODO: Highlight with box does not render well in Sx, ediff, occur, evil-search.
;; (set-face-attribute 'highlight nil :background 'unspecified :box "white")
;; (set-face-attribute 'error nil :foreground "red" :weight 'bold)


;; ;;; Whitespace mode
;; (with-eval-after-load 'whitespace
;;   (set-face-background 'whitespace-space-after-tab "#a9a9a9")
;;   (set-face-background 'whitespace-indentation "#696969"))


;; ;;; Compilation mode
;; (with-eval-after-load 'compile
;;   (set-face-foreground 'compilation-column-number "cyan")
;;   (set-face-foreground 'compilation-line-number "cyan"))

;; ;;; Ediff
;; (with-eval-after-load 'ediff-init
;;   (set-face-attribute 'ediff-even-diff-A nil :inherit 'ediff-current-diff-A :foreground 'unspecified :background 'unspecified :box nil)
;;   (set-face-attribute 'ediff-odd-diff-A nil :inherit 'ediff-current-diff-A :foreground 'unspecified :background 'unspecified :box nil)
;;   (set-face-attribute 'ediff-even-diff-B nil :inherit 'ediff-current-diff-B :foreground 'unspecified :background 'unspecified :box nil)
;;   (set-face-attribute 'ediff-odd-diff-B nil :inherit 'ediff-current-diff-B :foreground 'unspecified :background 'unspecified :box nil)
;;   (set-face-attribute 'ediff-even-diff-C nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)
;;   (set-face-attribute 'ediff-odd-diff-C nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)
;;   (set-face-attribute 'ediff-current-diff-A nil :box "white")
;;   (set-face-attribute 'ediff-current-diff-B nil :box "white")
;;   (set-face-attribute 'ediff-current-diff-C nil :box "white"))

;; ;;; Outline mode
;; (with-eval-after-load 'outline
;;   ;; (set-face-attribute 'outline-1 nil :inherit 'font-lock-warning-face)
;;   (set-face-attribute 'outline-1 nil :weight 'bold :foreground "#CBAC42")
;;   (set-face-attribute 'outline-2 nil :weight 'bold :foreground "#7BBF11")
;;   (set-face-attribute 'outline-3 nil :weight 'bold :foreground "#BC684F")
;;   (set-face-attribute 'outline-4 nil :weight 'bold :foreground "#4C95BF")
;;   (set-face-attribute 'outline-5 nil :weight 'bold :foreground "SeaGreen")
;;   (set-face-attribute 'outline-6 nil :weight 'bold :foreground "DarkSlateGray4")
;;   (set-face-attribute 'outline-7 nil :weight 'bold :foreground "DarkSlateBlue")
;;   (set-face-attribute 'outline-8 nil :weight 'bold :foreground "Gold"))



;; ;;; Mail mode
;; (font-lock-add-keywords
;;  'mail-mode
;;  '(
;;    ("^From:" . font-lock-warning-face)
;;    ("^To:" . font-lock-warning-face)
;;    ("^Newsgroups:" . font-lock-warning-face)
;;    ("^B?CC:" . font-lock-warning-face)
;;    ("^Subject:" . font-lock-warning-face)
;;    ("^Reply-To:" . font-lock-warning-face)
;;    ("^In-Reply-To:" . font-lock-warning-face)
;;    ;; Mail addresses.
;;    ("\\([[:alnum:]._-]+@[[:alnum:]._-]+\.[[:alnum:]._-]+\\)" 1 font-lock-string-face)
;;    ;; Quotes.
;;    ("^\> *\\([^\> ]\\).*$" . font-lock-string-face)
;;    ("^\> *\> *\\([^\> ]\\).*$" . font-lock-doc-face)
;;    ("^\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
;;    ("^\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
;;    ("^\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
;;    ("^\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
;;    ("^\> *\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
;;    ("^\> *\> *\> *\> *\> *\> *\> *\> *\\([^\> ]\\).*$" . font-lock-comment-face)
;;    ;; Signature (multi-line regexes are a bit flaky).
;;    ("^--.*\\(\n.*\\)*" . font-lock-comment-face)))

;; ;;; Key notes highlighting. We need to apply it to the mode hook since
;; ;;; font-lock-add-keywords has no inheritance support.
;; (set-face-foreground 'font-lock-warning-face "DarkOrange")
;; (defun ambrevar/fontify-comment-tag ()
;;   (font-lock-add-keywords
;;    nil
;;    ;; See https://en.wikipedia.org/wiki/Comment_(computer_programming)#Tags.
;;    (mapcar
;;     (lambda (keyword) `(,(concat "\\<\\(" keyword "\\):") 1 font-lock-warning-face prepend))
;;     '("FIXME\\(([^)]+)\\)?" "HACK" "OPTIMIZE\\(([^)]+)\\)?" "REVIEW\\(([^)]+)\\)?" "TODO\\(([^)]+)\\)?" "UNDONE" "UX" "WARNING" "XXX"))))
;; (dolist (hook '(prog-mode-hook text-mode-hook))
;;   (add-hook hook 'ambrevar/fontify-comment-tag))

;; ;;; Man pages
;; (with-eval-after-load 'man
;;   (set-face-attribute 'Man-underline nil :foreground (face-foreground 'font-lock-string-face) :underline nil)
;;   (set-face-attribute 'Man-overstrike nil :foreground (face-foreground 'font-lock-comment-face) :weight 'normal))
;; (with-eval-after-load 'woman
;;   (set-face-foreground 'woman-bold (face-foreground 'font-lock-comment-face)))

;; ;;; Term
;; ;;; Use lighter blue.
;; (with-eval-after-load 'ansi-color
;;   (setf (aref ansi-color-map 34) '(foreground-color . "#1e90ff")))

(provide 'visual)
