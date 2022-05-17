;;; This file enforces consistency in the visual style:
;;; - doc, here-doc, comments, strings are in the same taint.
;;; - search highlight, search lazy follow the same color code.
;;; - diffs (ediff, smerge, etc.) follow the same color code.

;;; To find the variable associated to a currently used color, place the cursor
;;; on it and call `describe-face'. Or browse the `list-faces-display'.

;; need to set mode line as well
;; and tap seperator

;; https://imagecolorpicker.com/
;; https://external-preview.redd.it/Z9UDrTfwrGa89JgPw-CrtYbgO9WuZulccPJZfZbExC4.jpg?auto=webp&s=cd43fed947bdc62db92383ee75d50c44822493ff

(defconst mindsape/woodsmoke		 "#17161e")
(defconst mindsape/woodsmoke-tint-1	 "#2e2d35")
(defconst mindsape/scarpa-flow		 "#5d5c62")
(defconst mindsape/scorpion		 "#625d5c")
(defconst mindsape/tundora		 "#4e4a4a")
(defconst mindsape/dune	                 "#3b3837")
(defconst mindsape/hint-of-red		 "#faf7f7")
(defconst mindsape/brown		 "#543f2f")
(defconst mindsape/sage			 "#95a178")
(defconst mindsape/heliotrope		 "#F689FF")
(defconst mindsape/heliotrope-shade	 "#311b33")
(defconst mindsape/purple	         "#8e41e0")
(defconst mindsape/fruit-salad	         "#479b59")
(defconst mindsape/sweet-pink	         "#Fe9aa1")
(defconst mindsape/sundown	         "#Feaeb4")
(defconst mindsape/mint-bright		 "#a1fe9a")
(defconst mindsape/mint-bright-1	 "#abfea5")
(defconst mindsape/mint-bright-2	 "#b6feb1")
(defconst mindsape/mint-bright-3	 "#c0febc")
(defconst mindsape/mint-bright-4	 "#d5ffd2")
(defconst mindsape/mint-bright-4	 "#d5ffd2")
(defconst mindsape/blue-chalk	         "#Fcd2ff")
(defconst mindsape/lunar-green	         "#404c3f")
(defconst mindsape/log-cabin	         "#131713")
(defconst mindsape/mint-green		 "#96fe8f")
(defconst mindsape/green-kelp		 "#1e331d"
  "A dark shade of `mindsape/mint-green`")
(defconst mindsape/anakiwa		 "#8fcefe")
(defconst mindsape/water		 "#C7E7FF")
(defconst mindsape/horison		 "#5F89A9")
(defconst mindsape/glacier		 "#6da2bc")
(defconst mindsape/gimbled		 "#bcb66d")
(defconst mindsape/hit-pink		 "#feb48f")
(defconst mindsape/amethyst-smoke	 "#Ac98bf")
(defconst mindsape/lilly "#B8a0af")
(defconst mindsape/summer-green "#a0b8a9")
(defconst mindsape/cursor-default        "NavajoWhite")

;; (ignore-errors
;;   (set-frame-font "Iosevka Fixed SS14-15"))

;; (set-face-foreground 'default "#d1abac")
(set-face-background 'default mindsape/woodsmoke)
(set-face-foreground 'default mindsape/hint-of-red)

(set-face-attribute
'cursor
 nil
 :foreground mindsape/cursor-default
 :background mindsape/cursor-default)


;;; Programming

(set-face-foreground 'font-lock-comment-face mindsape/tundora)
(set-face-foreground 'font-lock-comment-delimiter-face mindsape/scorpion)

(set-face-foreground
 'font-lock-comment-delimiter-face
 (face-foreground
  'font-lock-comment-face))

(set-face-foreground 'font-lock-doc-face "#84892f")

(set-face-foreground 'font-lock-string-face "#abd1d0")
(set-face-foreground 'font-lock-string-face mindsape/gimbled)


;; https://imagecolorpicker.com/color-code/febf8f
(set-face-attribute
 'font-lock-function-name-face
 nil
 :foreground mindsape/mint-green
 :box `(:line-width 1 :color ,mindsape/mint-bright))

(set-face-attribute
 'font-lock-constant-face
 nil
 :foreground mindsape/heliotrope
 :bold t
 :box `(:line-width 2 :color ,mindsape/heliotrope-shade))

(set-face-foreground 'font-lock-keyword-face "#febf8f")

(set-face-attribute
 'match
 nil
 :box nil
 :background mindsape/woodsmoke
 :bold t
 :foreground "green")

;; (set-face-attribute nil :box t)

(set-face-attribute
 'highlight
 nil
 :box
 `(:line-width 2 :color ,mindsape/heliotrope)
 :foreground mindsape/heliotrope
 :background mindsape/woodsmoke)



;; (set-face-attribute
;;  'reb-match-1
;;  nil
;;  :inherit 'reb-match-0
;;  :background mindsape/dune)


(set-face-attribute
 'font-lock-builtin-face
 nil
 :bold t
 :foreground
 ;; "MediumPurple1"
 mindsape/water
 :overline "MediumPurple1"
 )



(set-face-foreground 'font-lock-builtin-face mindsape/anakiwa)

(set-face-background 'region mindsape/tundora)

(set-face-attribute
 'region
 nil
 :foreground mindsape/sundown
 :box nil
 :background mindsape/log-cabin
 :underline nil)


(set-face-background 'mode-line mindsape/brown)
(set-face-background 'mode-line-inactive (face-background 'default))
(set-face-foreground 'mode-line (face-foreground 'default))
(set-face-attribute
 'font-lock-type-face
 nil
 :foreground mindsape/hit-pink
 :italic t
 :bold t)

(set-face-attribute
 'minibuffer-prompt
 nil
 :foreground
 mindsape/fruit-salad
 :bold t)


(when
    (facep
     'separator-line)
  (set-face-attribute
   'separator-line
   nil
   :background mindsape/woodsmoke))

(set-face-foreground 'escape-glyph "#8ffe93")

(set-face-underline 'link "goldenrod")
(set-face-foreground 'link "goldenrod")
(set-face-bold 'link t)

(set-face-background 'fringe mindsape/woodsmoke)
(set-face-background 'secondary-selection mindsape/brown)
(set-face-foreground 'vertical-border mindsape/brown)
(set-face-foreground 'internal-border mindsape/brown)
(set-face-foreground 'window-divider mindsape/brown)

(with-eval-after-load 'paren
  (set-face-foreground 'show-paren-match "White")
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-underline 'show-paren-match "White"))

(with-eval-after-load 'dired
  (set-face-attribute
   'dired-directory
   nil
   :background mindsape/woodsmoke-tint-1
   :foreground mindsape/amethyst-smoke
   :box t))

(with-eval-after-load
    'vertico
  (set-face-attribute
   'vertico-group-title
   nil
   :slant 'italic
   :foreground mindsape/hit-pink))

(with-eval-after-load
    'vertico-quick
  (set-face-attribute
   'vertico-quick1
   nil
   :background mindsape/hit-pink
   :foreground mindsape/woodsmoke)
  (set-face-attribute
   'vertico-quick2
   nil
   :background mindsape/glacier
   :foreground mindsape/woodsmoke))

(with-eval-after-load 'avy
  (set-face-attribute
   'avy-goto-char-timer-face
   nil
   :box nil
   :background mindsape/hit-pink)

  (set-face-attribute
   'avy-lead-face
   nil
   :foreground mindsape/hit-pink
   :background mindsape/woodsmoke
   :box t)

  (set-face-attribute
   'avy-lead-face-1
   nil
   :foreground mindsape/mint-bright-2
   :background mindsape/woodsmoke
   :box t)

  (set-face-attribute
   'avy-lead-face-0
   nil
   :foreground mindsape/mint-bright-1
   :background mindsape/woodsmoke
   :box t)

  (set-face-attribute
   'avy-lead-face-2
   nil
   :foreground mindsape/mint-bright-3
   :background mindsape/woodsmoke
   :box t)

  (set-face-attribute
   'avy-goto-char-timer-face
   nil
   :background mindsape/mint-bright-4
   :foreground mindsape/woodsmoke))

;; (with-eval-after-load 'rainbow-delimiters

;;   (set-face-attribute
;;    'rainbow-delimiters-base-face
;;    nil
;;    :foreground mindsape/hint-of-red
;;    ;; :box '(:line-width -1 :color mindsape/woodsmoke-tint-1)
;;    ))

(setq-default x-stretch-cursor t)
(setq-default visible-cursor nil)
(set-cursor-color mindsape/mint-bright-1)
(blink-cursor-mode -1)


(set-face-attribute
 'lazy-highlight
 nil
 :background mindsape/woodsmoke
 :foreground
 "green"
 :height 1.1
 :weight 'ultra-bold)

(with-eval-after-load
    'isearch
  (set-face-attribute
   'isearch
   nil
   :background mindsape/lilly
   :foreground "black"))

(with-eval-after-load
    'cider
  (set-face-attribute
   'cider-fringe-good-face
   nil
   :foreground mindsape/horison
   :background nil)
  (set-face-attribute
   'cider-error-highlight-face
   nil
   :foreground nil
   :background nil
   :underline `(:color ,mindsape/dune
		       :style wave)
   :box nil)
  ;; cider-instrumented-face
  ;; cider-reader-conditional-face
  )

(with-eval-after-load
    'markdown-mode
    (set-face-attribute
     'markdown-code-face
     nil
     :inherit 'default
     :background mindsape/lunar-green))


(defun ambrevar/fontify-comment-tag ()
  (font-lock-add-keywords
   nil
   ;; See https://en.wikipedia.org/wiki/Comment_(computer_programming)#Tags.
   (mapcar
    (lambda (keyword) `(,(concat "\\<\\(" keyword "\\):") 1 font-lock-warning-face prepend))
    '("FIXME\\(([^)]+)\\)?" "HACK" "KLUDGE" "OPTIMIZE\\(([^)]+)\\)?" "REVIEW\\(([^)]+)\\)?" "TODO\\(([^)]+)\\)?" "UNDONE" "UX" "WARNING" "XXX"))))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'ambrevar/fontify-comment-tag))

;;; Font size
(when (fboundp 'tool-bar-mode)
  ;; (set-face-attribute 'default nil :height 100)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12")))

(global-visual-line-mode)

(provide 'visual)
