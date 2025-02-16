;; This theme is for apes and wizards. 
;; Ideas, fruit and magic are colerful things.

;; https://imagecolorpicker.com/
;; https://external-preview.redd.it/Z9UDrTfwrGa89JgPw-CrtYbgO9WuZulccPJZfZbExC4.jpg?auto=webp&s=cd43fed947bdc62db92383ee75d50c44822493ff

(defconst mememacs-visuals/bg		         "black")
(defconst mememacs-visuals/woodsmoke-shade       "#121218")
(defconst mememacs-visuals/woodsmoke-tint-1	 "#2e2d35")
(defconst mememacs-visuals/scarpa-flow		 "#5d5c62")
(defconst mememacs-visuals/scorpion		 "#625d5c")
(defconst mememacs-visuals/tundora		 "#4e4a4a")
(defconst mememacs-visuals/dune	                 "#3b3837")
(defconst mememacs-visuals/hint-of-red		 "#faf7f7")
(defconst mememacs-visuals/brown		 "#543f2f")
(defconst mememacs-visuals/sage	                 "#95a178")
(defconst mememacs-visuals/heliotrope		 "#F689FF")
(defconst mememacs-visuals/heliotrope-shade	 "#311b33")
(defconst mememacs-visuals/white-pointer	 "#fde7ff")
(defconst mememacs-visuals/mauve         	 "#F8A0FF")
(defconst mememacs-visuals/purple	         "#8e41e0")
(defconst mememacs-visuals/metiorite	         "#3c2488")
(defconst mememacs-visuals/wasabi	         "#708824")
(defconst mememacs-visuals/fruit-salad	         "#479b59")
(defconst mememacs-visuals/sweet-pink	         "#Fe9aa1")
(defconst mememacs-visuals/sundown	         "#Feaeb4")
(defconst mememacs-visuals/mint-bright		 "#a1fe9a")
(defconst mememacs-visuals/mint-bright-1	 "#abfea5")
(defconst mememacs-visuals/mint-bright-2	 "#b6feb1")
(defconst mememacs-visuals/mint-bright-3	 "#c0febc")
(defconst mememacs-visuals/mint-bright-4	 "#d5ffd2")
(defconst mememacs-visuals/blue-chalk	         "#Fcd2ff")
(defconst mememacs-visuals/lunar-green	         "#404c3f")
(defconst mememacs-visuals/log-cabin	         "#131713")
(defconst mememacs-visuals/mint-green		 "#96fe8f")
(defconst mememacs-visuals/green-kelp		 "#1e331d"
  "A dark shade of `mememacs-visuals/mint-green`")
(defconst mememacs-visuals/anakiwa		 "#8fcefe")
(defconst mememacs-visuals/water		 "#C7E7FF")
(defconst mememacs-visuals/peach-orange          "#ffd2a0")
(defconst mememacs-visuals/aquamarine 		 "#a0ffd2")
(defconst mememacs-visuals/horison		 "#5F89A9")
(defconst mememacs-visuals/glacier		 "#6da2bc")
(defconst mememacs-visuals/gimbled		 "#bcb66d")
(defconst mememacs-visuals/hit-pink		 "#feb48f")
(defconst mememacs-visuals/amethyst-smoke	 "#Ac98bf")
(defconst mememacs-visuals/lilly                 "#B8a0af")
(defconst mememacs-visuals/summer-green          "#a0b8a9")
(defconst mememacs-visuals/cursor-default        "NavajoWhite")

(set-face-background 'default mememacs-visuals/bg)
(set-face-foreground 'default mememacs-visuals/hint-of-red)

(set-face-attribute
'cursor
 nil
 :foreground mememacs-visuals/cursor-default
 :background mememacs-visuals/cursor-default)

;;; Programming

(set-face-attribute
 'font-lock-comment-face
 nil
 :foreground "cyan"
 :italic nil)

(set-face-foreground 'font-lock-comment-delimiter-face mememacs-visuals/scorpion)
(set-face-foreground 'font-lock-comment-delimiter-face (face-foreground 'font-lock-comment-face))
(set-face-foreground 'font-lock-doc-face "#84892f")

(set-face-foreground 'font-lock-string-face "#abd1d0")
(set-face-foreground 'font-lock-string-face mememacs-visuals/gimbled)



;; https://imagecolorpicker.com/color-code/febf8f
(set-face-attribute
 'font-lock-function-name-face
 nil
 :foreground mememacs-visuals/mint-green
 :box `(:line-width 1 :color ,mememacs-visuals/mint-bright))

(set-face-attribute
 'font-lock-variable-name-face
 nil
 :underline nil
 :box nil
 :weight 'bold
 :slant 'italic
 :background nil
 ;; "white"
 :foreground "honeydew"
 ;; "honeydew"
 ;; "ivory"
 )

(set-face-attribute
 'font-lock-constant-face
 nil
 :foreground mememacs-visuals/heliotrope
 :bold t
 :underline nil
 :box `(:line-width 2 :color ,mememacs-visuals/heliotrope-shade))

(set-face-foreground 'font-lock-keyword-face "#febf8f")

(set-face-attribute
 'match
 nil
 :box nil
 :background mememacs-visuals/bg
 :bold t
 :foreground mememacs-visuals/mint-bright)

;; (set-face-attribute nil :box t)

(set-face-attribute
 'highlight
 nil
 :box
 `(:line-width 2 :color ,mememacs-visuals/heliotrope)
 :foreground mememacs-visuals/heliotrope
 :background mememacs-visuals/bg)

(set-face-attribute
 'font-lock-builtin-face
 nil
 :bold t
 :foreground "MediumPurple1"
 ;; :overline  mememacs-visuals/aquamarine
 :overline nil)

;; (set-face-foreground 'font-lock-builtin-face mememacs-visuals/anakiwa)

(set-face-attribute
 'region
 nil
 :foreground "white"
 :box nil
 :background "gray49"
 :underline nil)


(set-face-background 'mode-line mememacs-visuals/brown)
(set-face-background 'mode-line-inactive (face-background 'default))
(set-face-foreground 'mode-line (face-foreground 'default))
(set-face-attribute
 'font-lock-type-face
 nil
 :foreground mememacs-visuals/hit-pink
 :italic t
 :bold t
 :underline nil)

(set-face-attribute
 'minibuffer-prompt
 nil
 :foreground
 mememacs-visuals/fruit-salad
 :bold t)

(when
    (facep
     'separator-line)
  (set-face-attribute
   'separator-line
   nil
   :background mememacs-visuals/bg))

(set-face-foreground 'escape-glyph "#8ffe93")

(set-face-underline 'link "goldenrod")
(set-face-foreground 'link "goldenrod")
(set-face-bold 'link t)

(set-face-background 'fringe mememacs-visuals/bg)
(set-face-background 'secondary-selection mememacs-visuals/brown)
(set-face-foreground 'vertical-border mememacs-visuals/brown)
(set-face-foreground 'internal-border mememacs-visuals/brown)
(set-face-foreground 'window-divider mememacs-visuals/brown)

(with-eval-after-load 'paren
  (set-face-foreground 'show-paren-match "White")
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-underline 'show-paren-match "White"))

(with-eval-after-load 'dired
  (set-face-attribute
   'dired-directory
   nil
   :background mememacs-visuals/woodsmoke-tint-1
   :foreground mememacs-visuals/amethyst-smoke
   :box t))

(with-eval-after-load
    'vertico
  (set-face-attribute
   'vertico-group-title
   nil
   :slant 'italic
   :foreground mememacs-visuals/hit-pink))

(with-eval-after-load
    'vertico-quick
  (set-face-attribute
   'vertico-quick1
   nil
   :background mememacs-visuals/hit-pink
   :foreground mememacs-visuals/bg)
  (set-face-attribute
   'vertico-quick2
   nil
   :background mememacs-visuals/glacier
   :foreground mememacs-visuals/bg))

(with-eval-after-load 'avy
  (set-face-attribute
   'avy-goto-char-timer-face
   nil
   :box nil
   :background mememacs-visuals/hit-pink)

  (set-face-attribute
   'avy-lead-face
   nil
   :foreground "red"
   :background mememacs-visuals/bg
   :box t)

  (set-face-attribute
   'avy-lead-face-1
   nil
   :foreground "DarkOrange1"
   :background mememacs-visuals/bg
   :box t)

  (set-face-attribute
   'avy-lead-face-0
   nil
   :foreground "Orange1"
   :background mememacs-visuals/bg
   :box t)

  (set-face-attribute
   'avy-lead-face-2
   nil
   :foreground mememacs-visuals/hit-pink
   :background mememacs-visuals/bg
   :box t)

  (set-face-attribute
   'avy-goto-char-timer-face
   nil
   :background mememacs-visuals/mint-bright
   :foreground mememacs-visuals/bg))

(set-face-attribute 'fixed-pitch nil  :family "Monospace")
(set-face-attribute 'fixed-pitch-serif nil  :family "Monospace")

(setq-default x-stretch-cursor t)
(setq-default visible-cursor nil)
(blink-cursor-mode -1)

(set-face-attribute
 'lazy-highlight
 nil
 :background mememacs-visuals/bg
 :foreground "lawn green"
 :height 1.1
 :weight 'ultra-bold)

(with-eval-after-load
    'isearch
  (set-face-attribute
   'isearch
   nil
   :background mememacs-visuals/lilly
   :foreground "black"))

(with-eval-after-load
    'cider
  (set-face-attribute
   'cider-fringe-good-face
   nil
   :foreground mememacs-visuals/horison
   :background nil)
  (set-face-attribute
   'cider-error-highlight-face
   nil
   :foreground nil
   :background nil
   :underline `(:color ,mememacs-visuals/dune
		       :style wave)
   :box nil))

(set-face-attribute
 'success
 nil
 :foreground mememacs-visuals/mint-bright)

(with-eval-after-load
    'markdown-mode
  (set-face-attribute
   'markdown-code-face
   nil
   :inherit 'default
   :foreground nil
   :background mememacs-visuals/woodsmoke-shade))

(with-eval-after-load
    'embark
  (set-face-attribute
   'embark-target
   nil
   :inherit 'lazy-highlight))

(with-eval-after-load
    'org-faces
  (set-face-attribute
   'org-block
   nil
   :background mememacs-visuals/woodsmoke-shade
   :inherit nil))

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
  (add-to-list 'default-frame-alist '(font . "Fira Code-13"))
  ;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
  )

(global-visual-line-mode)

(with-eval-after-load
    'ansi-color
  (set-face-attribute
   'ansi-color-magenta
   nil
   :foreground "magenta")
  (set-face-attribute
   'ansi-color-green
   nil
   :foreground "chartreuse")
  (set-face-attribute
   'ansi-color-bright-green
   nil
   :foreground "green yellow")
  (set-face-attribute
   'ansi-color-red
   nil
   :foreground "red")
  (setq ansi-color-names-vector
	["black"
	 "red"
	 "chartreuse"
	 "gold1"
	 "DodgerBlue1"
	 "magenta"
	 "cyan1"
	 "gray90"]))


(with-eval-after-load
    'magit-process
  (set-face-attribute
   'magit-process-ok
   nil
   :foreground mememacs-visuals/mint-bright))

(with-eval-after-load
    'lsp-mode
  (set-face-attribute
   'lsp-face-highlight-read
   nil
   :inherit nil
   :underline "white"
   :bold t)
  (set-face-attribute
   'lsp-face-highlight-textual
   nil
   :inherit nil
   :underline nil
   :bold t))

;; figure out something for fira code
(set-face-attribute 'italic nil :underline nil)
(with-eval-after-load
    'ediff-init
  (set-face-attribute 'ediff-current-diff-C nil :foreground "white"))

(with-eval-after-load 'markdown 
(set-face-attribute 'markdown-line-break-face nil :underline nil)
)

(provide 'visual)

