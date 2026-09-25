;;; lispy-tutor.el --- Hands-on lispy-mode tutorial -*- lexical-binding: t -*-
;;
;;       =====================================================
;;        L I S P Y   T U T O R   --   c o m e   o n   i n
;;       =====================================================
;;
;; Welcome.  `lispy-mode' replaces most plain letters with one-key
;; commands whenever the cursor sits on a paren or a region is active.
;; The result feels like vi for Lisp -- shorter than Emacs, modeless
;; in spirit, paren-aware in practice.
;;
;; HOW TO USE THIS TUTOR
;; ---------------------
;; 1. Open this file.  Make sure `lispy-mode' is on (your init.el
;;    already turns it on for `emacs-lisp-mode').  The modeline shows
;;    " LY".  Toggle with =H-i= if needed.
;; 2. Every lesson sits under a `;;*` outline.  Press =J= / =K= to
;;    move between outlines, =i= to fold/unfold, =N= to narrow to
;;    one lesson, =W= to widen back, =I= to fold/unfold all.
;; 3. The code blocks in each lesson are PRACTICE TEXT.  Edit them.
;;    If you mangle them past recognition, revert from git:
;;      git checkout -- ~/.emacs.d/lispy-tutor.el
;; 4. Conventions used below:
;;      |    means cursor position
;;      =k=  means the key k (the one you press literally)
;;      M-x  is the usual Emacs meta-x prefix
;;
;; The tutor assumes your personal config (`~/.emacs.d/init.el`)
;; with meow, the `*1`/`*2`/`*3` eval-result ring, the
;; `lispy-eval-markers` jumpring, and the meow-normal `(` binding
;; that does back-or-wrap.  Lessons that touch your custom keys are
;; clearly marked  ;; *** YOUR CONFIG ***.
;;
;; Time budget: 45-60 minutes for the whole file, less if you skip.
;; You can stop anywhere and come back -- the lessons are independent
;; after Part 1.
;;
;; To start, press =J= until the outline title for Lesson 1.1 is the
;; one your cursor is on, then press =i= to expand it.

;;* Part 1 :: GROUND RULES
;;** Lesson 1.1  -- What "special" means
;;
;; Lispy's secret is that the cursor has TWO modes that you switch
;; between just by moving:
;;
;;   - SPECIAL  : cursor is on `(`, `[`, `{`, `)`, `]`, `}` or
;;                at the start of a comment, or region is active.
;;                Letters call commands.  Digits become prefix args.
;;
;;   - NORMAL   : everything else.  Letters self-insert.
;;
;; That's it.  No mode toggle.  Move onto a paren, you are special.
;; Move off, you are inserting.
;;
;; EXERCISE
;; --------
;; Park your cursor on the `(` of the form below, then try pressing
;; some plain letters: =j= =k= =h= =l= =f= =d=.  Things happen.
;; Now press =C-f= once to step into the form so you sit on `defun`.
;; Try the same letters -- they self-insert.  Press =u= a few times
;; to undo the damage.

(defun greet (name)
  (message "Hello, %s!" name))

;; Tip: turning on `show-paren-mode' makes "am I special?" obvious at
;; a glance.  Your init has it on by default.

;;** Lesson 1.2  -- The four arrows: h j k l
;;
;;            ^ k  (up a sibling sexp)
;;            |
;;   h <------+------> l   (out one level / into next level)
;;            |
;;            v j  (down a sibling sexp)
;;
;; - =j= / =k= move to the next/previous SIBLING sexp at the same depth.
;; - =h= / =l= move to the parent's opener / next-sibling-or-up.
;; - =f=     enters the current list (one level deeper).
;; - =d=     flips you to the OTHER paren of the same form.
;;
;; EXERCISE
;; --------
;; Put the cursor on the very first `(` of the form below.  Walk it:
;;   =f= (enter), =j= a few times, =k= back up,
;;   =l= to exit forward, =h= to exit backward,
;;   =d= flips between `(` and `)` of whatever form you are on.
;;
;; When stuck, =u= undoes; arrow commands never modify text.

(let ((nums '(1 2 3 4 5)))
  (mapcar (lambda (n) (* n n)) nums))

;; Tip: hold the key.  =jjjj= = =4j=.  The chord pattern =hjl= ("up,
;; sibling, deeper") will get muscle-memorised within a day.

;;** Lesson 1.3  -- Entering special from anywhere
;;
;; Two global keys (work even when not special):
;;
;;   =[= -- jump to the closest `(` going backward    (`lispy-backward`)
;;   =]= -- jump to the closest `)` going forward     (`lispy-forward`)
;;
;; They land you ON a paren, i.e. back in special.  Press them twice
;; and you skip out one more level.
;;
;; Two more handy entries:
;;
;;   =M-m=    mark the symbol the cursor is on             (then you are special via region)
;;   =C-M-,=  mark the surrounding list/string/comment     (`lispy-mark`)
;;
;; EXERCISE
;; --------
;; Place the cursor anywhere inside the word "barf" below.
;; Press =[= once: you should now be on the `(` of the innermost list.
;; Press =[= again: outer list.  Now press =]= twice to walk back out.
;; Finally, place cursor on "raise" and press =M-m= -- the word is
;; marked, you are special.  Press =m= to drop the mark.

(progn
  (defun slurp () "extend")
  (defun barf () "shrink")
  (defun raise () "lift one level"))

;;** Lesson 1.4  -- Digit argument
;;
;; In special, =0= ... =9= call `digit-argument`.  Any lispy command
;; can take a numeric prefix:
;;
;;   =3j=  -- move down 3 sexps
;;   =3c=  -- clone the current sexp three times
;;   =9l=  -- exit nine levels of parens (so: end of defun)
;;   =9h=  -- enter nine levels backward (so: start of defun)
;;   =999j=-- go to the last sexp in the file
;;
;; EXERCISE
;; --------
;; Park on the `(` of the form below.  Press =3j=.  You should now
;; be on the third sibling.  Press =9h= to fly to the start of the
;; defun.  Press =9l= to fly to its end.

(setq fruit-list
      '("apple"
        "banana"
        "cherry"
        "durian"
        "elderberry"))

;;** Lesson 1.5  -- Getting OUT of special on purpose
;;
;; Sometimes you want to insert a letter when you happen to be on a
;; paren.  Two clean exits:
;;
;;   =C-f=    forward one char (off the paren, into the form)
;;   =SPC=    inserts a space AND breaks special.
;;            Special trick: at `(|(` it gives you `(| (` so you can
;;            type a new head symbol.
;;   =C-q k=  quoted-insert: inserts the literal `k` even in special.
;;
;; EXERCISE
;; --------
;; Park on the `(` before `defun`.  Type =SPC= -- a space appears,
;; you are now in front of the space, ready to type.  Press =DEL= to
;; remove your edit, then =[= to recover.

(defun shout (s) (upcase s))

;;* Part 2 :: EDITING THE LIST
;;** Lesson 2.1  -- Delete: C-d, DEL, C-S-k
;;
;; Delete in lispy is paren-aware.  It never leaves you with an
;; unbalanced expression by accident.
;;
;;   =C-d=    delete the sexp the cursor sits on (forward direction)
;;   =DEL=    delete the sexp backward
;;   =C-S-k=  kill from cursor to end of line       *** YOUR CONFIG ***
;;            (your `mm/kill-whole-line-or-lispy-kill` runs
;;             `lispy-kill` when in lispy-mode)
;;
;; If the form is empty and you press =C-d= on the `(`, the empty
;; pair disappears.  If non-empty, you walk one sexp at a time
;; (children first, container last).
;;
;; EXERCISE
;; --------
;; Park on the `(` before `with-output-to-string` below.  Press =c=
;; once to clone it.  Now press =C-d= -- a sexp is removed.
;; Continue pressing =C-d= until everything inside is gone, then once
;; more to remove the empty form itself.  Press =u= to revert.

(with-output-to-string
  (princ "and the cow")
  (princ " said: ")
  (princ "moo."))

;;** Lesson 2.2  -- Insert pairs: ( { } "
;;
;;   =(=   inserts ()  and parks you inside       (`lispy-parens`)
;;   ={=   inserts {}  and parks you inside       (`lispy-braces`)
;;   =}=   inserts []  and parks you inside       (`lispy-brackets`)
;;   ="=   inserts ""  and parks you inside       (`lispy-quotes`)
;;
;; All four:
;;   - auto-add a space before if appropriate
;;   - wrap the active region if any
;;   - wrap the current SYMBOL when prefixed with =C-u=
;;   - in strings and comments they self-insert (except `=(=`)
;;
;; *** YOUR CONFIG NOTE ***
;; In meow normal state, `(` is rebound to `mm/lispy-back-or-lispy-pair`:
;;   - with no region : it does `lispy-backward` + enters meow insert.
;;   - with a region  : it wraps the region in parens.
;;
;; EXERCISE
;; --------
;; Place cursor anywhere on the symbol `square` below.  Press =M-m=
;; (mark-symbol).  Now press =(= to wrap it in parens.  Then type
;; "list " and =C-f= to land outside -- the buffer reads
;; `(list square)`.  =u= to revert.

(defun playground ()
  square cube triangle)

;;** Lesson 2.3  -- Clone (c)
;;
;;   =c=     duplicate the current sexp below/above (depends on
;;           which paren you are on).  Cursor stays on the new copy
;;           so you can keep chaining.
;;
;; EXERCISE
;; --------
;; Park on `(` of any line below.  Press =3c= -- three more copies
;; appear.  Now =w= moves the copy up, =s= moves it down (Lesson 2.4).

(setq todo
      '(("buy" "milk")))

;;** Lesson 2.4  -- Move sexps: w (up), s (down)
;;
;;   =w=  swap with previous sibling   (mnemonic: "up the page")
;;   =s=  swap with next sibling       (mnemonic: "south")
;;
;; They re-target your cursor onto the moved sexp so you can hold
;; them like elevator buttons.
;;
;; EXERCISE
;; --------
;; Park on `(princ "C")` below and hold =w= until C is first.

(progn
  (princ "A")
  (princ "B")
  (princ "C"))

;;** Lesson 2.5  -- Slurp and Barf: > and <
;;
;;   =>=   slurp: pull the next/previous sibling INTO the current list
;;   =<=   barf:  push the last/first child OUT of the current list
;;
;; The direction (forward or backward) is decided by which paren you
;; are on.  On `)` it eats/spits to the right; on `(` to the left.
;;
;; With a digit prefix they take/give that many sexps at once:
;;   =3>=  slurp three.
;;
;; EXERCISE
;; --------
;; Park on the `)` of `(when t)` below.  Press =>= -- the `(do-it)`
;; gets sucked in.  Press =>= again -- the `(also-this)` joins.
;; Now =<= to barf one back out.  =u= to revert.

(when t)
(do-it)
(also-this)

;;** Lesson 2.6  -- Raise (r) and Splice (/)
;;
;;   =r=     replace the parent list with the current child
;;           (mnemonic: "raise me up")
;;   =R=     raise the current child and all following siblings
;;   =/=     splice: dissolve the brackets of the current list, the
;;           children become siblings of the parent.
;;
;; EXERCISE
;; --------
;; In the form below, park on `(do-the-thing)` and press =r=.  The
;; surrounding `when` disappears, only `do-the-thing` remains.
;; =u=, then park on the outer `(when t ...)` and press =/= -- now
;; both children of when are spliced into the top level.

(progn
  (when t
    (do-the-thing)
    (do-the-other-thing)))

;;** Lesson 2.7  -- Convolute (C)
;;
;; "Turn this inside-out."  =C= swaps the order of nested forms.
;; Best understood by example.  Sequence:
;;
;;   BEFORE:                   AFTER C on the inner |:
;;   (let ((x 1))
;;     (when y                  (when y
;;       |(use x)))               (let ((x 1))
;;                                  (use x))))
;;
;; Useful for hoisting a `let` out of a `when`/`if`, or vice versa.
;;
;; EXERCISE
;; --------
;; Park on the `(` of `(use x)` and press =C=.  Compare to the
;; example above.  =u= to revert.

(let ((x 1))
  (when y
    (use x)))

;;** Lesson 2.8  -- Oneline (O) and Multiline (M)
;;
;;   =O=  collapse the current sexp onto a single line
;;   =M=  break it across lines, indenting properly
;;        (your bind is `lispy-alt-multiline`)
;;
;; EXERCISE
;; --------
;; Park on the `(` below.  Press =O= -- everything becomes one line.
;; Press =M= -- it breaks again.

(defun example (a b c)
  (+ a b c))

;;** Lesson 2.9  -- Stringify (S) and quotify
;;
;;   =S=        wrap the current sexp in a string, escaping quotes
;;              (and remove escaping if already a string -- toggle)
;;   =C-u "=    insert "" wrapping the symbol, not the sexp
;;
;; EXERCISE
;; --------
;; Park on `(message "hi")` below and press =S=.  You get
;; "(message \"hi\")".  Press =S= again to undo.

(message "hi")

;;** Lesson 2.10 -- Reverse (xR)
;;
;;   =xR=  reverse the order of children in the current list.
;;
;; EXERCISE
;; --------
;; Park on the `(` below and press =xR=.

'(one two three four)

;;** Lesson 2.11 -- Join (+) and Split (M-j)
;;
;;   =+=    join with the next sibling sexp.  E.g. two adjacent let-bindings
;;          merge into one parent.
;;   =M-j=  split the current sexp at point.
;;
;; EXERCISE
;; --------
;; Park on the `(` of the first `let`.  Press =+= and observe.

(let ((x 1)) (use x))
(let ((y 2)) (use y))

;;** Lesson 2.12 -- Comments (;)
;;
;;   =;=        comment / uncomment current line or sexp.
;;   =C-u ;=    reverses the operation.
;;
;; EXERCISE
;; --------
;; Park on the `(` of the form below; press =;= once to comment,
;; once again to uncomment.

(message "now you see me")

;;* Part 3 :: NAVIGATION + SEARCH
;;** Lesson 3.1  -- Avy jumps in a single keystroke
;;
;; Avy overlays hint letters on candidate locations and jumps when
;; you type the hint.  Lispy wires avy to several keys:
;;
;;   =q=   `lispy-ace-paren`       hop to any `(` in current top-level form
;;   =Q=   `lispy-ace-char`        hop to any occurrence of a char
;;   =a=   `lispy-ace-symbol`      jump and mark a symbol
;;   =H=   `lispy-ace-symbol-replace`  pick a symbol and overwrite it
;;   =-=   `lispy-ace-subword`     niche but neat (see README example)
;;   =T=   `lispy-ace-paren`       *** YOUR CONFIG *** also in meow-normal
;;
;; EXERCISE
;; --------
;; Park on the outer `(` of the form below.  Press =q= -- hint
;; letters appear above each inner `(`.  Type one.  You teleport.
;; Now try =a= and pick a symbol; it gets marked.  =m= to drop.

(when (and (numberp x)
           (positive? x)
           (< x 100))
  (do-something-with x))

;;** Lesson 3.2  -- Go to a definition: g, G, F, D
;;
;;   =g=   `lispy-goto-local`     pick a top-level def in current file
;;   =G=   `lispy-goto`           pick from the whole project (semantic/tags)
;;   =F=   `lispy-follow`         jump to the def of symbol at point
;;   =D=   `pop-tag-mark`         jump back where =F= came from
;;   =M-.=, =M-,= are the universal Emacs equivalents and still work.
;;
;; EXERCISE
;; --------
;; Park on the symbol `mapcar` below.  Press =F= -- you should land
;; on the C source or its docstring.  Press =D= to return.

(mapcar #'1+ '(1 2 3))

;;** Lesson 3.3  -- Occur (y) and Knight movement (z)
;;
;;   =y=   `lispy-occur`   like Emacs `occur` but limited to the
;;                         current top-level form and live-filtered.
;;   =z=   the "knight hydra": chess-knight-style jumps for when
;;         the next thing you want is one-down-and-over.
;;
;; EXERCISE
;; --------
;; Park on the `(` below and press =y=.  Type a word that's in the
;; form -- the line list narrows live.  RET to land there.

(progn
  (deflocal alpha 1)
  (deflocal beta 2)
  (deflocal gamma 3)
  (deflocal delta 4))

;;** Lesson 3.4  -- Outlines: J K i I N W
;;
;; Outlines in lispy are comments starting with `;;` followed by
;; one or more `*`.  The depth equals the number of stars.  This
;; file is one big outline tree.
;;
;; Anywhere (no need to be special):
;;
;;   =J=    next outline
;;   =K=    previous outline
;;   =I=    toggle fold all outlines at level 1 (also =S-TAB=)
;;   =2I=   show TOC at level 2
;;
;; When ON an outline header (special):
;;
;;   =i=    fold/unfold this branch
;;   =h= / =l=   promote / demote heading level
;;   =a=    new sibling outline
;;   =t=    move to end of headline
;;   =N=    narrow to this outline
;;   =W=    widen back
;;   =v=    recenter
;;
;; EXERCISE
;; --------
;; Press =I= now to fold everything; press =I= again to unfold.
;; Find this lesson's heading, press =N=, scroll around, then =W=.

;;** Lesson 3.5  -- back (b) -- the undo for navigation
;;
;;   =b=   `lispy-back`  pop the navigation history one step back.
;;
;; Every move (`j` `k` `l` `h` `f` etc.) is recorded.  =b= reverses
;; the journey without modifying any text.  Think of it as
;; navigation undo, complementary to =u= which is edit undo.
;;
;; EXERCISE
;; --------
;; Walk around the form below with =f j j h l j= then press =b=
;; repeatedly to retrace your steps.

(list (cons 'a 1)
      (cons 'b 2)
      (cons 'c 3))

;;* Part 4 :: REGIONS
;;** Lesson 4.1  -- Make a region
;;
;; In special:
;;   =m=          mark the current list                  (`lispy-mark-list`)
;;   =a=          ace-jump and mark a symbol             (`lispy-ace-symbol`)
;;
;; Anywhere:
;;   =M-m=        mark the symbol at point               (`lispy-mark-symbol`)
;;   =C-M-,=      mark the surrounding list/string/comment (`lispy-mark`)
;;
;; Once a region is active, the cursor is "special" too -- single
;; letters call commands until you deactivate.
;;
;; EXERCISE
;; --------
;; Park on the `(` of the form below and press =m=.  Whole list is
;; marked.  Press =m= again to deactivate.  Press =M-m= on a symbol
;; to mark just that.

(setq pizza-toppings '(cheese tomato basil))

;;** Lesson 4.2  -- Grow / shrink the region
;;
;;   =>=   slurp into region: extend by one sexp on the current side
;;   =<=   barf out of region: shrink by one sexp on the current side
;;   =d=   switch which SIDE of the region the cursor is on
;;         (matters because growing happens at the cursor side)
;;   =h= / =l=   move the region up to its parent list
;;   =i=   shrink to first child of the marked list
;;
;; EXERCISE
;; --------
;; Park on `(c d)` below and press =m=.  Now =>= to grow forward.
;; Press =d= to flip sides.  =>= grows backward.  =<= shrinks back.

(quote (a b (c d) (e f) (g h)))

;;** Lesson 4.3  -- Region operations
;;
;; With a region active:
;;
;;   =c=    clone the region (keep it active)
;;   =w=    move region up (swap with previous sibling)
;;   =s=    move region down
;;   =u=    undo and drop region
;;   =t=    teleport: jump the marked region INTO another paren
;;          (avy hint will appear)
;;   =C=    convolute the marked region's parents
;;   =n=    `lispy-new-copy` -- kill-ring-save without dropping the mark
;;   =P=    `lispy-paste`    -- replace region with current kill
;;
;; EXERCISE
;; --------
;; Park on `'(b c)` and press =m=.  Press =c= to clone.  Press =t=
;; -- avy will offer landing parens; pick one.  Your `'(b c)` is
;; pasted there.

(list 'a '(b c) 'd 'e
      (when t
        (do this here)))

;;* Part 5 :: IDE-LIKE FEATURES
;;** Lesson 5.1  -- Eval: e, E, p
;;
;;   =e=   `lispy-eval`              eval the sexp, show result in overlay.
;;   =E=   `lispy-eval-and-insert`   *** YOUR CONFIG *** -- wraps to
;;         `mm/lispy--eval-and-insert`: for Clojure it
;;         `cider-pprint-eval-last-sexp` into a popup; in elisp it
;;         inserts the result inline.  With a prefix it inserts.
;;   =p=   `lispy-eval-other-window` eval and show result in another window
;;   =2e=  eval and append `;; => ...` next to the sexp (handy for notes)
;;
;; *** YOUR CONFIG TIDBITS ***
;; After every =e= on an elisp form, your `mm/def-lispy-eval-out`
;; advice stores the result in `*1`, the previous in `*2`, the one
;; before that in `*3`.  So in scratch you can do:
;;
;;   (+ (length *1) (length *2))   ;; refer back to last two evals
;;
;; Also, `mm/remember-lispy-eval-point` advice remembers the marker
;; at every eval (ring of 3).  Call =M-x mm/lispy-eval-mark-last-or-consult=
;; to jump back to the most recent eval point.  With a prefix arg
;; (=C-u=) you get a `consult` picker of recent eval points across
;; buffers.  Worth binding to a leader key if you eval a lot.
;;
;; EXERCISE
;; --------
;; Park on the `(` below.  Press =e= -- result shows as overlay.
;; Now press =2e= -- a `;; => 21` comment appears.  =u= reverts.
;; Then evaluate the second form to populate `*1`; evaluate (length
;; *1) to confirm the ring is populated.

(* 3 7)
(buffer-name)

;;** Lesson 5.2  -- Inline docs: C-1, C-2
;;
;;   =C-1=   `lispy-describe-inline`  toggle inline docstring overlay
;;   =C-2=   `lispy-arglist-inline`   toggle inline arglist (eldoc-y)
;;
;; The overlays disappear on the next command, no buffer pop-up.
;;
;; EXERCISE
;; --------
;; Park on `mapcar` below and press =C-1=.  The docstring floats in.
;; Now =C-2= for the arglist.

(mapcar #'identity '(1 2 3))

;;** Lesson 5.3  -- Eval-and-replace, eval-expression
;;
;;   =xr=  `lispy-eval-and-replace`   eval the sexp and REPLACE it with
;;         the result.  Excellent for "what is this?" answers.
;;   =xv=  `lispy-eval-expression`    minibuffer eval (M-: with lispy on)
;;
;; EXERCISE
;; --------
;; Park on `(+ 4 5)` below, press =xr= -- it becomes `9`.  =u=.

(+ 4 5)

;;** Lesson 5.4  -- The x hydra (lispy-x): code transforms
;;
;; =x= opens a hydra.  Notable heads (press =x= then the letter):
;;
;;   xb  bind-variable        (extract sub-expression into a `let`)
;;   xu  unbind-variable      (inline a `let`-binding)
;;   xc  to-cond              (turn nested ifs into `cond`)
;;   xi  to-ifs               (turn `cond` into nested ifs)
;;   xd  to-defun             (turn lambda into a top-level defun)
;;   xl  to-lambda            (turn defun back into a lambda)
;;   xf  flatten              (inline a function call -- substitute body)
;;   xF  let-flatten          (substitute a let-bound value)
;;   xk  extract-block        (lift a sub-expr into its own defun call)
;;   xD  extract-defun        (lift selection into a new defun)
;;   xj  debug step-in        (start edebug in this function)
;;   xe  edebug               (instrument this function for edebug)
;;   xm  multi-cursor (cursor-ace)  spawn cursors at avy-picked symbols
;;   xR  reverse              (Lesson 2.10)
;;   x>  toggle-thread-last   (-> / ->> equivalents in Clojure, also elisp)
;;   xh  describe             (describe the function at point)
;;   xC  cleanup              (delete all `;; => ...` overlays this buffer)
;;   xs  save-buffer
;;   xT  ert                  (run ERT tests)
;;   xw  where (show top-level form)
;;
;; Press =x= then =?= for the in-Emacs hint card.
;;
;; EXERCISE 1 -- xb
;; ----------------
;; Park on `(* x x)` below.  Press =xb=.  You'll be prompted for a
;; name (try `sq`).  The expression is extracted into a fresh let.

(defun compute (x)
  (+ 1 (* x x)))

;; EXERCISE 2 -- xc / xi
;; ---------------------
;; Park on `(if ...)` below; =xc= turns it into `cond`.  Park on
;; `(cond ...)` and =xi= turns it back.

(if (zerop n)
    'zero
  (if (positivep n) 'pos 'neg))

;; EXERCISE 3 -- xf flatten
;; ------------------------
;; Park on `(double 3)` below and =xf=.  The body of `double`
;; substitutes in place: `(* 3 2)`.

(defun double (x) (* x 2))
(double 3)

;;** Lesson 5.5  -- Outline as notebook
;;
;; If you press =e= on an OUTLINE header, lispy evals every code
;; block under it sequentially (great for Clojure/Python/Julia,
;; works in elisp too).  An outline whose title ends in `:` gets
;; its result inserted/updated under the title each time you eval.
;;
;; EXERCISE
;; --------
;; Park on the `;;**` heading below, press =e=.  Note how the `result:`
;; outline's body gets the most recent value injected.

;;** Lesson 5.5.example  -- a tiny notebook
(setq notebook-x (random 100))
(setq notebook-y (random 100))

;;*** result:
;; (left empty intentionally; pressing =e= on the parent populates this)

;;** Lesson 5.6  -- edebug (Z, xe, xj)
;;
;;   =xe=     instrument the function at point for edebug
;;   =xj=     "debug step in" -- jump into the function being called
;;   =Z=      `lispy-edebug-stop`  break out of edebug while keeping
;;            current local values stored (look for `*1` style vars).
;;
;; Workflow: =xe= to instrument, then call the function from
;; somewhere; edebug pauses at the entry point.  =SPC= steps.
;; =Z= bails out into normal editing.
;;
;; EXERCISE (optional)
;; -------------------
;; Park on the `(` of `silly` below.  Press =xe=.  Move to the form
;; that calls it; press =e= to evaluate the call.  Edebug stops in
;; the function.  Step a few times with =SPC=, then =Z= to bail.

(defun silly (n)
  (let ((doubled (* 2 n)))
    (+ doubled 3)))
;; (silly 5)

;;* Part 6 :: MISC POWER TOOLS
;;** Lesson 6.1  -- Teleport (t)
;;
;; With a region active, =t= shows avy hints on every `(` in the
;; window and moves the marked region INTO the list you pick.
;; Excellent for "I wrote this here but it belongs there".
;;
;; EXERCISE
;; --------
;; Park on `'(misplaced)` below, press =m= then =t=.  Pick the `(`
;; of the `progn` and watch the form jump into it.

'(misplaced)

(progn
  (do-other-stuff))

;;** Lesson 6.2  -- Multi-cursor (xm)
;;
;;   =xm=  `lispy-cursor-ace`  pop a cursor at every avy-picked symbol.
;;
;; After =xm=, hit the hint letters of symbols you want to edit
;; simultaneously, then press RET; type once, edits land in all.
;;
;; EXERCISE
;; --------
;; Park on the outer `(` of the form below.  Press =xm=.  Pick a
;; few `foo`s, RET, type `bar` to rename them all.  C-g cancels MC.

(list 'foo 'foo 'foo 'baz 'foo 'baz)

;;** Lesson 6.3  -- Repeat (.)
;;
;;   =.=  `lispy-repeat` -- repeat the LAST lispy command.
;;
;; Handy with destructive-ish commands.  E.g. =3>= once, then =.= to
;; do another `3>` slurp.
;;
;; EXERCISE
;; --------
;; Park on the `)` of `(when t)` below.  Press =3>= once.  Now =.=
;; to slurp three more.

(when t)
a b c d e f g h

;;** Lesson 6.4  -- View (v)
;;
;;   =v=  on an outline    : recenter, then on repeats it cycles
;;                            recenter top/middle/bottom.
;;   =v=  on a paren        : `lispy-view` -- center the current sexp
;;                            on the screen.  Useful for very long defs.
;;
;; EXERCISE
;; --------
;; Park on the `(` of `compute` (Lesson 5.4) and press =v= a few
;; times.  Notice the recenter cycle.

;;** Lesson 6.5  -- Newlines, alt-line, meta-return
;;
;;   =RET= / =C-m=    `lispy-newline-and-indent` -- newline + indent
;;                    + clever about commas, &rest, etc.
;;   =M-RET=          `lispy-meta-return`        -- new outline at this
;;                    level OR open new line of cleverness.
;;   =2 SPC=          append a new sexp to the FRONT of current list.
;;   =3 SPC=          append a new sexp to the BACK.
;;   =4 SPC=          like =3 SPC= but on a fresh line.
;;
;; EXERCISE
;; --------
;; Park on the `(` of `(list 1 2 3)` below and press =3 SPC= -- a
;; space + cursor lands at the end, you can type the next element.
;; Then =2 SPC= adds to the front.

(list 1 2 3)

;;** Lesson 6.6  -- Threading (x>)
;;
;;   =x>=  toggle thread-last / inline (Clojure `->>`, elisp `thread-last`).
;;
;; Useful for refactoring deeply nested `(f (g (h x)))` into
;; `(thread-last x h g f)` and back.
;;
;; EXERCISE
;; --------
;; Park on the `(` below and press =x>=.  Repeat to undo.

(reverse (sort (mapcar #'1+ '(3 1 4 1 5 9)) #'<))

;;** Lesson 6.7  -- Undo (u) and Redo
;;
;;   =u=  `lispy-undo` -- deactivates region and undoes.
;;   In your config redo is your usual key (e.g. =U= in meow normal).
;;
;; This is mainly here as a reminder that you can mash =u= safely
;; after every lesson to revert experiments.

;;** Lesson 6.8  -- Quotes-region trick (C-u ")
;;
;; With nothing marked, =C-u "= turns the symbol at point into a
;; string.  With a region, =C-u "= stringifies the region.  Inverse
;; of =S= (stringify-sexp).
;;
;; EXERCISE
;; --------
;; Park on `boom` below; =M-m= to mark it; =C-u "= to stringify.

boom

;;* Part 7 :: YOUR-CONFIG QUIRKS (worth remembering)
;;
;; The following items come from ~/.emacs.d/.  They are NOT vanilla
;; lispy; they make your setup feel like yours.
;;
;;** 7.1 -- =H-i=  toggle lispy-mode
;; A safety toggle if a buffer is being weird; also turns off
;; magit-blame-mode/magit-blob-mode in the same call.
;;
;;** 7.2 -- meow-normal `(`  -- `mm/lispy-back-or-lispy-pair`
;; In meow normal state, `(` is overloaded:
;;   - no region : `lispy-backward` + enter meow-insert.  Quick exit
;;                 from "navigate" to "edit just inside this list".
;;   - region    : wrap the region in parens (`lispy-parens`).
;;
;;** 7.3 -- meow-normal `T` -- ace-paren from anywhere
;; Avoids needing to be special before pressing =q=.
;;
;;** 7.4 -- =e= / =E= in meow-insert and meow-normal
;; The bindings are wired to `mm/lispy--eval` and
;; `mm/lispy--eval-and-insert`.  Notable differences from vanilla:
;;   - in Clojure: =e= calls `cider-eval-last-sexp`; if no arg it
;;     stays at point, prefix arg `C-u` newlines first.
;;   - =E= in Clojure pops a `*mm-lispy-result*` buffer with output;
;;     with `C-u` it pretty-prints into a comment.
;;   - In elisp: =E= inserts result with `C-u`, otherwise behaves
;;     like overlay eval.
;;
;;** 7.5 -- The =*1= =*2= =*3= ring
;; Every successful elisp eval via `lispy--eval-elisp-form` updates
;; these three globals (last, second-to-last, third-to-last).
;; You can refer back to them in subsequent evals.
;;
;;   (+ *1 *2)   ;; sum last two results
;;
;;** 7.6 -- The eval-marker ring + `mm/lispy-eval-mark-last-or-consult`
;; Every =e= remembers the cursor position (per-buffer, ring of 3).
;; Call `mm/lispy-eval-mark-last-or-consult` (currently UNBOUND --
;; consider giving it a leader-key spot like `SPC e j`) to:
;;   - pop the most recent eval point (default), or
;;   - with prefix `C-u`: use `consult-mark` to pick across buffers.
;;
;;** 7.7 -- =C-S-k= -- kill-whole-line-or-lispy-kill
;; In lispy-mode it `lispy-kill`s the rest of the line/sexp; outside
;; lispy-mode it `kill-whole-line`.
;;
;;** 7.8 -- =C-l= -- end of defun + cleanup whitespace
;; `mm/c-l` is overloaded for several states (artist-mode, region,
;; copilot, lispy).  In lispy buffers it jumps to end of defun,
;; cleans trailing whitespace inside the closing paren stack, and
;; enters meow-insert ready to append.
;;
;;** 7.9 -- =C-w= -- `lispy-kill-at-point`
;; Bound on `lispy-mode-map-lispy` to kill the sexp under point
;; regardless of your normal `C-w` (kill-region) habit -- works only
;; when special.
;;
;;** 7.10 -- Auto-disable in CIDER debug
;; When `cider--debug-mode` or `cider-storm-debugging-mode` is
;; entered, lispy is turned off so single keys go to the debugger;
;; on exit, lispy comes back automatically.
;;
;; That's it.  Press =W= to widen, =I I= to refresh outlines, and go
;; write some Lisp.

;;* Cheat sheet (printable)
;;
;; Navigation (special) :  h j k l f d  [ ]  q a -  J K i N W I
;; Movement of sexp     :  w s  c  > <  r R  / + M-j  C X
;; Layout               :  O M  S  ;
;; Region               :  m  M-m  C-M-,  > <  d h l i  c P t n
;; Delete               :  C-d  DEL  C-S-k
;; Eval (your config)   :  e  E  p  2e  xr  *1 *2 *3
;; Inspect              :  C-1  C-2  F D  g G  y
;; Transform (x hydra)  :  xb xu xc xi xd xl xf xk xD xR xm xj xe x> xC
;; Misc                 :  .  v  Z  H-i  C-l  C-w
;;
;; Remember: when in doubt, press =u=.

(provide 'lispy-tutor)
;;; lispy-tutor.el ends here
