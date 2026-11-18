# pick: sources × verbs

You choose a **thing** from a **source**, then choose a **verb** to do with
it. Sources and verbs are plain executables in this directory
(`~/.config/pick/`, stowed from `scripts/.config/pick/`). Any source works
with any verb that accepts the thing's **type**. To add one, drop a file
here; there is nothing to register.

```
source ──lines──▶ pick (dmenu) ──value──▶ act (dmenu over verbs) ──▶ verb
                                  ▲
selection / clipboard ── act-selection (Embark for X)
```

## Keys (StumpWM)

| Key            | Does                                                    |
|----------------|---------------------------------------------------------|
| `s-, o <key>`  | pick from the source with `# key: <key>`                |
| `s-, o SPC`    | dmenu over all sources                                  |
| `s-, o .`      | act on the selection (PRIMARY, fallback CLIPBOARD)      |
| `s-, o ,`      | act on the CLIPBOARD (e.g. right after Wispr dictation) |
| `s-, .`        | act on the selection (shortcut for `s-, o .`)           |

Source keys: `w` windows · `m` marks · `y` clipboard · `a` pass · `n` notes ·
`g` PRs · `r` recordings · `b` bookmarks.

Run `pick-reload` (StumpWM command) after you add a source or change a
`# key:`. Front end: `ranger/.stumpwm.d/pick.lisp`. Every command runs
asynchronously and dmenu opens on the current head.

## Shell

```sh
pick                      # dmenu over sources, then items, then verbs
pick notes                # items of one source, then verbs
pick notes emacs          # verb fixed: no verb menu
pick bookmarks -p         # print the chosen value instead of acting
pick bookmarks -p | act copy
pick -l                   # list sources
echo foo | act claude     # run a verb on stdin
echo https://x.org | act  # type detected (url), dmenu over url verbs
act -l -t file            # which verbs accept files
act-selection [-c] [VERB] # Embark for X (-c: CLIPBOARD only)
pick -h / act -h          # full contract
```

Text you type into the item menu that matches no item is used as the value.

## Types

`url` · `file` · `text` · `window` · `mark` · `pass`. A new type needs no
setup: once a source emits it and some verb lists it in `# types:`, the
two work together.

`act` with no `-t` works out the type itself: a single-line URL is `url`,
an existing path (`~` expanded) is `file`, and anything else is `text`.

## Writing a source

`sources/NAME`: executable, no args, prints one item per line:

```
TYPE<TAB>DISPLAY<TAB>VALUE
```

- `TYPE<TAB>VALUE` also works (display = value), and a bare `VALUE` is
  type `text`.
- VALUE must fit on one line: write `\` newline tab as `\\` `\n` `\t`, and
  pick decodes them, so multi-line text works.
- Optional headers, anywhere in the first 15 lines:
  - `# desc: …` is the text shown in menus.
  - `# key: k` is the letter in `*pick-map*`.
  - `# verb: NAME` is the verb offered first for this source.
- Keep it fast. Cache slow calls in `~/.cache/pick/`, as `prs` does
  (5 min, refreshed in the background).
- If the tool it needs is missing, print nothing and exit 0.

Example:

```sh
#!/bin/sh
# desc: repos under ~/repos
# key: p
for d in ~/repos/*/; do printf 'file\t%s\t%s\n' "$(basename "$d")" "$d"; done
```

## Writing a verb

`verbs/NAME`: executable. `$1` = type, the value arrives on **stdin** (no
trailing newline). The environment also has `PICK_TYPE`, `PICK_DISPLAY`
and `PICK_SOURCE`.

- `# types: url text` is **required**: the types this verb accepts
  (`*` = all).
- `# default: url` puts the verb at the top of the menu for those types.
- `# desc: …` is the text shown in menus.

Verb menu order:
1. the source's `# verb:`
2. verbs whose `# default:` lists the type
3. all other verbs, alphabetically

Example:

```sh
#!/bin/sh
# types: text url
# desc: send to Teams chat draft via clipboard + notify
xclip -selection clipboard && notify-send "ready to paste"
```

A verb can act differently depending on the name it was called by. `note`
is a symlink to `capture`: `capture` appends to today's denote journal, and
`note` makes a new denote note.

## What's here

**Sources**

| name        | key | type        | first verb | from                          |
|-------------|-----|-------------|------------|-------------------------------|
| windows     | w   | window      | focus      | StumpWM, all groups           |
| marks       | m   | mark        | focus      | `marks.lisp` / `marks.sexp`   |
| clipboard   | y   | text        |            | clipmenu history, newest first|
| pass        | a   | pass        | pass-copy  | `~/.password-store`           |
| notes       | n   | file        |            | denote dirs (asked from Emacs)|
| prs         | g   | url         |            | `gh`, review-requested, cached|
| recordings  | r   | file        | play       | `~/Pictures` rec-/screen-/box-|
| bookmarks   | b   | url         |            | qutebrowser quickmarks + bookmarks |

**Verbs**

| verb      | types           | does                                                     |
|-----------|-----------------|----------------------------------------------------------|
| copy      | *               | to CLIPBOARD (default for text)                          |
| paste     | text url file   | type into the focused window (xdotool)                   |
| open      | url file        | URLs in qutebrowser, files via xdg-open (default)        |
| emacs     | file text       | visit the file / show text in buffer `*pick*`           |
| focus     | window mark     | focus the window (default)                               |
| search    | text            | qutebrowser search, default engine                       |
| claude    | text file       | `claude-sel`, pick a prompt; result to clipboard (`PICK_CLAUDE_OUT=emacs\|paste`) |
| capture   | text url file   | append to today's denote journal                         |
| note      | text url file   | new denote note                                          |
| play      | file            | mpv                                                      |
| pass-copy | pass            | `pass -c`, clipmenud paused while copying (default)      |
| pass-type | pass            | type the password into the focused window                |

## Related

- `claude-sel` (`s-, c`): the Claude prompts in `~/.config/claude-sel/prompts`
  are also what the `claude` verb offers.
- Window marks: `s-'` (`marks.lisp`). The `marks` source shows the same
  marks.
- Clipboard history: `s-, y` opens clipmenu directly. The `clipboard`
  source is the same history, but its items go through the verb menu.
