# ambrevar
c.auto_save.session = True
c.completion.shrink = True
c.confirm_quit = ["downloads"]
c.content.cache.size = 5242880
c.downloads.location.directory = "~/tmp"
c.downloads.location.prompt = False
c.downloads.location.remember = True
c.hints.scatter = False
c.hints.uppercase = True
c.input.partial_timeout = 2000
c.tabs.tabs_are_windows = True
c.new_instance_open_target = "window"
c.tabs.show = "multiple"
c.colors.webpage.preferred_color_scheme = "dark"

c.content.cookies.accept = 'all'
c.content.cookies.store = True
c.content.javascript.enabled = True
c.content.javascript.clipboard = 'access-paste'

# lazy_restore does not work when idle.
c.session.lazy_restore = True

c.content.blocking.method = "both"

config.bind('ye', ':spawn emacsclient --eval "(kill-new \\"{url}\\")"', mode='normal')
config.bind(',r', ':spawn emacsclient --eval "(github-pull-readme \\"{url}\\")"', mode='normal')
config.bind(',l', ':spawn emacsclient "org-protocol://store-link?url={url}" ', mode='normal')
config.bind(',c', ':spawn emacsclient "org-protocol://capture?url={url}" ', mode='normal')

config.bind('tb', 'config-cycle statusbar.show always never')
config.bind("tt", 'config-cycle tabs.show never always')
config.bind('<Ctrl-Shift-j>', 'tab-next')
config.bind('<Ctrl-Shift-k>', 'tab-prev')
config.bind('J', 'scroll-page 0 1')
config.bind('K', 'scroll-page 0 -1')
config.bind('<Space><Space>', 'set-cmd-text :')

config.bind("u", 'undo --window')
config.bind(',,', 'mode-enter passthrough', mode='normal')
config.bind(',,', 'mode-enter normal', mode='passthrough')
config.bind('<Ctrl-Space>', 'toggle-selection', mode='caret')

config.bind('<Escape>', 'mode-enter normal')
# config.bind(',v',':spawn vlc {url}', mode='normal')
config.bind(',v', ':spawn mpv {url}', mode='normal')
config.bind(',g', ':spawn google-chrome-stable {url}', mode='normal')
config.bind(',u', 'spawn --userscript edit_url_with_emacs')

config.bind('<Ctrl-Escape>', 'mode-enter normal' , mode='passthrough')

# config.bind(',p', 'spawn --userscript qute-pass --dmenu-invocation dmenu --password-only', mode='insert')

c.tabs.position = "left"
c.tabs.max_width = 7
c.tabs.show = "never"

c.url.default_page = "https://search.brave.com/"

c.url.searchengines = {
    'DEFAULT': "https://search.brave.com/search?q={}",
    'duck'   : 'https://duckduckgo.com/?q={}',
    'wa'     : 'https://wiki.archlinux.org/?search={}',
    "so"     : "http://stackoverflow.com/search?q={}",
    "ex"     : "https://examine.com/search/?q={}",
    "leo"    : "http://dict.leo.org/frde/index_de.html#/search={}",
    "aur"    : "https://aur.archlinux.org/packages?O=0&K={}",
    "yt"     : "http://www.youtube.com/results?search_query={}",
    "goo"    : "https://www.google.com/search?q={}",
    'gg'     : 'https://g4gsearch.com/ws/search/search?a=true&c=%7B%7D&e=true&m&p=1&q={}&s=_score&w=%5B%5D',
    'wi'     : 'https://en.wikipedia.org/wiki/{}',
    "clja"   : 'https://clojars.org/search?q={}'
}


import os
import os.path

# work config is not in a public git repo

work_config_path = os.path.join(os.environ.get('XDG_CONFIG_HOME', os.path.expanduser('~/.config')), 'qutebrowser', 'work_config.py')

if os.path.exists(work_config_path):
    exec(open(work_config_path).read())

c.editor.command = ['emacsclient', "-c", "--eval", "(mm/edit-with-editor \"{file}\")" ]

# Font
c.fonts.default_size = "14pt"

# Mememacs color scheme (from visual.el)
bg = "black"
fg = "#faf7f7"                # hint-of-red
heliotrope = "#F689FF"
brown = "#543f2f"
mint_green = "#96fe8f"
fruit_salad = "#479b59"
hit_pink = "#feb48f"
woodsmoke_tint = "#2e2d35"
anakiwa = "#8fcefe"
horison = "#5F89A9"
sweet_pink = "#fe9aa1"

# Completion
c.colors.completion.fg = fg
c.colors.completion.odd.bg = bg
c.colors.completion.even.bg = bg
c.colors.completion.category.fg = hit_pink
c.colors.completion.category.bg = bg
c.colors.completion.category.border.top = brown
c.colors.completion.category.border.bottom = brown
c.colors.completion.item.selected.fg = heliotrope
c.colors.completion.item.selected.bg = bg
c.colors.completion.item.selected.border.top = heliotrope
c.colors.completion.item.selected.border.bottom = heliotrope
c.colors.completion.item.selected.match.fg = horison
c.colors.completion.match.fg = horison
c.colors.completion.scrollbar.fg = heliotrope
c.colors.completion.scrollbar.bg = bg

# Context menu
c.colors.contextmenu.menu.bg = bg
c.colors.contextmenu.menu.fg = fg
c.colors.contextmenu.selected.bg = bg
c.colors.contextmenu.selected.fg = heliotrope

# Downloads
c.colors.downloads.bar.bg = bg
c.colors.downloads.start.fg = bg
c.colors.downloads.start.bg = anakiwa
c.colors.downloads.stop.fg = bg
c.colors.downloads.stop.bg = mint_green
c.colors.downloads.error.fg = sweet_pink

# Hints
c.colors.hints.fg = fg              # hint letters: hint-of-red
c.colors.hints.bg = bg              # background: black
c.colors.hints.match.fg = heliotrope # already typed: heliotrope
c.hints.border = f"1px solid {heliotrope}"

# Keyhint
c.colors.keyhint.fg = fg
c.colors.keyhint.suffix.fg = heliotrope
c.colors.keyhint.bg = bg

# Messages
c.colors.messages.error.fg = fg
c.colors.messages.error.bg = "#8b0000"
c.colors.messages.error.border = "#8b0000"
c.colors.messages.warning.fg = bg
c.colors.messages.warning.bg = hit_pink
c.colors.messages.warning.border = hit_pink
c.colors.messages.info.fg = fg
c.colors.messages.info.bg = bg
c.colors.messages.info.border = brown

# Prompts
c.colors.prompts.fg = fg
c.colors.prompts.bg = bg
c.colors.prompts.border = brown
c.colors.prompts.selected.fg = heliotrope
c.colors.prompts.selected.bg = bg

# Statusbar
c.colors.statusbar.normal.fg = fg
c.colors.statusbar.normal.bg = bg
c.colors.statusbar.insert.fg = fg
c.colors.statusbar.insert.bg = bg
c.colors.statusbar.passthrough.fg = fg
c.colors.statusbar.passthrough.bg = bg
c.colors.statusbar.private.fg = fg
c.colors.statusbar.private.bg = woodsmoke_tint
c.colors.statusbar.command.fg = fg
c.colors.statusbar.command.bg = bg
c.colors.statusbar.caret.fg = bg
c.colors.statusbar.caret.bg = heliotrope
c.colors.statusbar.caret.selection.fg = bg
c.colors.statusbar.caret.selection.bg = heliotrope
c.colors.statusbar.progress.bg = mint_green
c.colors.statusbar.url.fg = fg
c.colors.statusbar.url.error.fg = sweet_pink
c.colors.statusbar.url.hover.fg = anakiwa
c.colors.statusbar.url.success.http.fg = fg
c.colors.statusbar.url.success.https.fg = mint_green
c.colors.statusbar.url.warn.fg = hit_pink

# Tabs
c.colors.tabs.bar.bg = bg
c.colors.tabs.indicator.start = anakiwa
c.colors.tabs.indicator.stop = mint_green
c.colors.tabs.indicator.error = sweet_pink
c.colors.tabs.odd.fg = fg
c.colors.tabs.odd.bg = bg
c.colors.tabs.even.fg = fg
c.colors.tabs.even.bg = bg
c.colors.tabs.selected.odd.fg = heliotrope
c.colors.tabs.selected.odd.bg = bg
c.colors.tabs.selected.even.fg = heliotrope
c.colors.tabs.selected.even.bg = bg
c.colors.tabs.pinned.odd.fg = fg
c.colors.tabs.pinned.odd.bg = woodsmoke_tint
c.colors.tabs.pinned.even.fg = fg
c.colors.tabs.pinned.even.bg = woodsmoke_tint
c.colors.tabs.pinned.selected.odd.fg = heliotrope
c.colors.tabs.pinned.selected.odd.bg = bg
c.colors.tabs.pinned.selected.even.fg = heliotrope
c.colors.tabs.pinned.selected.even.bg = bg

config.load_autoconfig(False)
