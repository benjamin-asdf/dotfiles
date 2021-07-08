import dracula.draw

dracula.draw.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    }
})


# ambrevar
# c.auto_save.session = True
c.completion.shrink = True
c.confirm_quit = ["downloads"]
c.content.cache.size = 5242880
# c.downloads.location.directory = "~/temp"
c.downloads.location.prompt = False
c.downloads.location.remember = True
# c.editor.command = ["emacsclient", "+{line}:{column}", "{}"]
c.hints.scatter = False
# c.hints.uppercase = True
c.input.partial_timeout = 2000
# c.tabs.tabs_are_windows = True
# c.new_instance_open_target = "window"
# c.tabs.show = "multiple"
# c.window.title_format = "{title}{title_sep}{host}"
# lazy_restore does not work when idle.
# c.session.lazy_restore = True


config.bind('tb', 'config-cycle statusbar.hide true false')
config.bind("tt", 'config-cycle tabs.show never always')
config.bind('<Ctrl-Shift-j>', 'tab-next')
config.bind('<Ctrl-Shift-k>', 'tab-prev')
config.bind('J', 'scroll-page 0 1')
config.bind('K', 'scroll-page 0 -1')
config.bind('<Space><Space>', 'set-cmd-text :')

config.bind('<Ctrl-l>', 'mode-enter passthrough')
config.bind('<Ctrl-Space>', 'toggle-selection', mode='caret')

config.bind('<Escape>', 'mode-enter normal')
config.bind('<Ctrl-e>',':spawn ec --eval "(team-trello-card-dispatch \\"{url}\\")"', mode='normal')
config.bind('<Ctrl-v>',':spawn vlc {url}', mode='normal')
config.bind('<Ctrl-Escape>', 'mode-enter normal' , mode='passthrough')

config.bind('<Ctrl-p>', 'spawn --userscript qute-pass --dmenu-invocation dmenu --password-only', mode='insert')

c.tabs.position = "left"
c.tabs.max_width = 7
c.tabs.show = "never"

c.url.default_page = "https://searx.bar/"

c.url.searchengines = {
    'DEFAULT': 'https://searx.bar/?q={}',
    'duck'   : 'https://duckduckgo.com/?q={}',
    'wa'     : 'https://wiki.archlinux.org/?search={}',
    "so"     : "http://stackoverflow.com/search?q={}",
    "ex"     : "https://examine.com/search/?q={}",
    "leo"    : "http://dict.leo.org/frde/index_de.html#/search={}",
    "aur"    : "https://aur.archlinux.org/packages.php?O=0&K={}&do_Search=Go",
    "yt"     : "http://www.youtube.com/results?search_query={}",
    "goo"    : "https://www.google.com/search?q={}",
    'gg'     : 'https://g4gsearch.com/ws/search/search?a=true&c=%7B%7D&e=true&m&p=1&q={}&s=_score&w=%5B%5D',
    'wi'     : 'https://en.wikipedia.org/wiki/{}',
    'un'     : 'https://docs.unity3d.com/ScriptReference/30_search.html?q={}'
}



c.editor.command = ['ec', '{file}']

c.url.start_pages = [c.url.default_page, "https://trello.com/b/kABlCnCj/ben"]

config.load_autoconfig(False)
