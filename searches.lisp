;;;
;;; File: searches.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Суббота, Февраль  9 2013
;;;
;;;
;;;

(in-package #:stumpwm)

(setf searchengines:*search-browser-executable* (browser-executable (psetup-default-browser *persistent-setup*)))
(setf searchengines:*search-browser-params* (browser-cliargs (psetup-default-browser *persistent-setup*)))

(defparameter *URL-AMAZON*          "http://www.amazon.com/exec/obidos/external-search?index=all&keyword=~a")
(defparameter *URL-ALPHA*           "http://www.wolframalpha.com/input/?i=~a")
(defparameter *URL-DEB*             "http://packages.debian.org/~a")
(defparameter *URL-DEBBTS*          "http://bugs.debian.org/~a")
(defparameter *URL-DEBPTS*          "http://packages.qa.debian.org/~a")
(defparameter *URL-DICTIONARY*      "http://dictionary.reference.com/browse/~a")
(defparameter *URL-GOOGLE*          "http://www.google.com/search?num=100&q=~a")
(defparameter *URL-HACKAGE*         "http://hackage.haskell.org/package/~a")
(defparameter *URL-HOOGLE*          "http://www.haskell.org/hoogle/?q=~a")
(defparameter *URL-IMAGES*          "http://images.google.fr/images?q=~a")
(defparameter *URL-IMDB*            "http://www.imdb.com/find?s=all&q=~a")
(defparameter *URL-ISOHUNT*         "http://isohunt.com/torrents/?ihq=~a")
(defparameter *URL-LUCKY*           "http://www.google.com/search?btnI&q=~a")
(defparameter *URL-GOOGLE-MAPS*     "http://maps.google.com/maps?q=~a")
(defparameter *URL-MATHWORLD*       "http://mathworld.wolfram.com/search/?query=~a")
(defparameter *URL-OPENSTREETMAP*   "http://gazetteer.openstreetmap.org/namefinder/?find=~a")
(defparameter *URL-SCHOLAR*         "http://scholar.google.com/scholar?q=~a")
(defparameter *URL-THESAURUS*       "http://thesaurus.reference.com/search?q=~a")
(defparameter *URL-WIKIPEDIA-EN*    "http://en.wikipedia.org/wiki/Special:Search?go=Go&search=~a")
(defparameter *URL-WIKTIONARY-EN*   "http://en.wiktionary.org/wiki/Special:Search?go=Go&search=~a")
(defparameter *URL-YOUTUBE*         "http://www.youtube.com/results?search_type=search_videos&search_query=~a")
(defparameter *URL-WAYBACK*         "http://web.archive.org/web/*/~a")
(defparameter *URL-EMACSWIKI*       "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&sa=Search&siteurl=www.emacswiki.org%2F&q=~a")
(defparameter *URL-VIMSCRIPT*       "http://www.vim.org/scripts/script_search_results.php?order_by=rating&direction=descending&search=search&keywords=~a")
(defparameter *URL-DELICIOUS*       "http://del.icio.us/tag/~a")
(defparameter *URL-GITHUB*          "http://github.com/search?type=Everything&q=~a")
(defparameter *URL-GITHUB-CODE*     "http://github.com/search?type=Code&q=~a")
(defparameter *URL-FREEBSD-PORTS*   "http://www.freebsd.org/cgi/ports.cgi?stype=name&query=~a")
(defparameter *URL-PROSTOPLEER*     "http://prostopleer.com/#/search?q=~a")
(defparameter *URL-GOOGLE-EMACS*    "http://www.google.ru/?q=emacs+~a")
(defparameter *URL-FIREFOX-ADDONS*  "http://addons.mozilla.org/en-US/firefox/search/?cat=all&x=0&y=0&q=~a")
(defparameter *URL-GMANE-BEG*       "http://search.gmane.org/?group=gmane.comp.lang.haskell.beginners&query=~a")
(defparameter *URL-GMANE-CAFE*      "http://search.gmane.org/?group=gmane.comp.lang.haskell.cafe&query=~a")
(defparameter *URL-HPASTE*          "http://hpaste.org/fastcgi/hpaste.fcgi/?search=~a")
(defparameter *URL-GMANE-SUP*       "http://search.gmane.org/?group=gmane.mail.sup.general&query=~a")
(defparameter *URL-HAYOO*           "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=~a")
(defparameter *URL-PYPI*            "http://pypi.python.org/pypi?%3Aaction=search&submit=search&term=~a")
(defparameter *URL-LINGVO*          "http://lingvopro.abbyyonline.com/ru/Search/en-ru/~a")
(defparameter *URL-MULTITRAN*       "http://www.multitran.ru/c/M.exe?CL=1&l1=1&s=~a")
(defparameter *URL-YANDEX-MAPS*     "http://maps.yandex.ru/?text=~a")
(defparameter *URL-DEBIAN-PACKAGES* "http://packages.debian.org/search?searchon=names&keywords=~a")
(defparameter *URL-ZUGAINA*         "http://gpo.zugaina.org/Search?search=~a")
(defparameter *URL-GOOGLE-TRANSLATE* "http://translate.google.ru/#en|ru|~a")
(defparameter *URL-WIKIPEDIA-RU*    "http://ru.wikipedia.org/wiki/Special:Search?go=Go&search=~a")
(defparameter *URL-BUGSALL-XM*      "http://code.google.com/p/xmonad/issues/list?can=1&q=~a")
(defparameter *URL-BUGSNEW-XM*      "http://code.google.com/p/xmonad/issues/list?can=6&q=~a")
(defparameter *URL-BUGSOPEN-XM*     "http://code.google.com/p/xmonad/issues/list?can=2&q=~a")
(defparameter *URL-GMANE-XM*        "http://search.gmane.org/?group=gmane.comp.lang.haskell.xmonad&query=~a")
(defparameter *URL-YANDEX*          "http://yandex.ru/yandsearch?text=~a")
(defparameter *URL-PIRATEBAY*       "http://thepiratebay.org/search/~a")
(defparameter *URL-CANIUSE*         "http://caniuse.com/#search=~a")
(defparameter *URL-OHLOH-CODE*      "http://code.ohloh.net/search?s=~a")
(defparameter *URL-LIBGEN-SCIMAG*   "http://libgen.org/scimag/?s=~a")
(defparameter *URL-RUTRACKER*       "http://rutracker.org/forum/tracker.php?nm=~a")
(defparameter *URL-ALTS-IO*         "http://alts.io/s?q=~a")
(defparameter *URL-LAUNCHPAD*       "https://launchpad.net/+search?field.text=~a")
(defparameter *URL-EMACS-BUGS*      "http://debbugs.gnu.org/cgi/search.cgi?search=search&skip=0&phrase=~a")
(defparameter *URL-SEARCHCODE*      "https://searchcode.com/?q=~a")
(defparameter *URL-DOCKER*          "https://hub.docker.com/search/?isAutomated=0&isOfficial=0&page=1&pullCount=0&starCount=0&q=~a")

(searchengines:make-searchengine-selection "search-google-selection" *URL-GOOGLE* "Google search" :map *search-keymap* :key "s-g")
(searchengines:make-searchengine-selection "search-yandex-selection" *URL-YANDEX* "Yandex search" :map *search-keymap* :key "s-y")
(searchengines:make-searchengine-selection "search-hackage-selection" *URL-HACKAGE* "Hackage search")
(searchengines:make-searchengine-selection "search-hoogle-selection" *URL-HOOGLE* "Hoogle search")
(searchengines:make-searchengine-selection "search-googlemaps-selection" *URL-GOOGLE-MAPS* "Google Maps search")
(searchengines:make-searchengine-selection "search-openstreetmap-selection" *URL-OPENSTREETMAP* "Openstreetmap search")
(searchengines:make-searchengine-selection "search-wikipedia-en-selection" *URL-WIKIPEDIA-EN* "Wikipedia English search")
(searchengines:make-searchengine-selection "search-wiktionary-en-selection" *URL-WIKTIONARY-EN* "Wiktionary English search")
(searchengines:make-searchengine-selection "search-youtube-selection" *URL-YOUTUBE* "Youtube search")
(searchengines:make-searchengine-selection "search-wayback-selection" *URL-WAYBACK* "Wayback search")
(searchengines:make-searchengine-selection "search-emacswiki-selection" *URL-EMACSWIKI* "EmacsWiki search")
(searchengines:make-searchengine-selection "search-vimscript-selection" *URL-VIMSCRIPT* "Vim scripts search" :map *search-keymap* :key "s-v")
(searchengines:make-searchengine-selection "search-github-selection" *URL-GITHUB* "Github search" :map *search-keymap* :key "s-h")
(searchengines:make-searchengine-selection "search-github-code-selection" *URL-GITHUB-CODE* "Github code search")
(searchengines:make-searchengine-selection "search-google-emacs-selection" *URL-GOOGLE-EMACS* "Google Emacs search")
(searchengines:make-searchengine-selection "search-firefox-addons-selection" *URL-FIREFOX-ADDONS* "Firefox addons search")
(searchengines:make-searchengine-selection "search-hpaste-selection" *URL-HPASTE* "HPaste search")
(searchengines:make-searchengine-selection "search-hayoo-selection" *URL-HAYOO* "Hayoo search")
(searchengines:make-searchengine-selection "search-pypi-selection" *URL-PYPI* "PyPI search" :map *search-keymap* :key "s-p")
(searchengines:make-searchengine-selection "search-lingvo-selection" *URL-LINGVO* "Lingvo search")
(searchengines:make-searchengine-selection "search-multitran-selection" *URL-MULTITRAN* "Multitran search" :map *search-keymap* :key "s-t")
(searchengines:make-searchengine-selection "search-yandex-maps-selection" *URL-YANDEX-MAPS* "Yandex Maps search" :map *search-keymap* :key "s-m")
(searchengines:make-searchengine-selection "search-zugaina-selection" *URL-ZUGAINA* "Zugaina search" :map *search-keymap* :key "s-z")
(searchengines:make-searchengine-selection "search-google-translate-selection" *URL-GOOGLE-TRANSLATE* "Google Translate search")
(searchengines:make-searchengine-selection "search-wikipedia-ru-selection" *URL-WIKIPEDIA-RU* "Wikipedia Russian search")
(searchengines:make-searchengine-selection "search-piratebay-selection" *URL-PIRATEBAY* "PirateBay search")
(searchengines:make-searchengine-selection "search-ohloh-selection" *URL-OHLOH-CODE* "Ohloh code search" :map *search-keymap* :key "s-o")
(searchengines:make-searchengine-selection "search-libgen-scimag-selection" *URL-LIBGEN-SCIMAG* "Libgen scientific" :map *search-keymap* :key "s-s")
(searchengines:make-searchengine-selection "search-rutracker-selection" *URL-RUTRACKER* "Rutracker torrents" :map *search-keymap* :key "s-r")
(searchengines:make-searchengine-selection "search-alts-io-selection" *URL-ALTS-IO* "Python reference" :map *search-keymap* :key "s-a")
(searchengines:make-searchengine-selection "search-launchpad-selection" *URL-LAUNCHPAD* "Search on Launchpad" :map *search-keymap* :key "s-l")
(searchengines:make-searchengine-selection "search-emacs-bugs-selection" *URL-EMACS-BUGS* "Search for Emacs bugs")
(searchengines:make-searchengine-selection "search-searchcode-selection" *URL-SEARCHCODE* "Search for CODE" :map *search-keymap* :key "s-c")
(searchengines:make-searchengine-selection "search-docker-selection" *URL-DOCKER* "Search for CODE" :map *search-keymap* :key "s-d")
(searchengines:make-searchengine-selection "search-open-selection" "" "Just open selection"  :map *search-keymap* :key "s-o")

(searchengines:make-searchengine-prompt "search-google-prompt" "Google" *URL-GOOGLE* "Google search" :map *search-keymap* :key "s-C-g")
(searchengines:make-searchengine-prompt "search-yandex-prompt" "Yandex" *URL-YANDEX* "Yandex search" :map *search-keymap* :key "s-C-y")
(searchengines:make-searchengine-prompt "search-hackage-prompt" "Hackage" *URL-HACKAGE* "Hackage search")
(searchengines:make-searchengine-prompt "search-hoogle-prompt" "Hoogle" *URL-HOOGLE* "Hoogle search")
(searchengines:make-searchengine-prompt "search-googlemaps-prompt" "Google Maps" *URL-GOOGLE-MAPS* "Google Maps search")
(searchengines:make-searchengine-prompt "search-openstreetmap-prompt" "Openstreetmap" *URL-OPENSTREETMAP* "Openstreetmap search")
(searchengines:make-searchengine-prompt "search-wikipedia-en-prompt" "English Wikipedia" *URL-WIKIPEDIA-EN* "Wikipedia English search")
(searchengines:make-searchengine-prompt "search-wiktionary-en-prompt" "English Wiktionary" *URL-WIKTIONARY-EN* "Wiktionary English search")
(searchengines:make-searchengine-prompt "search-youtube-prompt" "Youtube" *URL-YOUTUBE* "Youtube search")
(searchengines:make-searchengine-prompt "search-wayback-prompt" "Wayback" *URL-WAYBACK* "Wayback search")
(searchengines:make-searchengine-prompt "search-emacswiki-prompt" "EmacsWiki" *URL-EMACSWIKI* "EmacsWiki search")
(searchengines:make-searchengine-prompt "search-vimscript-prompt" "Vim scripts" *URL-VIMSCRIPT* "Vim scripts search" :map *search-keymap* :key "s-C-v")
(searchengines:make-searchengine-prompt "search-github-prompt" "Github" *URL-GITHUB* "Github search")
(searchengines:make-searchengine-prompt "search-github-code-prompt" "Github code" *URL-GITHUB-CODE* "Github code search")
(searchengines:make-searchengine-prompt "search-google-emacs-prompt" "Google with Emacs" *URL-GOOGLE-EMACS* "Google Emacs search")
(searchengines:make-searchengine-prompt "search-firefox-addons-prompt" "Firefox addons" *URL-FIREFOX-ADDONS* "Firefox addons search")
(searchengines:make-searchengine-prompt "search-hpaste-prompt" "HPaste" *URL-HPASTE* "HPaste search")
(searchengines:make-searchengine-prompt "search-hayoo-prompt" "Hayoo" *URL-HAYOO* "Hayoo search")
(searchengines:make-searchengine-prompt "search-pypi-prompt" "PyPI" *URL-PYPI* "PyPI search" :map *search-keymap* :key "s-C-p")
(searchengines:make-searchengine-prompt "search-lingvo-prompt" "Lingvo" *URL-LINGVO* "Lingvo search")
(searchengines:make-searchengine-prompt "search-multitran-prompt" "Multitran" *URL-MULTITRAN* "Multitran search" :map *search-keymap* :key "s-C-t")
(searchengines:make-searchengine-prompt "search-yandex-maps-prompt" "Yandex Maps" *URL-YANDEX-MAPS* "Yandex Maps search")
(searchengines:make-searchengine-prompt "search-zugaina-prompt" "Zugaina" *URL-ZUGAINA* "Zugaina search" :map *search-keymap* :key "s-C-z")
(searchengines:make-searchengine-prompt "search-google-translate-prompt" "Google Translate" *URL-GOOGLE-TRANSLATE* "Google Translate search")
(searchengines:make-searchengine-prompt "search-wikipedia-ru-prompt" "Russian Wikipedia" *URL-WIKIPEDIA-RU* "Wikipedia Russian search")
(searchengines:make-searchengine-prompt "search-piratebay-prompt" "PirateBay" *URL-PIRATEBAY* "PirateBay search")
(searchengines:make-searchengine-prompt "search-ohloh-prompt" "Ohloh" *URL-OHLOH-CODE* "Ohloh code search" :map *search-keymap* :key "s-C-o")
(searchengines:make-searchengine-prompt "search-libgen-scimag-prompt" "Libgen scientific" *URL-LIBGEN-SCIMAG* "Libgen scientific search" :map *search-keymap* :key "s-C-s")
(searchengines:make-searchengine-prompt "search-alts-io-prompt" "Python reference" *URL-ALTS-IO* "Python reference" :map *search-keymap* :key "s-C-a")
(searchengines:make-searchengine-prompt "search-rutracker-prompt" "Rutracker" *URL-RUTRACKER* "Rutracker torrents" :map *search-keymap* :key "s-C-r")
(searchengines:make-searchengine-prompt "search-launchpad-prompt" "Launchpad" *URL-LAUNCHPAD* "Search on Launchpad" :map *search-keymap* :key "s-C-l")
(searchengines:make-searchengine-prompt "search-emacs-bugs-prompt" "Emacs bugs" *URL-EMACS-BUGS* "Search for Emacs bugs")
(searchengines:make-searchengine-prompt "search-searchcode-prompt" "SearchCode" *URL-SEARCHCODE* "Search for CODE" :map *search-keymap* :key "s-C-c")
(searchengines:make-searchengine-prompt "search-docker-prompt" "Docker" *URL-DOCKER* "Search for Docker images" :map *search-keymap* :key "s-C-d")

(searchengines:make-searchengine-augmented "search-google-augmented" "Augmented Google" *URL-GOOGLE* "Google search" :map *search-keymap* :key "C-g")
