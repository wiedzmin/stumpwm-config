;;;
;;; File: searches.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Суббота, Февраль  9 2013
;;;
;;;
;;;

(setf *search-browser-command* "firefox -new-tab")

(make-search-engine-selection search-google-selection *URL-GOOGLE* "Google search")
(make-search-engine-selection search-yandex-selection *URL-YANDEX* "Yandex search")
(make-search-engine-selection search-hackage-selection *URL-HACKAGE* "Hackage search")
(make-search-engine-selection search-hoogle-selection *URL-HOOGLE* "Hoogle search")
(make-search-engine-selection search-googlemaps-selection *URL-GOOGLE-MAPS* "Google Maps search")
(make-search-engine-selection search-openstreetmap-selection *URL-OPENSTREETMAP* "Openstreetmap search")
(make-search-engine-selection search-wikipedia-en-selection *URL-WIKIPEDIA-EN* "Wikipedia English search")
(make-search-engine-selection search-wiktionary-en-selection *URL-WIKTIONARY-EN* "Wiktionary English search")
(make-search-engine-selection search-youtube-selection *URL-YOUTUBE* "Youtube search")
(make-search-engine-selection search-wayback-selection *URL-WAYBACK* "Wayback search")
(make-search-engine-selection search-emacswiki-selection *URL-EMACSWIKI* "EmacsWiki search")
(make-search-engine-selection search-vimscript-selection *URL-VIMSCRIPT* "Vim scripts search")
(make-search-engine-selection search-github-selection *URL-GITHUB* "Github search")
(make-search-engine-selection search-github-code-selection *URL-GITHUB-CODE* "Github code search")
(make-search-engine-selection search-google-emacs-selection *URL-GOOGLE-EMACS* "Google Emacs search")
(make-search-engine-selection search-firefox-addons-selection *URL-FIREFOX-ADDONS* "Firefox addons search")
(make-search-engine-selection search-hpaste-selection *URL-HPASTE* "HPaste search")
(make-search-engine-selection search-hayoo-selection *URL-HAYOO* "Hayoo search")
(make-search-engine-selection search-pypi-selection *URL-PYPI* "PyPI search")
(make-search-engine-selection search-lingvo-selection *URL-LINGVO* "Lingvo search")
(make-search-engine-selection search-multitran-selection *URL-MULTITRAN* "Multitran search")
(make-search-engine-selection search-yandex-maps-selection *URL-YANDEX-MAPS* "Yandex Maps search")
(make-search-engine-selection search-zugaina-selection *URL-ZUGAINA* "Zugaina search")
(make-search-engine-selection search-google-translate-selection *URL-GOOGLE-TRANSLATE* "Google Translate search")
(make-search-engine-selection search-wikipedia-ru-selection *URL-WIKIPEDIA-RU* "Wikipedia Russian search")
(make-search-engine-selection search-piratebay-selection *URL-PIRATEBAY* "PirateBay search")
(make-search-engine-selection search-ohloh-selection *URL-OHLOH-CODE* "Ohloh code search")
(make-search-engine-selection search-libgen-scimag-selection *URL-LIBGEN-SCIMAG* "Libgen scientific")

(make-search-engine-prompt search-google-prompt "Google" *URL-GOOGLE* "Google search")
(make-search-engine-prompt search-yandex-prompt "Yandex" *URL-YANDEX* "Yandex search")
(make-search-engine-prompt search-hackage-prompt "Hackage" *URL-HACKAGE* "Hackage search")
(make-search-engine-prompt search-hoogle-prompt "Hoogle" *URL-HOOGLE* "Hoogle search")
(make-search-engine-prompt search-googlemaps-prompt "Google Maps" *URL-GOOGLE-MAPS* "Google Maps search")
(make-search-engine-prompt search-openstreetmap-prompt "Openstreetmap" *URL-OPENSTREETMAP* "Openstreetmap search")
(make-search-engine-prompt search-wikipedia-en-prompt "English Wikipedia" *URL-WIKIPEDIA-EN* "Wikipedia English search")
(make-search-engine-prompt search-wiktionary-en-prompt "English Wiktionary" *URL-WIKTIONARY-EN* "Wiktionary English search")
(make-search-engine-prompt search-youtube-prompt "Youtube" *URL-YOUTUBE* "Youtube search")
(make-search-engine-prompt search-wayback-prompt "Wayback" *URL-WAYBACK* "Wayback search")
(make-search-engine-prompt search-emacswiki-prompt "EmacsWiki" *URL-EMACSWIKI* "EmacsWiki search")
(make-search-engine-prompt search-vimscript-prompt "Vim scripts" *URL-VIMSCRIPT* "Vim scripts search")
(make-search-engine-prompt search-github-prompt "Github" *URL-GITHUB* "Github search")
(make-search-engine-prompt search-github-code-prompt "Github code" *URL-GITHUB-CODE* "Github code search")
(make-search-engine-prompt search-google-emacs-prompt "Google with Emacs" *URL-GOOGLE-EMACS* "Google Emacs search")
(make-search-engine-prompt search-firefox-addons-prompt "Firefox addons" *URL-FIREFOX-ADDONS* "Firefox addons search")
(make-search-engine-prompt search-hpaste-prompt "HPaste" *URL-HPASTE* "HPaste search")
(make-search-engine-prompt search-hayoo-prompt "Hayoo" *URL-HAYOO* "Hayoo search")
(make-search-engine-prompt search-pypi-prompt "PyPI" *URL-PYPI* "PyPI search")
(make-search-engine-prompt search-lingvo-prompt "Lingvo" *URL-LINGVO* "Lingvo search")
(make-search-engine-prompt search-multitran-prompt "Multitran" *URL-MULTITRAN* "Multitran search")
(make-search-engine-prompt search-yandex-maps-prompt "Yandex Maps" *URL-YANDEX-MAPS* "Yandex Maps search")
(make-search-engine-prompt search-zugaina-prompt "Zugaina" *URL-ZUGAINA* "Zugaina search")
(make-search-engine-prompt search-google-translate-prompt "Google Translate" *URL-GOOGLE-TRANSLATE* "Google Translate search")
(make-search-engine-prompt search-wikipedia-ru-prompt "Russian Wikipedia" *URL-WIKIPEDIA-RU* "Wikipedia Russian search")
(make-search-engine-prompt search-piratebay-prompt "PirateBay" *URL-PIRATEBAY* "PirateBay search")
(make-search-engine-prompt search-ohloh-prompt "Ohloh" *URL-OHLOH-CODE* "Ohloh code search")
(make-search-engine-prompt search-libgen-scimag-prompt "Libgen scientific" *URL-LIBGEN-SCIMAG* "Libgen scientific search")
