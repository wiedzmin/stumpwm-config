(asdf:compute-source-registry)

(asdf:load-system :drakma)

(in-package #:stumpwm)

(defvar *search-browser-executable* nil
  "Browser to use while performing searches")
(defvar *search-browser-params* nil
  "Additional executable parameters for searching browser")

(defmacro make-search-engine-prompt (name caption url docstring)
  `(defcommand ,name (search)
     ((:string ,(concatenate 'string "Search in " caption " for: ")))
     ,docstring
     (when search
       (check-type search string)
       (let* ((search-processed (drakma:url-encode search :utf-8))
              (uri (format nil ,url search-processed)))
         (if (eql *search-browser-executable* nil)
             (message-no-timeout "stumpwm::*search-browser-executable* is nil, set it first")
             (run-shell-command
              (concatenate 'string
                           *search-browser-executable*
                           " " (format nil "~{~A~^ ~}" *search-browser-params*)
                           " \"" uri "\"")
              (,(intern (string-upcase *search-browser-executable*)))))))))

(defmacro make-search-engine-selection (name url docstring)
  `(defcommand ,name () ()
     ,docstring
     (let* ((search-processed (drakma:url-encode (get-x-selection) :utf-8))
           (uri (format nil ,url search-processed)))
       (if (eql *search-browser-executable* nil)
           (message-no-timeout "stumpwm::*search-browser-executable* is nil, set it first")
           (run-shell-command
            (concatenate 'string
                         *search-browser-executable*
                         " " (format nil "~{~A~^ ~}" *search-browser-params*)
                         " \"" uri "\"")
            (,(intern (string-upcase *search-browser-executable*))))))))

(defmacro make-search-engine-augmented (name caption url docstring)
  `(defcommand ,name (augmentation)
     ((:string ,(concatenate 'string "Augment " caption " search: ")))
     ,docstring
     (when augmentation
       (check-type augmentation string)
       (let* ((search-processed (concatenate
                                 'string
                                 (drakma:url-encode augmentation :utf-8) " "
                                 (drakma:url-encode (get-x-selection) :utf-8)))
              (uri (format nil ,url search-processed)))
         (if (eql *search-browser-executable* nil)
             (message-no-timeout "stumpwm::*search-browser-executable* is nil, set it first")
             (run-shell-command
              (concatenate 'string
                           *search-browser-executable*
                           " " (format nil "~{~A~^ ~}" *search-browser-params*)
                           " \"" uri "\"")
              (,(intern (string-upcase *search-browser-executable*)))))))))

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
(defparameter *URL-CRATE-IO*        "http://crate.io/?q=~a")
(defparameter *URL-RUTRACKER*       "http://rutracker.org/forum/tracker.php?nm=~a")
(defparameter *URL-ALTS-IO*         "http://alts.io/s?q=~a")
