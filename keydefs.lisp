;;;
;;; File: keydefs.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Среда, Февраль  6 2013
;;;
;;;
;;;

(set-prefix-key (kbd "s-d"))

(define-keysym #x1008ff11 "XF86AudioLowerVolume")
(define-keysym #x1008ff12 "XF86AudioMute")
(define-keysym #x1008ff13 "XF86AudioRaiseVolume")

(defparameter *search-keymap* (build-keymap
                               ("s-g"  "search-google-selection")
                               ("s-y"  "search-yandex-selection")
                               ("s-t"  "search-multitran-selection")
                               ("s-v"  "search-vimscript-selection")
                               ("s-o"  "search-ohloh-selection")
                               ("s-m"  "search-yandex-maps-selection")
                               ("s-h"  "search-github-selection")
                               ("s-h"  "search-github-selection")
                               ("s-p"  "search-pypi-selection")
                               ("s-s"  "search-libgen-scimag-selection")
                               ("s-C-g"  "search-google-prompt")
                               ("s-C-y"  "search-yandex-prompt")
                               ("s-C-t"  "search-multitran-prompt")
                               ("s-C-v"  "search-vimscript-prompt")
                               ("s-C-o"  "search-ohloh-prompt")
                               ("s-C-p"  "search-pypi-prompt")
                               ("s-C-s"  "search-libgen-scimag-prompt")
                               ))

(defparameter *pull-keymap* (build-keymap
                               ("c"  "p-conkeror")
                               ("e"  "p-emacs")
                               ("u"  "p-urxvt")
                               ("f"  "p-firefox")
                               ("g"  "p-google-chrome")
                               ("v"  "p-virtualbox")
                               ("q"  "p-qmpdclient")
                               ("k"  "p-krusader")
                               ("l"  "p-vlc")
                               ))

(defparameter *raise-keymap* (build-keymap
                               ("c"  "conkeror")
                               ("e"  "emacs")
                               ("u"  "urxvt")
                               ("f"  "firefox")
                               ("g"  "google-chrome")
                               ("v"  "virtualbox")
                               ("q"  "qmpdclient")
                               ("k"  "krusader")
                               ("l"  "vlc")
                               ))

(defparameter *heads-keymap* (build-keymap
                               ("Right"  "enable-external-monitor-right")
                               ("Left"  "enable-external-monitor-left")
                               ("Up"  "enable-external-monitor-above")
                               ("Down"  "disable-external-monitor")
                               ))

(defparameter *swap-keymap* (build-keymap
                               ("Right"  "exchange-direction right")
                               ("Left"  "exchange-direction left")
                               ("Up"  "exchange-direction up")
                               ("Down"  "exchange-direction down")
                               ))

(defparameter *desktop-keymap* (build-keymap
                                ("a"  "update-all-modelines")
                                ("d"  "custom/dump-group")
                                ("m"  "custom/restore-group-multihead")
                               ))

(defkeys-root
  ("0" "remove-split")
  ("1" "only")
  ("2" "vsplit")
  ("3" "hsplit")
  ("Delete" "toggle-modeline")
  ("ESC" "abort")
  ("Q" "restart-hard") ;was "quit"
  ("\\" "balance-frames")
  ("p" "global-pull-windowlist")
  ("s-Down" "move-window down")
  ("s-Left" "move-window left")
  ("s-Right" "move-window right")
  ("s-Up" "move-window up")
  ("s-b" "mode-line")
  ("s-c" "screenshot-window-default")
  ("s-i" "iresize")
  ("s-l" "lock-screen")
  ("s-m" "screenshot-default")
  ("v" "view-gmail")
  ("w" "global-windowlist")
  )

(when (and (boundp '*xmonad-style-groups*) *xmonad-style-groups*)
  (defkeys-top
    ("s-1" "gselect 1")
    ("s-2" "gselect 2")
    ("s-3" "gselect 3")
    ("s-4" "gselect 4")
    ("s-5" "gselect 5")
    ("s-6" "gselect 6")
    ("s-7" "gselect 7")
    ("s-8" "gselect 8")
    ("s-9" "gselect 9")
    ("s-C-w" "gmove")
    ))

(defkeys-top
  ("s-P" "exec gmrun")
  ("s-p" "exec dmenu_run")
  ("s-L" "update-mode-line")
  ("s-C" "kill-window")
  ;;
  ("XF86AudioRaiseVolume" "raise-volume")
  ("XF86AudioLowerVolume" "lower-volume")
  ;;
  ("s-TAB" "next")
  ("s-C-TAB" "prev")
  ;;
  ("s-C-z" "exec mpc prev")
  ("s-C-x" "exec mpc play")
  ("s-C-c" "exec mpc toggle")
  ("s-C-v" "exec mpc stop")
  ("s-C-b" "exec mpc next")
  ;;
  ("s-Right"   "move-focus right")
  ("s-Left"    "move-focus left" )
  ("s-Up"      "move-focus up"   )
  ("s-Down"    "move-focus down" )
  ;;
  ("C-\\"    "switch-window-layout")
  ("s-e"     "raise-urgent")
  ("s-f"     "fullscreen")
  ;;
  ("s-/"       '*search-keymap*)
  ("s-x"       '*pull-keymap*)
  ("s-r"       '*raise-keymap*)
  ("s-v"       '*heads-keymap*)
  ("s-s"       '*swap-keymap*)
  ("s-p"       '*desktop-keymap*)
  )

(flet ((dk (k c)
        (define-key *top-map* k (format nil c *simple-resize-increment*))))
 (dk (kbd "s-S-Up") "resize 0 -~D")
 (dk (kbd "s-S-Down") "resize 0 ~D")
 (dk (kbd "s-S-Left") "resize -~D 0")
 (dk (kbd "s-S-Right") "resize ~D 0"))
