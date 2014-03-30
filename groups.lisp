(clear-window-placement-rules)

(if (and (boundp '*xmonad-style-groups*) *xmonad-style-groups*)
    (progn
      (run-commands "gnewbg web" "gnewbg im" "gnewbg work" "gnewbg read"
                    "gnewbg term" "gnewbg mm" "gnewbg file" "gnewbg virt")
      (define-frame-preference "web"
        (0 t   t :class "Firefox")
        (0 t   t :class "Conkeror")
        (0 t   t :class "Google-chrome"))
      (define-frame-preference "work"
        (0 t   t :class "Emacs")
        (0 t   t :class "Gvim")
        (0 t   t :class "OpenOffice.org")
        (0 t   t :class "LibreOffice")
        (0 t   t :class "Eclipse")
        (0 t   t :class "Sublime_text"))
      (define-frame-preference "read"
        (0 t   t :class "Apvlv")
        (0 t   t :class "Kpdf")
        (0 t   t :class "Djview3")
        (0 t   t :class "Djview4")
        (0 t   t :class "Kchmviewer")
        (0 t   t :class "Okular")
        (0 t   t :class "Assistant")
        (0 t   t :class "Fbreader")
        (0 t   t :class "Calibre-gui")
        (0 t   t :class "Zathura"))
      (define-frame-preference "term"
        (0 t   t :class "Konsole")
        (0 t   t :class "Gnome-terminal")
        (0 t   t :class "Sakura")
        (0 t   t :class "Terminator")
        (0 t   t :class "URxvt"))
      (define-frame-preference "mm"
        (0 t   t :class "Qmpdclient")
        (0 t   t :class "Vlc")
        (0 t   t :class "Smplayer"))
      (define-frame-preference "file"
        (0 t   t :class "Krusader")
        (0 t   t :class "Dolphin")
        (0 t   t :title "VIFM")
        (0 t   t :title "ranger:")
        (0 t   t :class "Recoll"))
      (define-frame-preference "virt"
        (0 t   t :class "VirtualBox")))
    (progn
      ;; by default force video players to go to frame 0
      ;; otherwise they freeze up stumpwm being residing
      ;; on external monitor occasionally (VGA-specific)
      (define-frame-preference "Default"
        (0 t   t :class "Qmpdclient")
        (0 t   t :class "Vlc")
        (0 t   t :class "Smplayer"))))

(defcommand rearrange-windows () ()
  (sync-window-placement))
