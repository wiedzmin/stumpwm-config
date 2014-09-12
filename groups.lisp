(clear-window-placement-rules)

;; by default force video players to go to frame 0
;; otherwise they freeze up stumpwm being residing
;; on external monitor occasionally (VGA-specific)
(define-frame-preference "Default"
  (0 t   t :class "Qmpdclient")
  (0 t   t :class "Vlc")
  (0 t   t :class "Smplayer"))

;; NOTE for more elaborate bunch of groups see xmonad config

(defcommand rearrange-windows () ()
  "Rearrange existing windows among groups (if any)"
  (sync-window-placement))
