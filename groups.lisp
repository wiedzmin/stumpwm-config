(clear-window-placement-rules)

;; NOTE for more elaborate bunch of groups see xmonad config

(defcommand rearrange-windows () ()
  "Rearrange existing windows among groups (if any)"
  (sync-window-placement))
