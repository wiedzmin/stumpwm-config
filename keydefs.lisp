;;;
;;; File: keydefs.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Среда, Февраль  6 2013
;;;
;;;
;;;

(in-package #:stumpwm)

(set-prefix-key (kbd "s-z"))

;; (define-keysym #x1008ff11 "XF86AudioLowerVolume")
;; (define-keysym #x1008ff12 "XF86AudioMute")
;; (define-keysym #x1008ff13 "XF86AudioRaiseVolume")

(define-keys *raise-keymap*
  ("DEL"  "other")
  ("3" "custom/spawn-emacs-frame"))

(define-keys *heads-keymap*
  ("Right"  "enable-external-monitor-right")
  ("Left"  "enable-external-monitor-left")
  ("Up"  "enable-external-monitor-above")
  ("Down"  "disable-external-monitor")
  ("r"  "toggle-external-head-rotation")
  ("u"  "update-heads-layout"))

(define-keys *frames-keymap*
  ("Right"  "exchange-direction right")
  ("Left"  "exchange-direction left")
  ("Up"  "exchange-direction up")
  ("Down"  "exchange-direction down")
  ("0" "remove-split")
  ("1" "only")
  ("2" "vsplit")
  ("3" "hsplit")
  ("5" "warp-mouse-active-frame")
  ("\\" "balance-frames"))

(define-keys *desktop-keymap*
  ("a"  "update-all-modelines")
  ("d"  "custom/dump-group")
  ("t"  "stumptray")
  ("g"  "custom/org-clock-goto")
  ("r"  "resume-dunst")
  ("O"  "custom/org-agenda")
  ("o"  "custom/org-agenda-list")
  ("Left" "winner-undo")
  ("Right" "winner-redo"))

(define-keys *swank-keymap*
  ("t"  "swank-toggle")
  ("s"  "swank-status"))

(define-keys *shell-keymap*
  ("h"  "custom/run-htop")
  ("i"  "custom/run-iotop")
  ("p"  "custom/run-powertop")
  ("Up"  "custom/start-job-vpn")
  ("Down"  "custom/stop-job-vpn")
  ("DEL"  "custom/restart-job-vpn")
  ("s"  "custom/job-vpn-status"))

(define-keys *root-map*
  ("Delete" "toggle-modeline")
  ("ESC" "abort")
  ("Q" "restart-hard") ;was "quit"
  ("p" "global-pull-windowlist")
  ("s-Down" "move-window down")
  ("s-Left" "move-window left")
  ("s-Right" "move-window right")
  ("s-Up" "move-window up")
  ("s-b" "mode-lines")
  ("s-c" "screenshot-window-default")
  ("s-i" "iresize")
  ("s-l" "lock-screen")
  ("s-m" "screenshot-default")
  ("w" "custom/rofi-windowlist")
  ("l" "custom/choose-group-layout")
  ("b" "custom/choose-book")
  )

(define-keys *top-map*
  ("s-P" "exec gmrun")
  ("s-C" "kill-window")
  ;;
  ("XF86AudioRaiseVolume" "raise-volume")
  ("XF86AudioLowerVolume" "lower-volume")
  ("XF86AudioMute" "toggle-volume")
  ;;
  ("XF86MonBrightnessUp" "brightness-up")
  ("XF86MonBrightnessDown" "brightness-down")
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
  ("s-u"     "raise-urgent")
  ("s-f"     "fullscreen")
  ;;
  ("s-/"       *search-keymap*)
  ("s-1"       *pull-keymap*)
  ("s-2"       *raise-keymap*)
  ("s-`"       *heads-keymap*)
  ("s-3"       *frames-keymap*)
  ("s-q"       *desktop-keymap*)
  ("s-w"       *web-keymap*)
  ("s-s"       *swank-keymap*)
  ("s-4"       *shell-keymap*)
  ("s-e"       "fselect")
  ;;
  ("s-i" "display-current-window-info")
  )

(flet ((dk (k c)
        (define-key *top-map* k (format nil c *simple-resize-increment*))))
 (dk (kbd "s-S-Up") "resize 0 -~D")
 (dk (kbd "s-S-Down") "resize 0 ~D")
 (dk (kbd "s-S-Left") "resize -~D 0")
 (dk (kbd "s-S-Right") "resize ~D 0"))
