;;;
;;; File: commands.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Среда, Февраль  6 2013
;;;
;;;
;;;

(in-package #:stumpwm)

(defcommand swank-start () ()
  "Start Swank backend"
  (setf stumpwm:*top-level-error-action* :break)
  (if *swank-p*
      (message "Swank server already running.")
      (progn
        (swank:create-server :port *swank-port*
                             :style swank:*communication-style*
                             ;;:coding-system "utf-8-unix"
                             :dont-close t)
        (setf *swank-p* t)
        (echo-string (current-screen)
                     "Starting Swank. M-x slime-connect RET RET, then (in-package stumpwm)."))))

(defcommand swank-stop () ()
  "Stop Swank backend"
  (setf stumpwm:*top-level-error-action* :break)
  (swank:stop-server *swank-port*)
  (setf *swank-p* nil)
  (echo-string (current-screen)
         "Stopping Swank..."))

(defcommand raise-volume () ()
  "Raise volume."
  (run-shell-command "amixer -c 0 set Master 10+"))

(defcommand lower-volume () ()
  "Lower volume."
  (run-shell-command "amixer -c 0 set Master 10-"))

(defcommand toggle-modeline () ()
  "Toggle mode line."
  (stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head)))

(defcommand screenshot-default () ()
  "Make screenshot of root window"
  (%screenshot-window (screen-root (current-screen)) (screenshot-filename)))

(defcommand screenshot-window-default () ()
  "Make screenshot of focus window"
  (%screenshot-window (window-xwin (current-window)) (screenshot-filename)))

(defcommand update-mode-line () ()
  "Update the mode-line sooner than usual."
  (update-all-mode-lines))

(defcommand view-gmail () ()
  "View current gmail unread"
  (format-gmail nil))

;; fix with message
(defcommand display-current-window-info () ()
  "Shows the properties of the current window. These properties can be
used for matching windows with run-or-raise or window placement
rules."
  (let ((w (current-window))
        (*suppress-echo-timeout* t)
        (nl (string #\NewLine)))

    ;; (message-no-timeout "class: ~a~%instance~a~%..." (window-class w) (window-res w) ...)
    (echo-string (current-screen)
                 (concat "class:    " (window-class w) nl
                         "instance: " (window-res w) nl
                         "type:     :" (string (window-type w)) nl
                         "role:     " (window-role w) nl
                         "title:    " (window-title w) nl
                         "width:    " (format nil "~a" (window-width w)) nl
                         "height    " (format nil "~a" (window-height w))))))

(defcommand lock-screen () ()
  "Lock the screen."
  (run-shell-command "slimlock"))

(defcommand reinit () ()
  "Reload stumpwm config file"
            (run-commands "reload" "loadrc"))

;; This command runs the stumpwm "quit" command, but only if there aren't any windows open.
(defcommand safequit () ()
            "Checks if any windows are open before quitting."
            (let ((win-count 0))
              ;; Count the windows in each group
              (dolist (group (screen-groups (current-screen)))
                (setq win-count (+ (length (group-windows group)) win-count)))
              ;; Display the number of open windows or quit
              (if (= win-count 0)
                  (run-commands "quit")
                  (message (format nil "You have ~d ~a open" win-count
                                   (if (= win-count 1) "window" "windows"))))))

(defcommand enable-external-monitor-right () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --right-of LVDS1" nil))

(defcommand enable-external-monitor-left () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --left-of LVDS1" nil))

(defcommand enable-external-monitor-above () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --above LVDS1" nil))

(defcommand disable-external-monitor () ()
  "Disables external monitor"
  (run-shell-command "xrandr --output VGA1 --off"))

(defcommand resize-heads () ()
  "Resizes primary head (to see tray)"
  (resize-head 0 0 15 1600 885))

(defcommand rule-them-all () ()
  "Make rules for all currently active windows"
  (clear-window-placement-rules)
  (dolist (w (all-windows))
    (make-rule-for-window w t)))

;; TBD make mode-line follow frame focus between heads
(defcommand toggle-mode-line-here () ()
  (stumpwm:toggle-mode-line (stumpwm:current-screen)
                            (stumpwm:current-head)))

(defcommand (fprev tile-group) () ()
  "Cycle through the frame tree to the previous frame."
  (focus-prev-frame (current-group)))

(define-pull-raise-pairs
  emacs conkeror libreoffice qmpdclient krusader konsole vlc
  (firefox "Firefox" "firefox")
  (urxvt "URxvt")
  (VirtualBox "VirtualBox" "VirtualBox")
  (google-chrome "Google-chrome")
  ;; TBD find out a way to make shell singletones with apps below
  ;; (ranger "URxvt" "urxvt -e ranger-python2.7" "ranger" "ranger")
  ;; (vifm "URxvt" "urxvt -e vifm" "vifm" "vifm")
  ;; (htop "URxvt" "urxvt -e sudo htop" "htop" "htop")
  ;; (ipython "URxvt" "urxvt -e ipython" "ipython" "ipython")
  ;; (mc "URxvt" "urxvt -e mc" "mc" "Midnight Commander")
  )

(defcommand update-all-modelines () ()
  "Update modelines on all heads."
  (enable-mode-line-all-heads))

(defcommand custom/dump-group () ()
  "Dump group for future use"
  (let ((group-file (concatenate 'string  *STUMPWM-LIB-DIR* "layouts/.stumpwm-group" )))
    (dump-to-file (dump-group (current-group)) group-file)))

(defcommand custom/restore-group-multihead () ()
  "Restore group windows placement for multihead setup"
  (let ((group-file (concatenate 'string  *STUMPWM-LIB-DIR* "layouts/.stumpwm-multihead" )))
    (cond ((not (probe-file group-file))
           (message "~s not found" group-file))
          (t
           (restore-group (current-group) (read-dump-from-file group-file))
           ))))
