;;;
;;; File: commands.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Среда, Февраль  6 2013
;;;
;;;
;;;

(in-package #:stumpwm)

(let ((swank-p nil))
  (defun stop-swank ()
    (setf stumpwm:*top-level-error-action* :break)
    (swank:stop-server *swank-port*)
    (setf swank-p nil)
    (echo-string (current-screen)
                       "Stopping Swank..."))
  (defun stop-swank-on-quit ()
    (when swank-p (stop-swank)))
  (defun start-swank ()
    (swank:create-server :port *swank-port*
                         :style swank:*communication-style*
                         ;;:coding-system "utf-8-unix"
                         :dont-close t)
    (setf swank-p t)
    (echo-string (current-screen)
                   "Starting Swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
  (defcommand swank-toggle () ()
    "Toggle the swank server on/off"
    (if swank-p
        (stop-swank)
        (start-swank)))
  (defcommand swank-status () ()
    (if swank-p
        (echo-string (current-screen) "Swank is ON")
        (echo-string (current-screen) "Swank is OFF")))
  (add-hook *quit-hook* 'stop-swank-on-quit))

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
  (screenshot:screenshot (screenshot-filename)))

(defcommand screenshot-window-default () ()
  "Make screenshot of focus window"
  (screenshot:screenshot-window (screenshot-filename)))

(defcommand update-mode-line () ()
  "Update the mode-line sooner than usual."
  (update-all-mode-lines))

(defcommand display-current-window-info () ()
  "Shows the properties of the current window. These properties can be
used for matching windows with run-or-raise or window placement
rules."
  (let ((w (current-window))
        (*suppress-echo-timeout* t)
        (nl (string #\NewLine)))
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
  (run-shell-command "slock")
  (run-shell-command "sleep 1 && xset dpms force off"))

(defcommand enable-external-monitor-right () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --right-of LVDS1" nil)
  (setf *heads-updated* nil))

(defcommand enable-external-monitor-left () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --left-of LVDS1" nil)
  (setf *heads-updated* nil))

(defcommand enable-external-monitor-above () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --above LVDS1" nil)
  (setf *heads-updated* nil))

(defcommand disable-external-monitor () ()
  "Disables external monitor"
  (run-shell-command "xrandr --output VGA1 --off")
  (setf *heads-updated* nil)
  (banish :head)
  (banish :frame))

;TODO: fix "above" config as it fails to navigate windows after calling 'resize-heads
(defcommand resize-heads () ()
  "Resizes primary head (to see tray)"
  (let ((internal-head (nth 0 (screen-heads (current-screen)))))
    (unless *heads-updated*
      (resize-head 0
                   (head-x internal-head)
                   (+ (head-y internal-head) 15)
                   (head-width internal-head)
                   (- (head-height internal-head) 15)
                   )
      (setf *heads-updated* t))))

(defcommand (fprev tile-group) () ()
  "Cycle through the frame tree to the previous frame."
  (focus-prev-frame (current-group)))

(define-pull-raise-pairs
  emacs conkeror libreoffice
  qmpdclient konsole
  vlc fbreader zathura
  firefox skype
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

(defcommand update-heads-layout () ()
  "Update stuff after attaching external head(s)"
  (let ((update-commands nil))
    (push "update-all-modelines" update-commands)
    (when *reserve-tray-placement*
      (push "resize-heads" update-commands))
    (apply #'run-commands update-commands))
  (update-emacs-frames))

(defcommand custom/dump-group (filename) ((:string "Set filename: "))
  "Dump group for future use"
  (let ((group-file (cat *STUMPWM-LIB-DIR* "layouts/" filename)))
    (dump-to-file (dump-group (current-group)) group-file)))

;; TBD Refactor to generalize with search macros
(defcommand custom/open-selection () ()
  "Open selection in browser as URL"
  (open-in-browser (get-x-selection)))

(defcommand custom/choose-group-layout () ()
  "Select windows layout from menu"
  (select-layout-from-menu))

(defmacro defwebjump (caption url)
  `(defcommand
       ,(concat-as-symbol "custom/open-" (string-downcase (substitute #\- #\Space caption)))
       () ()
     ,(format nil "Open ~a" caption)
     (open-in-browser ,url)))

(defwebjump "Gmail" "https://mail.google.com/mail/u/0/#inbox")
(defwebjump "Github" "https://github.com/wiedzmin")
(defwebjump "Yandex" "http://yandex.ru")
(defwebjump "Facebook" "https://facebook.com/")
(defwebjump "Multitran" "http://www.multitran.ru/")
(defwebjump "Youtube" "http://youtube.com/")
(defwebjump "Toodledo" "http://www.toodledo.com/")
(defwebjump "Lumosity" "http://lumosity.com/")
(defwebjump "Feedly" "http://cloud.feedly.com/")
(defwebjump "Delicious" "https://delicious.com/")

(defcommand custom/choose-book () ()
  "Select books to read from menu"
  (select-books-from-menu))

;TODO: abstract away terminal shell command
(defcommand custom/run-htop () ()
  "Run htop"
  (run-shell-command "urxvt -e htop"))

(defcommand custom/run-wicd-curses () ()
  "Run wicd-curses"
  (run-shell-command "urxvt -e wicd-curses"))

(defcommand custom/run-wicd-gtk () ()
  "Run wicd-gtk"
  (run-shell-command "wicd-gtk"))

(defcommand custom/run-iotop () ()
  "Run iotop"
  (run-shell-command "urxvt -e sudo iotop"))

(defcommand custom/run-powertop () ()
  "Run powertop"
  (run-shell-command "urxvt -e sudo powertop"))
