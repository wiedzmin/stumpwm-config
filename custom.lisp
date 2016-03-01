(in-package #:stumpwm)

;; borders
(setf *maxsize-border-width* 2)
(setf *transient-border-width* 1)
(setf *normal-border-width* 1)
(setf *window-border-style* :thin)

(set-focus-color "red")
(set-win-bg-color "black")
(set-border-color "grey16")
(set-msg-border-width 0)

;; modeline
(setf *mode-line-position* :bottom)
(setf *screen-mode-line-format*
     (list " "
           "^B[^b"
           "%d"
           "^B]^b "
           "[%c %t] [%M / %N] [%D] [%l] [%b] %m"
           (string #\NewLine)
           " "
           "^n"
           "^7*(^[^n ^]" `(:eval (fix-str-length (current-window-title) 200)) ")^n "
           ))
(setf *mode-line-foreground-color* "DarkSeaGreen")
(setf *mode-line-background-color* "Gray15")

(setf *mode-line-timeout* 10)
(setf disk::*disk-modeline-fmt* "%m: %a/%s")
(setf *time-modeline-string* "%d-%m-%Y ^3*^B%H:%M^b^n %a")

;; gravity
(setf *input-window-gravity* :center)
(setf *message-window-gravity* :bottom-right)

(set-normal-gravity :bottom)
(set-maxsize-gravity :center)
(set-transient-gravity :center)

;; various
(setf *frame-number-map* "1234567890")
(setf *startup-message* "Never Stop Hacking!")
(setf *run-or-raise-all-screens* t)
(setf *window-format* "%n%s%t")
(set-fg-color "yellow")
(setf *timeout-wait* 3)
(setf *mouse-focus-policy* :click)
(setf *grab-pointer-character* 40)
(setf *grab-pointer-character-mask* 41)
(setf *grab-pointer-foreground* (xlib:make-color :red 0.24 :green 0.70 :blue 0.44))
(setf *grab-pointer-background* (xlib:make-color :red 0.173 :green 0.325 :blue 0.792))

(setf *reserve-tray-placement* t)
(setf *mouse-follows-focus* t)

(setf *rotate-external-head* t)

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
    "Echo swank running status"
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

(defcommand toggle-volume () ()
  (run-shell-command "amixer set Master toggle >> /dev/null" t))

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

(defcommand suspend-dunst () ()
   "Suspend dunst"
   (run-shell-command "killall -SIGUSR1 dunst"))

(defcommand resume-dunst () ()
   "Resume dunst and receive suspended messages"
   (run-shell-command "killall -SIGUSR2 dunst"))

(defcommand lock-screen () ()
  "Lock the screen."
  (suspend-dunst)
  (run-shell-command "i3lock")
  (run-shell-command "sleep 1 && xset dpms force off"))

(defcommand warp-mouse-active-frame () ()
  (let* ((current-frame (tile-group-current-frame (current-group)))
         (pointer-x (- (+ (frame-x current-frame) (frame-width current-frame)) 100))
         (pointer-y (+ 100 (frame-y current-frame))))
    (warp-pointer (current-screen) pointer-x pointer-y)))

(defcommand enable-external-monitor-right () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --right-of LVDS1" nil)
  (when *rotate-external-head*
    (run-shell-command "xrandr --output VGA1 --rotate left" nil))
  (setf *heads-updated* nil))

(defcommand enable-external-monitor-left () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --left-of LVDS1" nil)
  (when *rotate-external-head*
    (run-shell-command "xrandr --output VGA1 --rotate left" nil))
  (setf *heads-updated* nil))

(defcommand enable-external-monitor-above () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --above LVDS1" nil)
  (setf *heads-updated* nil))

(defcommand disable-external-monitor () ()
  "Disables external monitor"
  (run-shell-command "xrandr --output VGA1 --off")
  (warp-mouse-active-frame)
  (setf *heads-updated* nil))

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

(defcommand update-all-modelines () ()
  "Update modelines on all heads."
  (enable-mode-line-all-heads))

(defcommand update-heads-layout () ()
  "Update stuff after attaching external head(s)"
  (let ((update-commands nil))
    (when *update-all-modelines*
      (push "update-all-modelines" update-commands))
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

(defcommand custom/org-clock-goto () ()
  "Go to recently clocked-in Org heading in emacs"
  (emacs-org-clock-goto))

(defcommand custom/org-agenda () ()
  "Open generic orgmode agenda in emacs"
  (emacs-org-open-agenda))

(defcommand custom/org-agenda-list () ()
  "Open calendar-pinned orgmode agenda in emacs"
  (emacs-org-open-agenda-list))

(defcommand brightness-up () ()
  (run-shell-command "xbacklight -inc 10"))

(defcommand brightness-down () ()
  (run-shell-command "xbacklight -dec 10"))

(defcommand mode-lines () ()
  "A command to toggle the mode line visibility for all screens/heads."
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (toggle-mode-line screen head))))

(defcommand custom/start-job-vpn () ()
  (run-shell-command "~/scripts/start-job-vpn.sh"))

(defcommand custom/stop-job-vpn () ()
  (run-shell-command "sudo killall openvpn"))

(defcommand custom/rofi-windowlist () ()
  (run-shell-command "rofi -show window"))

(defcommand custom/bother-stuck-emacs () ()
  (run-shell-command "pkill -SIGUSR2 emacs"))

(defcommand custom/set-default-browser () ()
  (set-default-browser)
  (save-default-browser))

(defcommand current-browser () ()
  "Echo current browser"
  (echo-string (current-screen) (browser-name default-browser)))

(define-application emacs :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application urxvt :class "URxvt" :map *raise-keymap* :key "t" :pullp t :pull-map *pull-keymap* :pull-key "t")
(define-application firefox :map *raise-keymap* :pullp t :pull-map *pull-keymap* :binded nil)
(define-application google-chrome-stable :class "google-chrome" :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application virtualbox :command "VirtualBox" :class "VirtualBox" :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application qmpdclient :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application vlc :map *raise-keymap* :key "l" :pullp t :pull-map *pull-keymap* :pull-key "l")
(define-application fbreader :map *raise-keymap* :key "2" :pullp t :pull-map *pull-keymap* :pull-key "2")
(define-application zathura :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application sakura :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application skype :map *raise-keymap* :pullp t :pull-map *pull-keymap* :binded nil)
(define-application wicd-gtk :command "wicd-gtk -n" :class "Wicd-client.py" :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application pavucontrol :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application krdc :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application vncviewer :map *raise-keymap* :key "i" :pullp t :pull-map *pull-keymap* :pull-key "i")
(define-application copyq :command "copyq show" :map *raise-keymap* :key "c" :pullp t :pull-map *pull-keymap* :pull-key "c")
(define-application sonata :map *raise-keymap* :key "m" :pullp t :pull-map *pull-keymap* :pull-key "m")

(defwebjump "Gmail" "https://mail.google.com/mail/u/0/#inbox" :key "m")
(defwebjump "Github" "https://github.com/wiedzmin" :key "g")
(defwebjump "Yandex" "http://yandex.ru" :key "y")
(defwebjump "Facebook" "https://facebook.com/" :key "f" :binded nil)
(defwebjump "Multitran" "http://www.multitran.ru/" :key "t")
(defwebjump "Youtube" "http://youtube.com/" :key "b")
(defwebjump "Toodledo" "http://www.toodledo.com/" :key "o")
(defwebjump "Lumosity" "http://lumosity.com/" :key "l")
(defwebjump "Feedly" "http://cloud.feedly.com/" :key "d")
(defwebjump "Delicious" "https://delicious.com/" :key "s")
(defwebjump "Gismeteo radar" "http://www.gismeteo.ru/map/647/" :key "r")
(defwebjump "Pocket" "https://getpocket.com/a/queue/1" :key "p")
(defwebjump "Chillout music" "http://www.di.fm/chillout" :key "c")
(defwebjump "Google Docs" "https://docs.google.com/document/u/0/" :key "u")
(defwebjump "Telegram" "https://web.telegram.org/#/im" :key "4")
(defwebjump "Orgmode maillist" "http://news.gmane.org/gmane.emacs.orgmode" :key "O")
