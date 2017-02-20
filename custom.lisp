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

(define-application emacs :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application urxvt :class "URxvt" :map *raise-keymap* :key "t" :pullp t :pull-map *pull-keymap* :pull-key "t")
(define-application sakura :map *raise-keymap* :key "s" :pullp t :pull-map *pull-keymap* :pull-key "s")
(define-application firefox :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application google-chrome-stable :class "Google-chrome" :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application virtualbox :command "VirtualBox" :class "VirtualBox" :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application qmpdclient :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application vlc :class "vlc" :map *raise-keymap* :key "l" :pullp t :pull-map *pull-keymap* :pull-key "l")
(define-application fbreader :map *raise-keymap* :key "2" :pullp t :pull-map *pull-keymap* :pull-key "2")
(define-application zathura :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application skype :map *raise-keymap* :pullp t :pull-map *pull-keymap* :binded nil)
(define-application pavucontrol :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application krdc :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application vncviewer :map *raise-keymap* :key "i" :pullp t :pull-map *pull-keymap* :pull-key "i")
(define-application copyq :command "copyq show" :map *raise-keymap* :key "c" :pullp t :pull-map *pull-keymap* :pull-key "c")
(define-application sonata :map *raise-keymap* :key "m" :pullp t :pull-map *pull-keymap* :pull-key "m")
(define-application telegram :command "Telegram" :map *raise-keymap* :key "T" :pullp t :pull-map *pull-keymap* :pull-key "T")

(defwebjump "Gmail" "https://mail.google.com/mail/u/0/#inbox" :key "m")
(defwebjump "Github" "https://github.com/wiedzmin" :key "g")
(defwebjump "Yandex" "http://yandex.ru" :key "y")
(defwebjump "Facebook" "https://facebook.com/" :key "f" :browser "Google chrome")
(defwebjump "Multitran" "http://www.multitran.ru/" :key "t")
(defwebjump "Youtube" "http://youtube.com/" :key "b" :browser "Google chrome")
(defwebjump "Toodledo" "http://www.toodledo.com/" :key "o")
(defwebjump "Lumosity" "http://lumosity.com/" :key "l" :browser "Google chrome")
(defwebjump "Feedly" "http://cloud.feedly.com/" :key "d" :binded nil)
(defwebjump "Delicious" "https://delicious.com/" :key "s")
(defwebjump "Gismeteo radar" "http://www.gismeteo.ru/map/647/" :key "r")
(defwebjump "Pocket" "https://getpocket.com/a/queue/1" :key "p")
(defwebjump "Chillout music" "http://www.di.fm/chillout" :key "c" :browser "Google chrome")
(defwebjump "Google Docs" "https://docs.google.com/document/u/0/" :key "u")
(defwebjump "Telegram" "https://web.telegram.org/#/im" :key "4")
(defwebjump "Orgmode maillist" "http://news.gmane.org/gmane.emacs.orgmode" :key "O")
(defwebjump "Storm locator" "http://ru.blitzortung.org/live_dynamic_maps.php" :binded nil)
(defwebjump "Yandex Music" "https://music.yandex.ru" :key "Y")
(defwebjump "RelaxFM" "http://relax-fm.ru" :key "R")
(defwebjump "E-lactancia" "http://e-lactancia.org/" :key "e")
(defwebjump "Facebook messenger" "https://www.facebook.com/messages/t/" :key "C")
