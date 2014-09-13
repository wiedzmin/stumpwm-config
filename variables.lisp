;;;
;;; File: variables.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Четверг, Февраль  7 2013
;;;
;;;
;;;

(defparameter *simple-resize-increment* 100)

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
           "[%c %t] [%M / %N] [%D] [%l] [%b]"
           (string #\NewLine)
           " "
           "^n"
           "^7*(^[^n ^]" `(:eval (window-title-and-notifications-with-fix-length 200 20)) ")^n "
           ))
(setf *mode-line-foreground-color* "DarkSeaGreen")
(setf *mode-line-background-color* "Gray15")

(setf *mode-line-timeout* 10)
(setf *disk-modeline-fmt* "%m: %a/%s")
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

(defparameter *reserve-tray-placement* nil
  "Should we reserve screen real estate for system tray app?")

(defvar *heads-updated* nil
  "Tracks status of heads updates")

(defparameter X-TERM "urxvt"
  "What shall be the command run when we want an X terminal?")
