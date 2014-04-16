;;;
;;; File: variables.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Четверг, Февраль  7 2013
;;;
;;;
;;;

(setf *normal-border-width* 0
      *maxsize-border-width* 0
      *transient-border-width* 1
      *window-border-style* :thick)     ; :thick :thin :tight :none
(setf *mode-line-position* :bottom)
(setf *frame-number-map* "1234567890")
(setf *startup-message* "Never Stop Hacking!")
(setf *run-or-raise-all-screens* t)
(setf *mode-line-foreground-color* "DarkSeaGreen")
(setf *mode-line-timeout* 10)
(setf *normal-border-width* 1)
(setf *disk-modeline-fmt* "%m: %a/%s")
(setf *time-modeline-string* "%d-%m-%Y ^3*^B%H:%M^b^n %a")
(setf *screen-mode-line-format*
     (list " "
           "^B[^b"
           "%d"
           "^B]^b "
           "[%c %t] [%M / %N] [%D] [%l] [%b]"
           (string #\NewLine)
           " "
           `(:eval (mode-line-groups-list))
           "^7*(^[^n ^]" `(:eval (window-title-and-notifications-with-fix-length 200 20)) ")^n "
           ))
(setf *window-format* "%n%s%t")

;; Window border colors.
(set-focus-color "red")
(set-win-bg-color "black")

(setf *window-border-style* :thin)      ; :thick :thin :tight :none
(set-normal-gravity :bottom)
(set-maxsize-gravity :center)
(set-transient-gravity :center)

(set-fg-color "yellow")
(set-border-color "grey16")
(set-msg-border-width 0)
(setf *input-window-gravity* :center)

;; Set the message and input box to the bottom right. This way it overlaps with mode-line.
(setf *message-window-gravity* :bottom-right)
;; in seconds, how long a message will appear for. This must be an integer.
(setf *timeout-wait* 5)

(defparameter *simple-resize-increment* 100)

;; (defparameter X-TERM "urxvt"
;;   "What shall be the command run when we want an X terminal?")

;; (defparameter X-WWW-BROWSER "firefox"
;;   "What GUI WWW browser shall we use?")
