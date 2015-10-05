;;;
;;; File: variables.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Четверг, Февраль  7 2013
;;;
;;;
;;;
(in-package #:stumpwm)

(defparameter *simple-resize-increment* 100)

(defparameter *pull-keymap* (make-sparse-keymap))
(defparameter *raise-keymap* (make-sparse-keymap))
(defparameter *search-keymap* (make-sparse-keymap))
(defparameter *web-keymap* (make-sparse-keymap))
(defparameter *heads-keymap* (make-sparse-keymap))
(defparameter *frames-keymap* (make-sparse-keymap))
(defparameter *desktop-keymap* (make-sparse-keymap))
(defparameter *swank-keymap* (make-sparse-keymap))
(defparameter *shell-keymap* (make-sparse-keymap))
(defparameter *reserve-tray-placement* nil
  "Should we reserve screen real estate for system tray app?")
(defparameter *update-all-modelines* nil
  "Should we update all modelines while updating heads?")
(defvar *heads-updated* nil
  "Tracks status of heads updates")
(defparameter *BROWSER* "firefox"
  "What shall be the command run when we want a browser?")
(defparameter *BROWSER-PARAMS* '("-new-tab")
  "What shall be CLI params for the browser?")
(defparameter *ALTERNATIVE-BROWSER* "google-chrome-stable"
  "What shall be the command run when we want an alternative browser?")
(defparameter *ALTERNATIVE-BROWSER-PARAMS* nil
  "What shall be CLI params for the browser?")
(defparameter *PDF-VIEWER* "zathura"
  "Default PDF viewer")
(defparameter *autostarts* nil
  "a list of application names that should be automatically started on session start")
(defparameter *mouse-follows-focus* nil
  "Should mouse pointer follow window focus?")

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
