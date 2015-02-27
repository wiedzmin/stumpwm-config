;;;
;;; File: urgent-windows.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Вторник, Март 12 2013
;;;
;;;
;;;

(defvar *urgent-windows-stack* nil)

;; TODO add hook so I get notified when people say my name on IRC or IM me
(defun echo-urgent-window (target)
  (message-no-timeout "~a has a message for you." (window-title target))
  (push target *urgent-windows-stack*))

(defun raise-urgent-window ()
  (let ((last-urgent (pop *urgent-windows-stack*)))
    (when last-urgent
      (gselect (window-group last-urgent))
      (really-raise-window last-urgent))))

(defcommand raise-urgent () ()
  "Raise urgent window"
  (raise-urgent-window))
