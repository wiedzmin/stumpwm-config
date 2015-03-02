;;;; Save keyboard layout per window for stumpwm.

;TODO: move to stumpwm-contrib someday

(asdf:compute-source-registry)
(asdf:load-system :xkeyboard)

(in-package :stumpwm)

(defun get-current-layout (display)
  (xlib:device-state-locked-group (xlib:get-state display)))

;; (defparameter *layout-exception-classes* '("Emacs"))

(defun window-focus-changed (window previous-window)
  (declare (ignore previous-window))
  (when window
    (let ((window-layout (getf (xlib:window-plist (window-xwin window)) :keyboard-layout)))
      (if window-layout
          (xlib:lock-group *display* :group window-layout)
          (xlib:lock-group *display* :group 0)))))

(defun group-focus-changed (group previous-group)
  (let ((previous-window (stumpwm::group-current-window previous-group))
        (window (stumpwm::group-current-window group)))
    (window-focus-changed window previous-window)))

(defcommand enable-per-window-layout () ()
  "Enable layout switching"
  (xlib::initialize-extensions *display*) ;; we need it because
  (xlib:enable-xkeyboard *display*) ;; stumpwm opens display before extension definition
  (add-hook *focus-group-hook* 'group-focus-changed)
  (add-hook *focus-window-hook* 'window-focus-changed))

(defcommand disable-per-window-layout () ()
  "Disable layout switching"
  (remove-hook *focus-window-hook* 'window-focus-changed)
  (remove-hook *focus-group-hook* 'group-focus-changed))

;; TODO generalize it
(defun toggle-window-layout ()
  (let ((cur-window (current-window)))
    (when cur-window
      (let ((current-window-class (window-class cur-window)))
        (if (string= current-window-class "Emacs")
            (meta (kbd "C-\\"))
            (progn
              (let ((current-layout (get-current-layout *display*)))
                (if (= current-layout 0)
                    (xlib:lock-group *display* :group 1)
                    (xlib:lock-group *display* :group 0)))
              (let ((current-layout (get-current-layout *display*)))
                (setf (getf (xlib:window-plist (window-xwin (current-window)))
                            :keyboard-layout) current-layout))))))))

(defcommand switch-window-layout () ()
  "Switches keyboard layout for current window"
  (toggle-window-layout))
