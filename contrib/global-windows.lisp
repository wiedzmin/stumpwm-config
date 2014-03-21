;; -*- coding: utf-8 -*-
;;
;; Filename: global-select.lisp
;; Created: Пн апр  1 01:15:20 2013 (+0400)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-global-windows ()
  "Returns a list of the names of all the windows in the current screen."
  (let ((groups (sort-groups (current-screen)))
        (windows nil))
    (dolist (group groups)
      (dolist (window (group-windows group))
        ;; Don't include the current window in the list
        (when (not (eq window (current-window)))
          (setq windows (cons window windows)))))
    windows))

(defun my-global-window-names ()
  "Returns a list of the names of all the windows in the current screen."
  (mapcar (lambda (window) (window-name window)) (my-global-windows)))

(defun my-window-in-group (query group)
  "Returns a window matching QUERY in GROUP."
  (let ((match nil)
        (end nil)
        (name nil))
    (dolist (window (group-windows group))
      (setq name (window-name window)
            end (min (length name) (length query)))
      ;; Never match the current window
      (when (and (string-equal name query :end1 end :end2 end)
                 (not (eq window (current-window))))
        (setq match window)
        (return)))
    match))

(defun sort-windows-global (windowlist)
  "Return a copy of the screen's window list sorted by number."
  (sort1 windowlist 'string-lessp :key 'window-name))

(defun goto-window (window)
  "Raise the window win and select its frame.  For now, it does not
select the screen."
  (let* ((group (window-group window))
         (frame (window-frame window))
         (old-frame (tile-group-current-frame group)))
    (frame-raise-window group frame window)
    (focus-all window)
    (unless (eq frame old-frame)
      (show-frame-indicator group))))

(define-stumpwm-type :my-global-window-names (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen) prompt (my-global-window-names))))

(defmacro with-global-windowlist (name docstring &rest args)
 `(defcommand ,name (&optional (fmt *window-format*)) (:rest)
   ,docstring
   (let ((global-windows-list (my-global-windows)))
     (if (null global-windows-list)
         (message "No other windows on screen ;)")
         (let ((window (select-window-from-menu (sort-windows-global global-windows-list) fmt)))
           (when window
             (progn ,@args)))))))

(with-global-windowlist global-windowlist "Like windowlist, but for all groups not just the current one."
  (goto-window window))

(with-global-windowlist global-pull-windowlist "Like windowlist, but for all groups not just the current one."
  (move-window-to-group window (current-group))
  (pull-window window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global-select.lisp ends here
