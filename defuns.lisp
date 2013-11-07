;;;
;;; File: defuns.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Среда, Февраль  6 2013
;;;
;;;
;;;

(defun current-window-title ()
  (let ((current-window (current-window)))
    (if current-window
        (window-title current-window)
        "No Windows")))

(defun screenshot-filename ()
  (concatenate
   'string
   "~/images/screenshots/screenshot-"
   (run-shell-command "date +\"%d-%m-%Y-%T\" | tr -d '[:cntrl:]'" t)
   ".png"))

(defun show-key-seq (key seq val)
  (message "Key sequence: ~A" (print-key-seq (reverse seq))))
(add-hook *key-press-hook* 'show-key-seq)

;;; find window and to what your whant.
(defun find-window-group (group props)
  (find-if (lambda (w)
             (apply 'window-matches-properties-p w props))
           (group-windows group)))

(defun find-window-range (props &optional all-groups all-screens)
  (let ((screens (if all-screens
                     *screen-list*
                     (list (current-screen)))))
    (if all-groups
        (loop named outer
           for s in screens
           do (loop
                 for g in (screen-groups s)
                 for win = (find-window-group g props)
                 when win
                 do (return-from outer win)))
        (find-window-group (current-group) props))))


(defun to-window (win)
  (let* ((group (window-group win))
         (frame (window-frame win))
         (old-frame (tile-group-current-frame group)))
    (frame-raise-window group frame win)
    (focus-all win)
    (unless (eq frame old-frame)
      (show-frame-indicator group))))

(defun find-window-dwim (props &key all-groups all-screens do-func send-str exec-shell switch-to to-and-back)
  (let ((win (find-window-range props all-groups all-screens))
        (last-window (if to-and-back
                         (current-window))))
    (if win
        (if (or switch-to to-and-back)
            (progn
              (to-window win)
              do-func
              (if exec-shell (if (run-shell-command exec-shell t)
                                 (if to-and-back (to-window last-window)))))
            (progn
              (eval do-func)
              (if send-str (window-send-string win send-str))
              (if exec-shell (run-shell-command exec-shell))))
        (message "No such windows!"))))
(export 'find-window-dwim)

;; macro for faster startups
(defmacro replace-hook (hook fn)
  `(remove-hook, hook, fn)
  `(add-hook, hook, fn))

;; todo: use function user-homedir-pathname and merge-pathnames to update it.
(defun expand-file-name (path &optional default-directory)
  (let ((first-char (subseq path 0 1))
        (home-dir (concat (getenv "HOME") "/"))
        (dir (if default-directory
                 (if (string= (subseq (reverse default-directory) 0 1) "/")
                     default-directory
                     (concat default-directory "/")))))
    (cond ((string= first-char "~") (concat home-dir (subseq path 2)))
          ((string= first-char "/") path)
          (dir (if (string= (subseq dir 0 1) "/")
                   (concat dir path)
                   (expand-file-name (concat dir path))))
          (t (concat home-dir path)))))

(defun cat (&rest strings) ; "Concatenates strings, like the Unix command 'cat'. A shortcut for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))

(defmacro program-with-layout (name &key (command (string-downcase (string name)))
                               (props `'(:class ,(string-capitalize command))))
  `(defcommand ,name () ()
     (gnew ,command)
     (restore-from-file ,(concat "/home/dima/.stumpwm_files/"
                                 command "-layout"))
     (restore-window-placement-rules ,(concat "/home/dima/.stumpwm_files/"
                                              command "-rules"))
     (run-or-raise ,command ,props)
     (place-existing-windows))) ; needed if the command has already been run


(defun window-title-and-notifications-with-fix-length (length notifications-length)
  (if (< length notifications-length)
      (error "length should bigger than notifications-length!"))
  (if notifications
      (cat (fix-str-length (current-window-title) (- length
                                                     notifications-length
                                                     3))
           " ["
           (fix-str-length (format nil "~{ ~a~#[~:;;~]~}" notifications) notifications-length)
           "]")
    (fix-str-length (current-window-title) length)))

(defun mode-line-groups-list ()
  (if (and (boundp '*xmonad-style-groups*) *xmonad-style-groups*)
      "^6*(^[^n ^]%g)^n "
      "^n"))

(defun current-window-title ()
  (let ((current-window (current-window)))
    (if current-window
        (window-title current-window)
      (cat "No Window In ::"
(group-name (current-group)) "::"))))

(defun fix-str-length (str length)
  (if (> (length str) length)
      (cat (subseq str 0 (- length 2)) ".*")
    (format nil "~va" length str)))

(defun change-vol (sign val) () "Change mixer volume by value val"
  (run-shell-command (format nil "mixer vol ~A~A" sign val)))

(defmacro define-pull-raise-pairs (&body items)
  "Defines pairs of pull/raise commands for applications in 'items'. Credits to ivan4th"
  `(progn
     ,@(loop for item in items
             for %name = (if (consp item) (first item) item)
             for class = (if (consp item) (second item) (string-capitalize %name))
             for name = (if (symbolp %name) (string-downcase %name) %name)
             for command = (or (when (consp item) (third item)) name)
             for instance = (or (when (and (consp item) (>= (list-length item) 4)) (fourth item)) nil)
             for title = (or (when (and (consp item) (>= (list-length item) 5)) (fifth item)) nil)
             collect
             `(defcommand ,(intern (format nil "P-~a" (string-upcase name))) () ()
                ,(format nil "Start ~a unless it is already running, ~
in which case pull it into the current frame."
                         name)
                (run-or-pull ,command '(:class ,class :instance ,instance :title ,title)))
             collect
             `(defcommand ,(intern (string-upcase name)) () ()
                ,(format nil "Start ~a unless it is already running, ~
in which case focus it."
                         name)
                (run-or-raise ,command '(:class ,class :instance ,instance :title ,title))))))
