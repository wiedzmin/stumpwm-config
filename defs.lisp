(in-package #:stumpwm)

(defparameter *FOREGROUND-COLOR* "green")
(defparameter *BACKGROUND-COLOR* "black")
(defparameter *BORDER-COLOR* "green")
(defparameter *swank-port* 4005)

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


(defmacro define-keys (keymap &rest keys)
  `(dolist (keydef (quote ,keys))
     (define-key ,keymap (kbd (car keydef)) (cadr keydef))))

(defmacro build-keymap (&rest keys)
  `(let ((keymap (make-sparse-keymap)))
     (define-keys keymap ,@keys)
     keymap))

(defun cat (&rest strings) ; "Concatenates strings, like the Unix command 'cat'. A shortcut for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))

(defun concat-as-symbol (prefix suffix)
  (intern (string-upcase (cat prefix suffix))))

(defun screenshot-filename ()
  (cat
   "~/screenshots/screenshot-"
   (run-shell-command "date +\"%d-%m-%Y-%T\" | tr -d '[:cntrl:]'" t)
   ".png"))

(defun fix-str-length (str length)
  (if (> (length str) length)
      (cat (subseq str 0 (- length 2)) ".*")
    (format nil "~va" length str)))

(defun current-window-title ()
  (let ((current-window (current-window)))
    (if current-window
        (let ((title (window-title current-window)))
          (subseq title 0 (search " - http" title)))
        (cat "No Window In ::"
           (group-name (current-group)) "::"))))

(defmacro define-application (name &key (command (string-downcase (string name)))
                                     (class (string-capitalize command) )
                                     (instance nil)
                                     (title nil)
                                     (map '*top-map*)
                                     (key (subseq command 0 1))
                                     (pullp nil)
                                     (pull-map nil)
                                     (pull-name (intern1 (concat "p-" (string name))))
                                     (pull-key (subseq command 0 1)))
  "Define a command and key binding to run or raise a program. If
@var{pullp} is set, also define a command and key binding to run or
pull the program."
  `(progn
     (defcommand ,name () ()
                 ,(format nil "Start ~a unless it is already running, ~
in which case focus it."
                          name)
       (run-or-raise ,command '(:class ,class :title ,title)))
     (define-key ,map (kbd ,key) ,(string-downcase (string name)))
     ,(when pullp
        `(progn
           (defcommand (,pull-name tile-group) () ()
                       ,(format nil "Start ~a unless it is already running, ~
in which case pull it into the current frame."
                                name)
             (run-or-pull ,command '(:class ,class :instance ,instance :title ,title)))
           (define-key ,pull-map (kbd ,pull-key) ,(string-downcase (string pull-name)))))))

(defmacro defwebjump (caption url &key (map *web-keymap*) (key nil))
  (let ((command-name (concat-as-symbol "custom/open-" (string-downcase (substitute #\- #\Space caption)))))
    `(progn
       (defcommand
           ,command-name () ()
         ,(format nil "Open ~a" caption)
         (open-in-browser ,url))
       (when ,key
         (define-key ,map (kbd ,key) ,(string-downcase command-name))))))

(defun enable-mode-line-all-heads ()
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head t))))

(defmacro with-emacs (&body body)
  `(when (find-matching-windows '(:class "Emacs") t t)
     (emacs)
     (progn ,@body)))

(defmacro with-emacs-noninteractive (&body body)
  `(when (find-matching-windows '(:class "Emacs") t t)
     (run-shell-command
      ,(string-downcase (format nil "emacsclient --eval '~a'" (prin1 `(progn ,@body)))))
     (unless (search "emacs" (window-title (current-window)))
       (emacs))))

(defun update-emacs-frames ()
  (if (> (length (screen-heads (car *screen-list*))) 1)
      (with-emacs-noninteractive (custom/update-frames 2))
      (with-emacs-noninteractive (custom/update-frames 1))))

(defun emacs-org-clock-goto ()
  (with-emacs-noninteractive
    (org-clock-goto)))

(defun emacs-org-open-agenda ()
  (with-emacs-noninteractive
    (org-agenda)))

(defun emacs-org-open-agenda-list ()
  (with-emacs-noninteractive
    (org-agenda-list)))

(defmacro define-filelist-selector (fn doc pathspec &body body)
  `(defun ,(intern (string-upcase fn)) ()
      ,doc
      (let ((filelist (directory-file-list ,@pathspec)))
        (let ((selected-file (select-from-menu
                              (current-screen)
                              (mapcar (lambda (pathname) (namestring pathname)) filelist))))
          (when (not (equal selected-file ""))
            ,@body)))))

(defun list-with-newlines (items)
  (format nil "~{~a~%~}" items))

(defun rofi-dmenu (items)
  (string-trim
   '(#\Newline)
   (run-shell-command
    (format nil "echo -e \"~a\" | rofi -dmenu"
            (list-with-newlines items)) t)))

(defmacro define-rofi-filelist-selector (fn doc pathspec &body body)
  `(defun ,(intern (string-upcase fn)) ()
      ,doc
      (let ((filelist (directory-file-list ,@pathspec)))
        (let ((selected-file (rofi-dmenu
                              (mapcar (lambda (pathname) (namestring pathname)) filelist))))
          (when (not (equal selected-file ""))
            ,@body)))))

(define-rofi-filelist-selector
    "select-layout-from-menu"
    "Select and apply saved window layout"
    (:subdir "layouts")
  (restore-group (current-group) (read-dump-from-file selected-file)))

(define-rofi-filelist-selector
    "select-books-from-menu"
    "Select from current virtual bookshelf"
    (:basedir "/home/octocat/bookshelf")
  (run-shell-command (format nil "~a \"~a\"" *PDF-VIEWER* selected-file) nil))

(defun open-in-browser (url &optional (background nil))
  (run-shell-command
   (cat
    *BROWSER*
    " " (format nil "~{~A~^ ~}" *BROWSER-PARAMS*) " "
    url)) ;FIXME: someway reorganize these browser commandlines
  (unless background
    (funcall (intern (string-upcase *BROWSER*)))))

(defun select-links-from-var (linkslist &key (with-captions nil))
  (let ((link (if with-captions
                  (cadr (select-from-menu (current-screen) linkslist))
                  (select-from-menu (current-screen) linkslist))))
    (when link
      (open-in-browser link))))

(defun global-pointer-position ()
  "Get global position of the mouse pointer."
  (xlib:global-pointer-position *display*))

(defun mouse-in-frame (frame)
  (multiple-value-bind (pointer-x pointer-y window)
      (global-pointer-position)
    (let* ((frame-start-x (frame-x frame))
           (frame-start-y (frame-y frame))
           (frame-end-x (+ frame-start-x (frame-width frame)))
           (frame-end-y (+ frame-start-y (frame-height frame))))
      (and (> pointer-x frame-start-x)
           (< pointer-x frame-end-x)
           (> pointer-y frame-start-y)
           (< pointer-y frame-end-y)))))

(defun mouse-location-in-frame (frame)
  (values
   (- (+ (frame-x frame) (frame-width frame)) 100)
   (+ (frame-y frame)
      (floor (/ (frame-height frame) 2)))))

(defun mouse-follow-focus (currentframe lastframe)
  (when *mouse-follows-focus*
    (let* ((current-frame currentframe)
           (current-frame-window (frame-window currentframe))
           (last-frame-window (frame-window lastframe)))
      (when (and
             (not (mouse-in-frame currentframe))
             last-frame-window
             current-frame-window
             (not (window-transient-p last-frame-window))
             (not (window-transient-p current-frame-window)))
        (multiple-value-bind (pointer-x pointer-y)
            (mouse-location-in-frame currentframe)
          (warp-pointer (current-screen) pointer-x pointer-y))))))
(add-hook *focus-frame-hook* 'mouse-follow-focus)

(defmacro define-list (pname clear &rest listdata)
  `(progn
    (unless (boundp ',pname)
      (defparameter ,pname nil))
    (when ,clear
      (setf ,pname nil))
    (dolist (item (quote ,listdata))
      (push item ,pname))
    (setf ,pname (reverse ,pname))))
