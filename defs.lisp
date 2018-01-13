(in-package #:stumpwm)

(defstruct browser name executable cliargs)
(defstruct (persistent-setup (:conc-name psetup-)) default-browser ext-head-rotated)

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
(defparameter *PDF-VIEWER* "zathura"
  "Default PDF viewer")
(defparameter *mouse-follows-focus* nil
  "Should mouse pointer follow window focus?")
(defparameter *internal-head-initial-height* (head-height (nth 0 (screen-heads (current-screen)))))
(defparameter *tray-height* 15)
(defparameter *available-browsers*
  `(("Firefox"
     ,(make-browser
       :name "Firefox"
       :executable "firefox"
       :cliargs '("-new-tab")))
    ("Palemoon"
     ,(make-browser
       :name "Palemoon"
       :executable "palemoon-bin"
       :cliargs '("-new-tab")))
    ("Google Chrome"
     ,(make-browser
       :name "Google Chrome"
       :executable "google-chrome-stable"
       :cliargs '("--new-tab")))))
(defparameter *persistent-setup-file* (concatenate 'string *STUMPWM-LIB-DIR* "persistent-setup"))
(defparameter *persistent-setup* nil)

(defun init-persistent-setup ()
  (setf *persistent-setup*
        (make-persistent-setup
         :default-browser (cadar *available-browsers*)
         :ext-head-rotated nil)))

(defun save-persistent-setup ()
  (dump-to-file *persistent-setup* *persistent-setup-file*))

(defun load-persistent-setup ()
  (handler-case (setf *persistent-setup* (read-dump-from-file *persistent-setup-file*))
    (error (e)
      (progn
        (message "Encountered error: ~a~%Falling back to defaults." e)
        (init-persistent-setup)
        (save-persistent-setup))))
  *persistent-setup*)

(add-hook *quit-hook* 'save-persistent-setup)

(defmacro define-keys (keymap &rest keys)
  `(dolist (keydef (quote ,keys))
     (define-key ,keymap (kbd (car keydef)) (cadr keydef))))

(defmacro build-keymap (&rest keys)
  `(let ((keymap (make-sparse-keymap)))
     (define-keys keymap ,@keys)
     keymap))

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

(defparameter *applications* (make-hash-table :test #'equal))

(defmacro define-application (name &key (command (string-downcase (string name)))
                                     (class (string-capitalize command) )
                                     (instance nil)
                                     (title nil)
                                     (map '*top-map*)
                                     (key (subseq command 0 1))
                                     (pullp nil)
                                     (pull-map nil)
                                     (pull-name (intern1 (concat "p-" (string name))))
                                     (pull-key (subseq command 0 1))
                                     (binded t))
  "Define a command and key binding to run or raise a program. If
@var{pullp} is set, also define a command and key binding to run or
pull the program."
  `(progn
     (define-key ,map (kbd ,key) nil)
     (defcommand ,name () ()
       ,(format nil "Start ~a unless it is already running, ~
in which case focus it."
                name)
       (run-or-raise ,command '(:class ,class :title ,title)))
     ,(when binded
            `(define-key ,map (kbd ,key) ,(string-downcase (string name))))
     ,(when pullp
            `(progn
               (define-key ,pull-map (kbd ,pull-key) nil)
               (defcommand (,pull-name tile-group) () ()
                 ,(format nil "Start ~a unless it is already running, ~
in which case pull it into the current frame."
                          name)
                 (run-or-pull ,command '(:class ,class :instance ,instance :title ,title)))
               ,(when binded
                      `(define-key ,pull-map (kbd ,pull-key) ,(string-downcase (string pull-name))))))
     (setf (gethash ',command *applications*)
           #',name
           )))

(defmacro defwebjump (caption url &key (map *web-keymap*) (key nil) (binded t) (browser nil))
  (let ((command-name (concat-as-symbol "custom/open-" (string-downcase (substitute #\- #\Space caption))))
        (browserobj (get-browser-by-name browser)))
    `(progn
       ,(when (and binded
                   key)
              `(define-key ,map (kbd ,key) nil))
       (defcommand
           ,command-name () ()
         ,(format nil "Open ~a" caption)
         ,(if browserobj
              `(open-in-browser ,url :browser ,browserobj)
              `(open-in-browser ,url))
         )
       ,(when (and binded
                   key)
              `(define-key ,map (kbd ,key) ,(string-downcase command-name))))))

(defun enable-mode-line-all-heads ()
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head t))))

(defun with-emacs-noninteractive (body)
  (when (find-matching-windows '(:class "Emacs") t t) ;TODO: is running Emacs instance really needed here?
    (run-shell-command
     (string-downcase
      (format nil "emacsclient --eval '~a'" (prin1 `(progn ,@body)))))
    (unless (search "emacs" (window-title (current-window)))
      (emacs))))

(defun update-emacs-frames ()
  (let ((heads-count (length (screen-heads (car *screen-list*)))))
    (with-emacs-noninteractive
      `((custom/update-frames ,heads-count)))))

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
      (let ((filelist (mapcar (lambda (pathname) (namestring pathname))
                              (directory-file-list ,@pathspec))))
        (let ((selected-file (select-from-menu (current-screen) filelist)))
          (when (not (equal selected-file ""))
            ,@body)))))

(defmacro define-filelist-selector-recursive (fn doc path filterfn &body body)
  `(defun ,(intern (string-upcase fn)) ()
      ,doc
      (let ((filelist nil))
        (cl-fad:walk-directory ,path (lambda (fname) (push (namestring fname) filelist)) :test ,filterfn)
        (let ((selected-file (select-from-menu (current-screen) filelist)))
          (when (not (equal selected-file ""))
            ,@body)))))

(defun list-with-newlines (items)
  (format nil "~{~a~%~}" items))

(defun rofi-dmenu (items)
  (string-trim
   '(#\Newline)
   (run-shell-command
    (format nil "echo -e \"~a\" | rofi -dmenu -i"
            (list-with-newlines items)) t)))

(defmacro define-rofi-filelist-selector (fn doc pathspec &body body)
  `(defun ,(intern (string-upcase fn)) ()
      ,doc
      (let ((filelist (mapcar (lambda (pathname) (namestring pathname)) (directory-file-list ,@pathspec))))
        (let ((selected-file (rofi-dmenu filelist)))
          (when (not (equal selected-file ""))
            ,@body)))))

(defmacro define-rofi-filelist-selector-recursive (fn doc path filterfn &body body)
  `(defun ,(intern (string-upcase fn)) ()
      ,doc
      (let ((filelist nil))
        (cl-fad:walk-directory ,path (lambda (fname) (push (namestring fname) filelist)) :test ,filterfn)
        (let ((selected-file (rofi-dmenu filelist)))
          (when (not (equal selected-file ""))
            ,@body)))))

(define-rofi-filelist-selector
    "select-layout-from-menu"
    "Select and apply saved window layout"
    (:subdir "layouts")
  (restore-group (current-group) (read-dump-from-file selected-file)))

(defparameter *ebook-formats*
  '("pdf" "djvu")
  "ebook formats to consider")

(define-rofi-filelist-selector-recursive
    "select-books-from-menu"
    "Select from current virtual bookshelf"
    "/home/octocat/bookshelf"
    (lambda (filespec) (member (pathname-type filespec) *ebook-formats* :test #'equal))
  (run-shell-command (format nil "~a \"~a\"" *PDF-VIEWER* selected-file) nil))

(defun select-links-from-var (linkslist &key (with-captions nil))
  (let ((link (if with-captions
                  (cadr (select-from-menu (current-screen) linkslist))
                  (select-from-menu (current-screen) linkslist))))
    (when link
      (open-in-browser link))))

(defun select-links-from-var-rofi (linkslist &key (with-captions nil))
  (let ((link (if with-captions
                  (cadr (assoc (rofi-dmenu (mapcar #'(lambda (item) (car item)) linkslist)) linkslist :test #'string=))
                  (select-from-menu (current-screen) linkslist))))
    (when link
      (open-in-browser link))))

(defun global-pointer-position ()
  "Get global position of the mouse pointer."
  (xlib:global-pointer-position *display*))

(defun mouse-in-frame (frame)
  (multiple-value-bind (pointer-x pointer-y window)
      (global-pointer-position)
    (declare (ignore window))
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
    (let* ((current-frame-window (frame-window currentframe))
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

(defun get-browser-by-name (name)
  (cadr (assoc name *available-browsers* :test #'equalp)))

;;TODO: incapsulate/relocate/improve
(defparameter *default-browser-changed-hook* '())

(defun set-default-browser ()
  (let ((browser (select-from-menu
                  (current-screen)
                  (mapcar (lambda (pair) (car pair)) *available-browsers*))))
    (when browser
      (setf (psetup-default-browser *persistent-setup*) (get-browser-by-name browser)))
    (when *default-browser-changed-hook*
      (dolist (func *default-browser-changed-hook*)
        (funcall func)))))

(defun open-in-browser (url &key (background nil) (browser (psetup-default-browser *persistent-setup*)))
  (let ((browser-program (browser-executable browser))
        (browser-args (browser-cliargs browser)))
    (run-shell-command
     (cat
      browser-program
      " " (format nil "~{~A~^ ~}" browser-args)
      " " url))
    (unless background
      (funcall (gethash (string-downcase browser-program) *applications*)))))

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

(add-hook *post-command-hook* (lambda (command)
                                (when (member command winner-mode:*default-commands*)
                                  (winner-mode:dump-group-to-file))))

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
  (xlib:lock-group *display* :group 0)
  (run-shell-command "i3lock -c 232729 && sleep 1 && xset dpms force off")) ;; TODO: maybe use template value for CLI command

(defcommand warp-mouse-active-frame () ()
  (let* ((current-frame (tile-group-current-frame (current-group)))
         (pointer-x (- (+ (frame-x current-frame) (frame-width current-frame)) 100))
         (pointer-y (+ 100 (frame-y current-frame))))
    (warp-pointer (current-screen) pointer-x pointer-y)))

(defcommand toggle-external-head-rotation () ()
  (setf (psetup-ext-head-rotated *persistent-setup*)
        (not (psetup-ext-head-rotated *persistent-setup*)))
  (disable-external-monitor)
  (save-persistent-setup))

(defcommand enable-external-monitor-right () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --right-of LVDS1" nil)
  (when (psetup-ext-head-rotated *persistent-setup*)
    (run-shell-command "xrandr --output VGA1 --rotate left" nil))
  (setf *heads-updated* nil)
  (save-persistent-setup))

(defcommand enable-external-monitor-left () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --left-of LVDS1" nil)
  (when (psetup-ext-head-rotated *persistent-setup*)
    (run-shell-command "xrandr --output VGA1 --rotate left" nil))
  (setf *heads-updated* nil)
  (save-persistent-setup))

(defcommand enable-external-monitor-above () ()
  "Enables external monitor"
  (run-shell-command "xrandr --output VGA1 --auto --above LVDS1" nil)
  (setf *heads-updated* nil)
  (save-persistent-setup))

(defcommand disable-external-monitor () ()
  "Disables external monitor"
  (run-shell-command "xrandr --output VGA1 --off")
  (warp-mouse-active-frame)
  (setf *heads-updated* nil)
  (save-persistent-setup))

;TODO: fix "above" config as it fails to navigate windows after calling 'resize-heads
(defcommand resize-heads () ()
  "Resizes primary head (to see tray)"
  (let ((internal-head (nth 0 (screen-heads (current-screen)))))
    (unless *heads-updated*
      (resize-head 0
                   (head-x internal-head)
                   *tray-height*
                   (head-width internal-head)
                   (- *internal-head-initial-height* *tray-height*))
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
  (run-shell-command "sudo /etc/init.d/job-vpn start"))

(defcommand custom/stop-job-vpn () ()
  (run-shell-command "sudo /etc/init.d/job-vpn stop"))

(defcommand custom/job-vpn-status () ()
  (format nil "job VPN: ~a"
          (car (last (split-seq (run-shell-command "sudo /etc/init.d/job-vpn status" t) " ")))))

(defcommand custom/start-sshuttle () ()
  (run-shell-command "sudo /etc/init.d/sshuttle start"))

(defcommand custom/stop-sshuttle () ()
  (run-shell-command "sudo /etc/init.d/sshuttle stop"))

(defcommand custom/sshuttle-status () ()
  (format nil "sshuttle: ~a"
          (car (last (split-seq (run-shell-command "sudo /etc/init.d/sshuttle status" t) " ")))))

(defcommand custom/restart-job-vpn () ()
  (custom/stop-job-vpn)
  (custom/start-job-vpn))

(defcommand custom/rofi-windowlist () ()
  (run-shell-command "rofi -show window"))

(defcommand custom/bother-stuck-emacs () ()
  (run-shell-command "pkill -SIGUSR2 emacs"))

(defcommand custom/set-default-browser () ()
  (set-default-browser)
  (save-persistent-setup))

(defcommand current-browser () ()
  "Echo current browser"
  (echo-string (current-screen) (browser-name (psetup-default-browser *persistent-setup*))))

(defcommand custom/spawn-emacs-frame () ()
  (run-shell-command "emacsclient -c -n -e '(switch-to-buffer nil)'"))

(defcommand custom/statusbar-osd () ()
  (run-shell-command "statusbar_osd"))

(defcommand custom/sbcl-send-exit () () ;TODO: generalize
  (window-send-string "(exit)"))

(defcommand custom/restart-wifi () ()
  (run-shell-command "sudo nmcli radio wifi off && sudo nmcli radio wifi on"))
