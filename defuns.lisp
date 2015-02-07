;;;
;;; File: defuns.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Среда, Февраль  6 2013
;;;
;;;
;;;

(defun defkey-top (key cmd)
  (define-key *top-map* (kbd key) cmd))

(defun defkey-root (key cmd)
  (define-key *root-map* (kbd key) cmd))

(defun defkey-in-map (keymap key cmd)
  (define-key keymap (kbd key) cmd))

(defmacro defkeys-top (&rest keys)
  (let ((ks (mapcar #'(lambda (k) (cons 'defkey-top k)) keys)))
    `(progn ,@ks)))

(defmacro defkeys-root (&rest keys)
  (let ((ks (mapcar #'(lambda (k) (cons 'defkey-root k)) keys)))
    `(progn ,@ks)))

(defmacro build-keymap (&rest keys)
  `(let ((m (make-sparse-keymap)))
     ,@(mapcar #'(lambda (k) `(defkey-in-map m ,(first k) ,(second k))) keys)
     m))

(defmacro populate-keymap (keymap &rest keys)
  (setf keymap
  `(let ((m (make-sparse-keymap)))
     ,@(mapcar #'(lambda (k) `(defkey-in-map m ,(first k) ,(second k))) keys)
     m)))

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
  (declare (ignore key val))
  (message "Key sequence: ~A" (print-key-seq (reverse seq))))

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

;TODO: actualize this copypaste
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

(defun fix-str-length (str length)
  (if (> (length str) length)
      (cat (subseq str 0 (- length 2)) ".*")
    (format nil "~va" length str)))

(defun current-window-title ()
  (let ((current-window (current-window)))
    (if current-window
        (window-title current-window)
        (cat "No Window In ::"
           (group-name (current-group)) "::"))))

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

(defun enable-mode-line-all-heads ()
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head t))))

(defun concat-as-symbol (prefix suffix)
  (intern (string-upcase (concatenate 'string prefix suffix))))

(defmacro restore-group-multihead-command (key relfilename)
  `(defcommand
       ,(concat-as-symbol "custom/restore-group-multihead-" key)
       () ()
     "Restore group windows placement for multihead setup"
     (let ((group-file (concatenate 'string  *STUMPWM-LIB-DIR* ,relfilename)))
       (cond ((not (probe-file group-file))
              (message "~s not found" group-file))
             (t
              (restore-group (current-group) (read-dump-from-file group-file))
              )))))

(defun update-emacs-frames ()
  (let ((heads-count (length (screen-heads (car *screen-list*)))))
    (when (find-matching-windows '(:class "Emacs") t t)
      (emacs)
      (send-meta-key (current-screen) (kbd "M-x"))
      (window-send-string "update-frames")
      (send-meta-key (current-screen) (kbd "RET"))
      (window-send-string (write-to-string heads-count))
      (send-meta-key (current-screen) (kbd "RET")))))

(defun directory-file-list (&key (basedir *STUMPWM-LIB-DIR*) (subdir nil))
  (let ((pathspec (if subdir
                      (concatenate 'string basedir "/" subdir)
                      basedir)))
    (directory
     (make-pathname
      :directory `(:absolute ,@(split-seq pathspec "/"))
      :name :wild :type :wild))))


(defmacro define-filelist-selector (fn doc pathspec &body body)
  `(defun ,(intern (string-upcase fn)) ()
      ,doc
      (let ((filelist (directory-file-list ,@pathspec)))
        (let ((selected-file (select-from-menu
                              (current-screen)
                              (mapcar (lambda (pathname) (namestring pathname)) filelist))))
          (when selected-file ;TODO: make more robust
            ,@body)))))

(define-filelist-selector
    "select-layout-from-menu"
    "Select and apply saved window layout"
    (:subdir "layouts")
  (restore-group (current-group) (read-dump-from-file selected-file)))

(define-filelist-selector
    "select-books-from-menu"
    "Select from current virtual bookshelf"
    (:basedir "/home/octocat/bookshelf")
  (run-shell-command (format nil "~a \"~a\"" *PDF-VIEWER* selected-file) nil))

(defun open-in-browser (url &optional (background nil))
  (run-shell-command
   (concatenate 'string
                *BROWSER*
                " " (format nil "~{~A~^ ~}" *BROWSER-PARAMS*) " "
                url)) ;FIXME: someway reorganize these browser commandlines
  (unless background
    (funcall (intern (string-upcase *BROWSER*)))))

(defun select-links-from-var (linkslist)
  (let ((link (select-from-menu
                    (current-screen)
                    linkslist)))
    (when link
      (open-in-browser link))))
