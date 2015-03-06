;;;
;;; File: defuns.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Среда, Февраль  6 2013
;;;
;;;
;;;

(defmacro define-keys (keymap &rest keys)
  `(dolist (keydef (quote ,keys))
     (define-key ,keymap (kbd (car keydef)) (cadr keydef))))

(defmacro build-keymap (&rest keys)
  `(let ((keymap (make-sparse-keymap)))
     (define-keys keymap ,@keys)
     keymap))

(defun screenshot-filename ()
  (cat
   "~/screenshots/screenshot-"
   (run-shell-command "date +\"%d-%m-%Y-%T\" | tr -d '[:cntrl:]'" t)
   ".png"))

(defun cat (&rest strings) ; "Concatenates strings, like the Unix command 'cat'. A shortcut for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))

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

(defun enable-mode-line-all-heads ()
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head t))))

(defun concat-as-symbol (prefix suffix)
  (intern (string-upcase (cat prefix suffix))))

(defmacro with-emacs (&body body)
  `(when (find-matching-windows '(:class "Emacs") t t)
     (emacs)
     (progn ,@body)))

(defun update-emacs-frames ()
  (let ((heads-count (length (screen-heads (car *screen-list*)))))
    (with-emacs
      (send-meta-key (current-screen) (kbd "M-x"))
      (window-send-string "update-frames")
      (send-meta-key (current-screen) (kbd "RET"))
      (window-send-string (write-to-string heads-count))
      (send-meta-key (current-screen) (kbd "RET")))))

(defun directory-file-list (&key (basedir *STUMPWM-LIB-DIR*) (subdir nil))
  (let ((pathspec (if subdir
                      (cat basedir "/" subdir)
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
   (cat
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
