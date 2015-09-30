(in-package #:stumpwm)

(defun at-homedir (&optional suffix)
  (concatenate 'string (namestring (user-homedir-pathname)) suffix))

(defun find-subpath (path substring)
  (car
   (loop for dir in (directory path)
      when (search substring (namestring dir))
      collect (namestring dir))))

(defparameter *SLIME-DIR* (find-subpath (at-homedir ".emacs.d/elpa/*/") "/slime-"))
(defparameter *CL-USER-DIR* (at-homedir ".commonlisp/"))
(defparameter *STUMPWM-LIB-DIR* (at-homedir ".stumpwm.d/"))
(defparameter *STUMPWM-GIT-DIR* (at-homedir ".commonlisp/stumpwm-git/"))

(defun load-config-module (name)
  (load (concatenate 'string *STUMPWM-LIB-DIR* name)))

(load (concatenate 'string *SLIME-DIR* "swank-loader.lisp"))
(swank-loader:init) ;; Load swank.;; *prefix-key* ; swank will kick this off

(init-load-path (concatenate 'string *CL-USER-DIR* "stumpwm-contrib/"))

(load-module "battery")
(load-module "cpu")
(load-module "disk")
(load-module "mem")
(load-module "net")
(load-module "wifi")
(load-module "mpd")
(load-module "screenshot")
(load-module "notifications")
(load-module "ttf-fonts")
(load-module "globalwindows")
(load-module "urgentwindows")
(load-module "searchengines")
(load-module "perwindowlayout")

(load-config-module "constants.lisp")
(load-config-module "variables.lisp")
(load-config-module "defuns.lisp")
(load-config-module "commands.lisp")
(load-config-module "keydefs.lisp")
(load-config-module "searches.lisp")
(load-config-module "private.lisp")

(setf *emacs-toggle-input-method-key* "C-\\")
;; (xft:cache-fonts)
(set-font (make-instance 'xft:font :family "Consolas" :subfamily "Regular" :size 10))
(enable-mode-line-all-heads)
(run-commands "enable-per-window-layout" "mpd-connect")
