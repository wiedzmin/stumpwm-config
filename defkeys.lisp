;;;
;;; File: defkeys.lisp
;;; Author: aermolov <aaermolov@gmail.com>
;;;
;;; Created: Среда, Март 13 2013
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
