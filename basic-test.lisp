;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-simple-utils -- basic-test, a very simple test framework
;;;   Copyright (C) 2022  M E Leypold
;;;
;;;   This program is free software: you can redistribute it and/or modify
;;;   it under the terms of the GNU General Public License as published by
;;;   the Free Software Foundation, either version 3 of the License, or
;;;   (at your option) any later version.
;;;
;;;   This program is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;   GNU General Public License for more details.
;;;
;;;   You should have received a copy of the GNU General Public License
;;;   along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;;   For alternative licensing options, see README.md
;;;
;;;
;;; * Options -----------------------------------------------------------------------------------------------|

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Define package ----------------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-simple-utils/basic-test
  (:documentation "Testing cl-simple-utils")
  (:use
   :common-lisp
   :de.m-e-leypold.cl-simple-utils
   :de.m-e-leypold.cl-simple-utils/wrapped-streams)
  (:export
   :run-tests!
   :assert!
   :deftest!
   :explain
   :trace-expr
   :*indentation-step*
   :*program-name*
   :*tests*
   :*current-test*
   :*flags*
   :set-flag
   :flag-set-p
   :clear-flags
   :test-failure
   :*test-directories*
   :deftest-registry!
   :end-test-registry!
   ))

(in-package :de.m-e-leypold.cl-simple-utils/basic-test )

;;; * A very simple, untested test framework ----------------------------------------------------------------|

(defparameter *program-name* "tests.lisp")

(define-condition test-failure (condition)
  ((test-name
    :reader   test-name
    :initarg  :test-name
    :initform nil
    )
   (failed-condition
    :reader  failed-condition
    :initarg :failed-condition
    :initform nil)
   (explanation
    :reader  explanation
    :initarg :explanation
    :initform nil)))

(defmethod print-object ((failure test-failure) stream)

  "Print a `TEST-FAILURE' instance."

  (print-unreadable-object (failure stream :type t)
    (format stream ":test-name ~a" (test-name failure))
    (if (failed-condition failure)
	(format stream " :cond ~S" (failed-condition failure)))
    (if (explanation failure)
	(format stream " :explanation ~S" (explanation failure)))))

(defvar *tests* '())
(defvar *current-test* nil)

(defmacro assert! (cond)
  `(progn
     (format t "~&=> Checking: ~S.~%" (quote ,cond))
     (if ,cond
	 t
	 (progn
	   (format t "  *** Check failed in ~S ***~%" *current-test*)
	   (error 'test-failure
		  :test-name *current-test*
		  :failed-condition (quote ,cond))))))


(defun test-failure (&key failed-condition explanation)
  (error 'test-failure
	 :test-name *current-test*
	 :failed-condition failed-condition
	 :explanation explanation))


(defvar *test-directories*'())

(defvar *indentation-step* 4)

(defmacro deftest-registry! (&optional (sym))
  (if (not sym) ;; TODO: Or symbol-package != *package*
      (setf sym (intern "*REGISTRY*" *package*)))
  `(progn
     (defvar ,sym '() ,(format nil "Test registry defined by `DEFTEST-REGISTRY!' for package ~S" *package*))
     (setf *test-directories* (adjoin (quote ,sym) *test-directories*))))

(defmacro end-test-registry! (&optional (sym))
  (if (not sym) ;; TODO: Or symbol-package != *package*
      (setf sym (intern "*REGISTRY*" *package*)))
  (let ((newline (format nil "~%")))
    `(progn
       (setf *test-directories* (remove (quote ,sym) *test-directories*))
       (let ((fragments '()))
	 (dolist (testsym ,sym)
	   (push ,newline fragments)
	   (push (documentation testsym 'function) fragments)
	   (push ,newline fragments)
	   (let ((*package* (find-package "KEYWORD"))) (push (format nil "~S" testsym) fragments))
	   (push " - " fragments)
	   (push (symbol-name testsym) fragments))
	 (setf (documentation (quote ,sym) 'variable) (apply #'concatenate 'string fragments))))))


;;;       (dotimes (testsym (symbol-value sym))
;;; 	(push (documentation testsym 'function) fragments)
;;; 	(push (symbol-name testsym) fragments)))))
;;;       ;(funcall #'concatenate 'string fragments))))

(defun register-test (sym)
  (dolist (dir *test-directories*)
    (if (equal (symbol-package sym) (symbol-package dir))
	(set dir (adjoin sym (symbol-value dir))))))

(defmacro deftest! (name args docstring &body body)
  (assert (not args) nil (format nil "arguments of DEFTEST! ~S must be empty" name))
  `(progn
     (setf *tests* (adjoin (quote ,name) *tests*))
     (defun ,name ()
     ,docstring
       (let ((*current-test* (quote ,name)))
	 (format t "~&~a: Test ~S ...~%" *program-name* *current-test*)
	 (format t "~&~a~%" (documentation (quote ,name) 'function))
	 (with-indented-output (:indent *indentation-step*)
	   (progn ,@body))
	 (format t "~%~a: ... => OK (~S).~%" *program-name* *current-test*)))
     (register-test (quote ,name))))

(defun run-tests! ()
  (format t "~&~%~a: Will run ~a tests: ~S.~%"
	  *program-name* (length *tests*) (reverse *tests*))
  (format t "  First failing test will abort this test run.~%")
  (dolist (test (reverse *tests*))
    (format t "~%")
    (funcall test))
  (format t "~%~a: All ~a tests succeeded: ~a.~%"
	  *program-name* (length *tests*) (reverse *tests*)))

(defvar *flags* '()
  "A variable into which symbols will be adjoined to trace that forms have actually been evaluated.

   See `SET-FLAG'.")

(defun set-flag (name)
  (setf *flags* (adjoin name *flags*)))

(defun flag-set-p (name)
  (find name *flags*))

(defun clear-flags ()
  (setf *flags* '()))

(defun explain (message)
  (format t "~&=> ~a~%" message))

(defmacro trace-expr (expr)
  (let ((package *package*))
    `(let ((*package* ,package))
       (format t "~&~S => ~S~%" (quote ,expr) ,expr))))

;;; * -------------------------------------------------------------------------------------------------------|
;;; WRT the outline-* and comment-* variables, see the comment in test.lisp
;;;
;;; Local Variables:
;;; eval: (progn (outshine-mode 1) (column-enforce-mode 1) (toggle-truncate-lines 1))
;;; fill-column: 110
;;; column-enforce-column: 110
;;; outline-regexp: ";;; [*]\\{1,8\\} "
;;; comment-add: 2
;;; End:
