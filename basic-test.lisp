;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-simple-test -- a simple testing framework for common lisp.
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
   :run-tests-local
   :deftest-local
   :assert-local
   :explain
   :trace-expr
   :*indentation-step*
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

(defvar *tests-local* '())
(defvar *current-test-local* nil)

(defmacro assert! (cond)
  `(progn
     (format t "~&=> Checking: ~S.~%" (quote ,cond))
     (if ,cond
	 t
	 (progn
	   (format t "  *** Check failed in ~S ***~%" *current-test-local*)
	   (error 'test-failure
		  :test-name *current-test-local*
		  :failed-condition (quote ,cond))))))

(defmacro assert-local (cond)
  `(assert! ,cond))

(defun test-failure (&key failed-condition explanation)
  (error 'test-failure
	 :test-name *current-test-local*
	 :failed-condition failed-condition
	 :explanation explanation))


(defvar *indentation-step* 4)

(defmacro deftest! (name args docstring &body body)
  (assert (not args) nil (format nil "arguments of DEFTEST! ~S must be empty" name))
  `(progn
     (setf *tests-local* (adjoin (quote ,name) *tests-local*))
     (defun ,name ()
     ,docstring
       (let ((*current-test-local* (quote ,name)))
	 (format t "~&~a: Test ~S ...~%" *program-name* *current-test-local*)
	 (format t "~&~a~%" (documentation (quote ,name) 'function))
	 (with-indented-output (:indent *indentation-step*)
	   (progn ,@body))
	 (format t "~%~a: ... => OK (~S).~%" *program-name* *current-test-local*)))))

(defmacro deftest-local (name args docstring &body body)
  `(deftest! ,name ,args ,docstring ,@body))

(defun run-tests! ()
  (format t "~&~%~a: Will run ~a tests: ~S.~%"
	  *program-name* (length *tests-local*) (reverse *tests-local*))
  (format t "  First failing test will abort this test run.~%")
  (dolist (test (reverse *tests-local*))
    (format t "~%")
    (funcall test))
  (format t "~%~a: All ~a tests succeeded: ~a.~%"
	  *program-name* (length *tests-local*) (reverse *tests-local*)))

(defmacro run-tests-local ()
  `(run-tests!))

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
