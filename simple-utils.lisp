;;; ---------------------------------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-simple-utils -- Some utility functions.
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
;;; * -- Options --------------------------------------------------------------------------------------------|
;;;
;;;   Will be changed to defaults when cl-simple-test has reached sufficient maturity.

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;;; * -- Package definition ---------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-simple-utils
  (:documentation
   "
   Simple utilities for DE.M-E-LEYPOLD.*")
  (:use :common-lisp)
  (:export
   :inject-package-local-nickname
   :defpackage-doc
   :concatenate-lines
   :here-text
   ))

(in-package :de.m-e-leypold.cl-simple-utils)

;;; * -- Injecting package local nicknames ------------------------------------------------------------------|
;;;
;;;   Note: Currently there is only an implementation for SBCL. In future we'll either find
;;;         implementations for other lisp implementations or map the case of a missing
;;;         implementation to a warning (a drop to the debugger with a restart). The major use
;;;         case for this is to allow loading specification or test modules T after the module
;;;         M they test and still be able to reference the test module T with a short name from
;;;         the tested / specified module M. It makes for uncomfortable development if this
;;;         function is not there (because references like "see SPEC::FOOBAR" are just not
;;;         working for source navigation, but it does not inhibit use of M.
;;;
;;;   TODO: Find a implementation for other lisp implementations or convert (ASSERT NIL ...)
;;;         with restarts to an ignorable warning.

(defun inject-package-local-nickname (nickname for-package to-package)
  "
  Inject package local nickname NICKNAME for package FOR-PACKAGE into TO-PACKAGE.
"
  #+sbcl (let ((to-p (find-package to-package)))
	   (assert (packagep to-p))
	   (let ((*PACKAGE* to-p))
	     (sb-ext:add-package-local-nickname nickname for-package)))

  #-(or sbcl)
  (assert nil nil "No implementation for INJECT-PACKAGE-LOCAL-NICKNAME available." ))

;;; * -- Package docstrings as symbols ----------------------------------------------------------------------|

(defmacro defpackage-doc (&optional (sym))
  (format T "sym => ~D~%" sym)
  (if (not sym)
      (setf sym (intern "DOC" *package*)))  
  (let ((docstring (documentation *package* T)))
    `(defvar ,sym (format nil "#<DEFPACKAGE-DOC anchor for ~a>" (package-name *package*)) ,docstring)))

;;; * -- Concatenating lines to text blocks / paragraphs, here-text -----------------------------------------|


(defun concatenate-lines (lines
			   &key
			     prefix (indent 0)
			     dedent-delimiter (dedent 0)
			     (separator #\Newline) (separator-at-end T))
  (if (not prefix)
      (setf prefix (make-sequence 'string indent :initial-element #\Space))
      (if (> indent 0)
	  (setf prefix
		(concatenate 'string
			     (make-sequence 'string indent :initial-element #\Space) prefix))))
  (with-output-to-string (s)
    (do* ((line (car lines) (car rest))
	  (rest (cdr lines) (cdr rest)))
	  ((not line) T)
      (if dedent-delimiter
	  (let ((pos (search dedent-delimiter line)))
	    (if pos (setf dedent (+ pos (length dedent-delimiter))))))
      (if (> dedent 0)
	  (setf line (subseq line dedent)))
      (format s "~A~A" prefix line)
      (if (or separator-at-end rest)
	  (format s "~A" separator)))))


(defmacro here-text ((&key
			prefix (indent 0)
			dedent-delimiter (dedent 0)
			(separator #\Newline) (separator-at-end T))
		     &body lines)
  (funcall #'concatenate-lines lines
	   :prefix prefix
	   :indent indent
	   :dedent-delimiter dedent-delimiter
	   :dedent dedent
	   :separator separator
	   :separator-at-end separator-at-end
	   ))
