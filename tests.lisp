;;; ------------------------------------------------------------------------*- common-lisp -*-|
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

;;; * Options -----------------------------------------------------------------------------------------------|

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Define package ----------------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-simple-utils/tests
  (:documentation "Testing cl-simple-utils")
  (:use
   :common-lisp
   :de.m-e-leypold.cl-simple-utils
   :de.m-e-leypold.cl-simple-utils/basic-test
   :de.m-e-leypold.cl-simple-utils/wrapped-streams)
  (:export
   ))

(in-package :de.m-e-leypold.cl-simple-utils/tests)

(inject-package-local-nickname "TEST"
			       :de.m-e-leypold.cl-simple-utils/tests
			       :de.m-e-leypold.cl-simple-utils)


;;; * Wrapped streams ---------------------------------------------------------------------------------------|

(deftest-local indenting-a-char-stream ()
  "
  Checks: Indentation via `BASIC-INDENTING-CHARACTER-OUTPUT-STREAM
"

  (let* ((s (make-string-output-stream))
	 (w (indented-stream s :indent 4)))

    (explain "Formatting via BASIC-INDENTING-CHARACTER-OUTPUT-STREAM => STRING-OUTPUT-STREAM.")
    (format w "Hello,~%world!~%How are you?~%")

    (let ((result (get-output-stream-string s)))
      (trace-expr result)
      (assert (equal result
		     (format nil "    Hello,~%    world!~%    How are you?~%"))))))

;;; * -------------------------------------------------------------------------------------------------------|
;;;   WRT the outline-* and comment-* variables, see the comment in test.lisp
;;;
;;; Local Variables:
;;; eval: (progn (outshine-mode 1) (column-enforce-mode 1) (toggle-truncate-lines 1))
;;; fill-column: 110
;;; column-enforce-column: 110
;;; outline-regexp: ";;; [*]\\{1,8\\} "
;;; comment-add: 2
;;; End:
