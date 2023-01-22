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



;;; * Heredoc + Friends -------------------------------------------------------------------------------------|

(deftest-local multiline-string-literals ()
    "
     Check `HERE-TEXT' joins text lines to a single string at compile time
"

  (explain "Concatenation with prefix and indentation")

  (let ((it (here-text (:indent 5 :prefix "| ")
	      "Hello world!"
	      "How are you?"
	      "The end.")
	    ))
    (trace-expr it)
    (assert-local (equal it
			 (format nil "     | Hello world!~%     | How are you?~%     | The end.~%"))))

  (explain "Concatenation with dedentation and prefix")

  (let ((it (here-text (:dedent 2 :prefix ":")
	      "   Hello world!"
	      "   How are you?"
	      "   The end.")
	    ))
    (assert-local (equal it
			 (format nil ": Hello world!~%: How are you?~%: The end.~%")))
    (trace-expr it))

  (explain "Concatenation with dedentation delimiter")

  (let ((it (here-text (:dedent-delimiter "| ")
	      "   | Hello world!"
	      "   |   How are you?"
	      "   | The end.")
	    ))
    (assert-local (equal it
			 (format nil "Hello world!~%  How are you?~%The end.~%")))
    (trace-expr it))

  (explain "Concatenating with a separator different from #\Newline")

  (let ((it (here-text (:separator " +++ ")
	      "Hello world!"
	      "How are you?"
	      "The end.")
	    ))
    (trace-expr it)
    (assert-local (equal it
			 (format nil "Hello world! +++ How are you? +++ The end. +++ "))))

  (explain "Omitting the seaparator from the end")

  (let ((it (here-text (:separator " +++ " :separator-at-end nil)
	      "Hello world!"
	      "How are you?"
	      "The end.")
	    ))
    (trace-expr it)
    (assert-local (equal it
			 (format nil "Hello world! +++ How are you? +++ The end."))))

  (explain "Trying the case of an empty list of lines")

  (let ((it (here-text ())))
    (trace-expr it)))

;;; * Wrapped streams ---------------------------------------------------------------------------------------|

(deftest-local indenting-a-char-stream ()
    "
    Checks: Indentation via `BASIC-INDENTING-CHARACTER-OUTPUT-STREAM'

    A stream wrapped into `BASIC-INDENTING-CHARACTER-OUTPUT-STREAM'
    can be used as output destination for `FORMAT'.
"

  (let* ((s (make-string-output-stream))
	 (w (indented-stream s :indent 4)))

    (explain "Formatting via BASIC-INDENTING-CHARACTER-OUTPUT-STREAM => STRING-OUTPUT-STREAM.")
    (format w "Hello,~%world!~%How are you?~%")

    (let ((result (get-output-stream-string s)))
      (trace-expr result)
      (assert-local (equal result
		     (format nil "    Hello,~%    world!~%    How are you?~%"))))))


(deftest-local capturing-output ()
    "
    Checks: Output can be captured with `WITH-CAPTURING-OUTPUT-IN'.
"
  (with-capturing-output
      (result
	(format t "Hello, world!")
	(format *error-output* " How are you?"))

    (trace-expr result)
    (assert-local (equal result "Hello, world! How are you?"))))


(deftest-local indenting-standard-out ()
    "
    Checks: Indenting `*STANDARD-OUTPUT*' and `*ERROR-OUTPUT*'.

    `WITH-INDENTED-OUTPUT' can be used to prefix and indent `*STANDARD-OUTPUT*' and `*ERROR-OUTPUT*'.  While
    executing a block wrapped inton the is macro, both streams will be redirected to what was
    `*STANDARD-OUTPUT*' just before the `WITH-INDENTED-OUTPUT' block.
"

  (with-capturing-output
      (result
	(with-indented-output (:indent 4)
	  (format t "Hello,~%world!~%How are you?~%")))
    (trace-expr result)
    (assert (equal result
		   (format nil "    Hello,~%    world!~%    How are you?~%")))))

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
