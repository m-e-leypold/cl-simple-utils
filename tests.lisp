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

(deftest! multiline-string-literals ()
    "
    `HERE-TEXT' joins text lines to a single string at compile time.
"

  (explain "Concatenation with prefix and indentation")

  (let ((it (here-text (:indent 5 :prefix "| ")
	      "Hello world!"
	      "How are you?"
	      "The end.")
	    ))
    (trace-expr it)
    (assert! (equal it
			 (format nil "     | Hello world!~%     | How are you?~%     | The end.~%"))))

  (explain "Concatenation with dedentation and prefix.")

  (let ((it (here-text (:dedent 2 :prefix ":")
	      "   Hello world!"
	      "   How are you?"
	      "   The end.")
	    ))
    (assert! (equal it
			 (format nil ": Hello world!~%: How are you?~%: The end.~%")))
    (trace-expr it))

  (explain "Concatenation with dedentation delimiter.")

  (let ((it (here-text (:dedent-delimiter "| ")
	      "   | Hello world!"
	      "   |   How are you?"
	      "   | The end.")
	    ))
    (assert! (equal it
			 (format nil "Hello world!~%  How are you?~%The end.~%")))
    (trace-expr it))

  (explain "Concatenating with a separator different from #\Newline.")

  (let ((it (here-text (:separator " +++ ")
	      "Hello world!"
	      "How are you?"
	      "The end.")
	    ))
    (trace-expr it)
    (assert! (equal it
			 (format nil "Hello world! +++ How are you? +++ The end. +++ "))))

  (explain "Omitting the separator from the end.")

  (let ((it (here-text (:separator " +++ " :separator-at-end nil)
	      "Hello world!"
	      "How are you?"
	      "The end.")
	    ))
    (trace-expr it)
    (assert! (equal it
			 (format nil "Hello world! +++ How are you? +++ The end."))))

  (explain "Trying the case of an empty list of lines.")

  (let ((it (here-text ())))
    (trace-expr it)))

;;; * Wrapped streams ---------------------------------------------------------------------------------------|

(deftest! indenting-a-char-stream ()
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
      (assert! (equal result
		     (format nil "    Hello,~%    world!~%    How are you?~%"))))))


(deftest! capturing-output ()
    "
    Checks: Output can be captured with `WITH-CAPTURING-OUTPUT-IN'.
"
  (with-capturing-output
      (result
	(format t "Hello, world!")
	(format *error-output* " How are you?"))

    (trace-expr result)
    (assert! (equal result "Hello, world! How are you?"))))


(deftest! indenting-standard-out ()
    "
    Checks: Indenting `*STANDARD-OUTPUT*' and `*ERROR-OUTPUT*'.

    `WITH-INDENTED-OUTPUT' can be used to prefix and indent `*STANDARD-OUTPUT*' and `*ERROR-OUTPUT*'.  While
    executing a block wrapped into this macro, both streams will be redirected to what was
    `*STANDARD-OUTPUT*' just before the `WITH-INDENTED-OUTPUT' block.
"

  (with-capturing-output
      (result
	(with-indented-output (:indent 4)
	  (format t "Hello,~%world!~%How are you?~%")))
    (trace-expr result)
    (assert! (equal result
		   (format nil "    Hello,~%    world!~%    How are you?~%")))))


;;; * Symbol names ------------------------------------------------------------------------------------------|

(deftest! full-symbol-names ()
    "
    Checks that `SYMBOL-FULL-NAME' returns a full name (with package name) and format with ~S works the same
    in a `WITH-FULL-SYMBOL-NAMES'.
"

  (assert! (equal (symbol-full-name 'SOME-SYMBOL)
		  "DE.M-E-LEYPOLD.CL-SIMPLE-UTILS/TESTS::SOME-SYMBOL"))

  (assert! (equal (with-full-symbol-names (format nil "- ~S -" 'SOME-SYMBOL))		 
		  "- DE.M-E-LEYPOLD.CL-SIMPLE-UTILS/TESTS::SOME-SYMBOL -")))


(deftest! downcasing-symbol-names ()
    "
    Checks `DOWNCASE-SYMBOL-NAME' and indirectly `WITH-DOWNCASE-SYMBOLS'
"
  (assert! (equal (downcase-symbol-name 'foobar)
		  "foobar")))

;;; * Basic Test --------------------------------------------------------------------------------------------|
;;; ** Test registry ----------------------------------------------------------------------------------------|
;;;
;;;    The following test is a bit tricky, because it tests an aspect of the same framework that is used for
;;;    testing. So, during the test, various global state is reset to initial values, new tests are defined,
;;;    but original state is restored afterwards.
;;;
;;;    Changing any of the global state handling might break the test and require that the test is adapted.
;;;    This test, unfortunately, is not structure-shy.

(defparameter *registry-for-test* '() "Registry we use for `TEST-REGISTRY-AND-DESCRIPTION'")

(deftest! test-registry-and-description ()
    "
    Checks how tests are registered and test documentation is synthesized with `DEFTEST-RGISTRY!'
"

  (explain "Defining some tests within locally established framework state.")

  (let ((docstring ""))
    (let ((*tests* '()))
      (deftest-registry! *registry-for-test*)

      (deftest! t1 ()
	  "
          Checks for something1."
	)

      (deftest! t2 ()
	  "
          Checks for something2."
	)
      (end-test-registry! *registry-for-test*)

      (setf docstring
	    (documentation '*registry-for-test* 'variable)))

    (format t "DOCSTRING => ~%~S~%" docstring)
    (format t "*REGISTRY-FOR-TEST* => ~A~%" *registry-for-test*)

    (assert! (equal *registry-for-test* '(T1 T2))) ;; now in order of definition
    (assert! (equal docstring
		    (here-text ()
		      ""
		      "T1 - DE.M-E-LEYPOLD.CL-SIMPLE-UTILS/TESTS::T1"
		      ""
		      "          Checks for something1."
		      "T2 - DE.M-E-LEYPOLD.CL-SIMPLE-UTILS/TESTS::T2"
		      ""
		      "          Checks for something2.")
		    ))))

;;; * Documentation anchors ---------------------------------------------------------------------------------|

(defparameter *anchor-for-test* nil)

(deftest! test-define-documentation-anchor ()
  "
    Checks if `DEFINE-DOCUMENTATION-ANCHOR' specializes documentation on a variable
    symbol to a user supplied function.
"
  (if (boundp '*anchor-for-test*)
      (makunbound '*anchor-for-test*))
  (define-documentation-anchor *anchor-for-test*
    :get #'(lambda (x) (declare (ignore x)) "A B C"))

  (assert! (equal "A B C" (documentation '*anchor-for-test* 'variable)))
  )

(defclass documentation-node-for-test (base-documentation-node)
  ())

(defmethod make-docstring ((node documentation-node-for-test))
  "X Y Z")

(deftest! test-define-documentation-node ()
    "
    Checks if `DEFINE-DOCUMENTATION-NODE' specializes `DOCUMENTATION' on a given symbol to invoking
    `GET-DOCTRING' on the instance of a class derived from `BASE-DCOUMENTATION-NODE' which is stored in the
    symbol.
"
  (if (boundp '*anchor-for-test*)
      (makunbound '*anchor-for-test*))

  (define-documentation-node *anchor-for-test* documentation-node-for-test)

  (typep *anchor-for-test* 'documentation-node-for-test)
  (assert! (equal "X Y Z" (documentation '*anchor-for-test* 'variable))))

(deftest! test-make-symbol-into-documentation-anchor ()
    "
    Checks if `MAKE-SYMBOL-INTO-DOCUMENTATION-ANCHOR' specializes documentation on a symbol to a user supplied
    function.    
"
  (let ((test-symbol (gensym "G:TEST-SYMBOL:")))
    (make-symbol-into-documentation-anchor
     test-symbol
     :get #'(lambda (x) (declare (ignore x)) "A B C")
     :doc-type 'function)

    (assert! (equal (documentation test-symbol 'function)
		    "A B C"))))


(deftest! test-add-documentation-node-to-function ()
    "
    Checks defining a function as documentation anchor with `ADD-DOCUMENTATION-NODE-TO-FUNCTION'
"
  (let ((test-symbol (gensym "G:TEST-SYMBOL:")))
    (add-documentation-node-to-function test-symbol 'documentation-node-for-test)
    (assert (typep (get-documentation-node test-symbol) 'documentation-node-for-test))
    (assert! (equal (documentation test-symbol 'function)
		    "X Y Z"))))

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
