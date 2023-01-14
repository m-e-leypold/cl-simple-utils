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

(defpackage :de.m-e-leypold.cl-simple-utils/wrapped-streams
  (:documentation "Stream wrappers: Intending stream text before output")
  (:use :common-lisp :sb-gray)
  (:export
   #+sbcl :basic-indenting-character-output-stream
   #+sbcl :line-prefix
   #+sbcl :wrapped-stream
   #+sbcl :indented-stream
   :maybe-indented-stream
   #+sbcl :with-output-to
   #+sbcl :with-error-to
   #+sbcl :with-indented-output   
   :with-maybe-indented-output
   :with-capturing-output
   ))

(in-package :de.m-e-leypold.cl-simple-utils/wrapped-streams)

;;; * -- Indenting Output Stream ----------------------------------------------------------------------------|

#+sbcl
(defclass basic-indenting-character-output-stream (fundamental-character-output-stream)
  ((wrapped-stream
    :reader  wrapped-stream
    :initarg :wraps
    :initform (error "basic-indenting-character-output-stream must have a stream to wrap"))
   (line-prefix
    :reader  line-prefix
    :initarg :prefix
    :initform "")
   (in-line
    :accessor in-line
    :initform nil)))

;; TODO: Prefix as a string passed.
;; TODO: Track whether we already started, output prefix at beginnin

#+sbcl
(defmethod stream-write-char ((stream basic-indenting-character-output-stream) ch)
  ;; (format t "-> ~S" ch))
  (if (not (in-line stream))
      (progn
	(write-string (line-prefix stream) (wrapped-stream stream))
	(setf (in-line stream) T)))
  (write-char ch (wrapped-stream stream))
  (if (find ch '(#\Newline #\Return #\Linefeed))
      (setf (in-line stream) nil)))

;; (defmethod stream-write-string ((stream basic-indenting-character-output-stream) string &optional start end)
;;  (write-string string  (wrapped-stream stream) :start start :end end))

#+sbcl
(defun indented-stream (stream &key indent prefix)
  (if (not prefix)
      (setf prefix (make-sequence 'string indent :initial-element #\Space)))
  (make-instance 'basic-indenting-character-output-stream :wraps stream :prefix prefix))

(defun maybe-indented-stream (stream &key indent prefix)
  (declare (ignorable indent prefix))
  #+sbcl (indented-stream stream :indent indent :prefix prefix)
  #-sbcl stream)


(defmacro with-output-to (stream &body body)
  `(let ((*standard-output* ,stream))
     ,@body))


(defmacro with-error-to (stream &body body)
  `(let ((*error-output* ,stream))
     ,@body))
  
(defmacro with-indented-output ((&key indent prefix) &body body)
  (let ((wrapped (gensym "WRAPPED")))
    `(let ((,wrapped (indented-stream *standard-output* :indent ,indent :prefix ,prefix)))
       (with-output-to ,wrapped
	 (with-error-to ,wrapped
	   ,@body)))))

(defmacro with-maybe-indented-output ((&key indent prefix) &body body)
  #+sbcl `(with-indented-output (:indent ,indent :prefix ,prefix) ,@body)
  #-sbcl `nil)

(defmacro with-capturing-output ((var &body forms) &body body)
  (let ((stream (gensym "STREAM")))
    `(let ((,var nil))
       (let ((,stream (make-string-output-stream)))
	 (with-output-to ,stream
	   (with-error-to ,stream
	     ,@forms))
	 (setf ,var (get-output-stream-string ,stream)))
       ,@body)))
	 


;; (defparameter w (indented-stream *standard-output* :prefix "    | "))
;; (format t "----~%")
;; (format w "hello~%world~%!~%")
;; (format t "----~%")
