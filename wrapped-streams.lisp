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
   :basic-indenting-character-output-stream
   :line-prefix
   :wrapped-stream
   :indented-stream
   :maybe-indented-stream
   ))

(in-package :de.m-e-leypold.cl-simple-utils/wrapped-streams)

;;; * -- Indenting Output Stream ----------------------------------------------------------------------------|


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

(defun indented-stream (stream
			&key
			  indent
			  (prefix (make-sequence 'string indent :initial-element #\Space)))
  (make-instance 'basic-indenting-character-output-stream :wraps stream :prefix prefix))

;; (defparameter w (indented-stream *standard-output* :prefix "    | "))
;; (format t "----~%")
;; (format w "hello~%world~%!~%")
;; (format t "----~%")
