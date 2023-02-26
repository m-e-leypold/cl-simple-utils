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

   :define-constant
   
   :inject-package-local-nickname
   :defpackage-doc
   :defrestart

   :concatenate-lines
   :here-text
   :here-text*

   :symbol-full-name
   :with-full-symbol-names
   :with-downcase-symbols
   :downcase-symbol-name
   :with-gensyms

   :base-documentation-node
   :get-docstring
   :make-docstring
   :define-documentation-anchor
   :define-documentation-node
   :make-symbol-into-documentation-anchor
   :add-documentation-node-to-function
   :get-documentation-node

   :package-from-indicator
   :with-package-from-indicator

   :ensure-load-file-hooks
   :load-file-hooks-exist-p
   :add-load-file-hook
   :end-of-load-file
   
   :create-load-tracker
   :remove-load-tracker
   :get-load-tracker
   :with-load-tracker
   :with-load-tracker-value
   :get-load-tracker-value

   :guess-load-scope
   :file-load-scope-p
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

;;; * -- DEFINE-CONSTANT ------------------------------------------------------------------------------------|

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;;; * -- Package docstrings as symbols ----------------------------------------------------------------------|

(defmacro defpackage-doc (&optional (sym))
  (if (not sym)
      (setf sym (intern "-DOC-" *package*)))
  (let ((docstring (documentation *package* T)))
    `(defun ,sym () ,docstring nil)))

;;; * -- Restart as function with documentation -------------------------------------------------------------|

(defmacro defrestart (name arglist docstring)
  (assert (not arglist)) ;; must be empty
  `(defun ,name () ,docstring (invoke-restart (quote ,name))))


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

(defmacro here-text* (&body lines)
  (funcall #'concatenate-lines lines
	   :prefix nil
	   :indent 0
	   :dedent-delimiter nil
	   :dedent 0
	   :separator #\Space
	   :separator-at-end nil
	   ))

;;; * -- Symbols --------------------------------------------------------------------------------------------|
;;; ** -- Full names ----------------------------------------------------------------------------------------|

(defmacro with-full-symbol-names (&body body)
  `(let ((*package* (find-package "KEYWORD")))
    ,@body))

(defun symbol-full-name (symbol)
  (with-full-symbol-names
      (format nil "~S" symbol)))

;;; ** -- Lower case symbols ---------------------------------------------------------------------------------|

(defmacro with-downcase-symbols (&body body)
  `(let ((*print-case* :downcase))
     ,@body))

(defun downcase-symbol-name (symbol)
  (let ((*package* (symbol-package symbol)))
    (with-downcase-symbols (format nil "~S" symbol))))

;;; ** -- with-gensyms --------------------------------------------------------------------------------------|

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym (concatenate 'string "G." (symbol-name (quote ,n)) "."))))
     ,@body))
;;; * -- Documentation nodes --------------------------------------------------------------------------------|


(defclass base-documentation-node ()
  ((cached-docstring :accessor cached-docstring :initform nil))
  (:documentation
   "
   Base class of a documentation node to be used with `DEFINE-DOCUMENTATION-NODE'.

   The idea is, to derive from this class another one with a method `MAKE-DOCSTRING' that
   creates the documentation string from the data stored in the object.

   `GET-DOCSTRING' when invoked on the symbol by `DOCUMENTATION' will then either retrieve the
   previously cached string or obtain a new one via `MAKE-DOCSTRING'.
"))


(defgeneric make-docstring (node)
  (:documentation
   "
   Create the documentation string from the data stored in a documentation node.

   This method should only be invoked via `GET-DOCSTRING' which implements some caching logic
   based on the actual content of the slot `CACHED-DOCSTRING' of a class derived from
   `BASE-DOCUMENTATION-NODE'. NODE should probably be derived from this class.
"))

(defun get-docstring (node)
  "
  Get the documentation string from a node.

  Called by a specialized version of `DOCUMENTATION'. The documentation string will either be
  retrieved from NODE (slot `CACHED-DOCSTRING') or if thise returns NIL, be regenerated with
  `MAKE-DOCSTRING' and stored in the slot.
"
  (let ((s (cached-docstring node)))
    (if s
	s
	(let ((s2 (make-docstring node)))
	  (setf (cached-docstring node) s2)
	  s2))))


(defmacro define-documentation-anchor (symbol
				       &key
					 get
					 (def 'defparameter)
					 (value nil))
  "
  Define a symbol as documentation anchor.

  This will define SYMBOL as a parameter (with `DEFPARAMETER') with value VALUE and define a
  `DOCUMENTATION' method specialized to SYMBOL that will return the result of invoking (funcall
  GET SYMBOL).

  GET must be function as returned by #'.

  The whole idea is to be able to define a symbol where slime-describe-symbol returns a
  dynamically generated string, e.g. to reflect information that is defined elsewhere or only
  accumulated later (like about defined tests in a test registry).

  Example:

      (define-documentation-anchor *ancho*
         :get #'(lambda (x) (declare (ignore x)) \"A B C\"))

      (documentation '*anchor* 'variable)

      => \"A B C\"

   Variation: When invoking wth argument DEF bound to 'DEFVAR a variable will instead be
   defined (with all the consequences this has!)
"
  `(progn
     (,def ,symbol ,value)
     (defmethod documentation ((object (eql (quote ,symbol))) (doc-type (eql 'variable)))
       (funcall ,get ,symbol))))

(defmacro define-documentation-node (symbol type &rest arguments)
  "
  Define SYMBOL as documentation node of type TYPE.

  This macro marries `DEFINE-DOCUMENTATION-ANCHOR' with `BASE-DOCUMENTATION-NODE'. TYPE must be
  class type derived from `BASE-DOCUMENTATION-NODE'. The macro will instantiate an object of
  class TYPE applying `MAKE-INSTANCE' with the parameters ARGUMENTS. SYMBOL will be defined as
  parameter and the object bound to SYMBOL.

  Later invocation of `DOCUMENTATION' on symbol will call `GET-DOCSTRING' on the object, if
  necessary `MAKE-DOCSTRING' and return the already cached or the newly created string.

  Typically TYPE is a registry of some sort that accumulates information on objects that are
  defined later. The docstring often is formatted in a way to allow branching out to the
  registered objects.

  Hypothetical example with a test registry:

      (define-documentation-node *my-test-registry* test-registry)

      (deftest foo () ...)
      (deftest bar () ...)

  Invoking (documentation '*my-test-registry* 'variable), e.g. by using slime-describe-symbol,
  might return in this case a string formatted like the following:

       FOO -- Test the foo factor.  Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
              Donec hendrerit tempor tellus.

       BAR -- Check for bar.  Donec pretium posuere tellus.  Proin quam nisl, tincidunt et,
              mattis eget, convallis nec, purus.
"
  `(define-documentation-anchor ,symbol
     :value (apply #'make-instance (quote ,type) (quote ,arguments))
     :get #'get-docstring))


(defun make-symbol-into-documentation-anchor (symbol &key get doc-type)
  "
  Make an existing symbol into a documentation node.
"
  (eval `(defmethod documentation ((object (eql (quote ,symbol))) (doc-type (eql (quote ,doc-type))))
	   (funcall ,get (quote ,symbol)))))


(defun get-documentation-node (symbol)
  (get symbol 'documentation-node))

(defun add-documentation-node-to-function (symbol type &rest arguments)
  "
  Define function SYMBOL as documentation node of type TYPE.
"
  (setf (get symbol 'documentation-node) (apply #'make-instance type arguments))
  (make-symbol-into-documentation-anchor symbol
					 :get #'(lambda (x) (get-docstring (get-documentation-node x)))
					 :doc-type 'function))
  

;;; * -- Packages -------------------------------------------------------------------------------------------|

;;; ** -- Package indicator ---------------------------------------------------------------------------------|


(defun package-from-indicator (indicator)
  (if (not indicator)
      *package*
      (if (symbolp indicator)	  
	  (symbol-package indicator)
	  (progn
	    (assert (packagep indicator))
	    indicator))))

(defmacro with-package-from-indicator ((var indicator) &body body)
  `(let ((,var (package-from-indicator ,indicator)))
     ,@body))
  

;;; * -- Load file instrumentation --------------------------------------------------------------------------|

;;; ** -- End of load file hooks ----------------------------------------------------------------------------|

(define-constant +load-file-hooks-symbol-name+ "*%LOAD-FILE-HOOKS%*")

(defun ensure-load-file-hooks (&key where)
  (with-package-from-indicator (package where)
    (let ((symbol (intern +load-file-hooks-symbol-name+ package)))
      (if (not (and symbol (boundp symbol)))
	  (eval `(defparameter ,symbol nil))))))

(defun get-load-file-hooks (&key where)
  (with-package-from-indicator (package where)
    (let ((symbol (find-symbol +load-file-hooks-symbol-name+ package)))
      (assert (and symbol (boundp symbol)) 
	      nil "No symbol ~a in ~S" +load-file-hooks-symbol-name+ package)
      symbol)))

(defun load-file-hooks-exist-p (&key where)
  (with-package-from-indicator (package where)
    (let ((symbol (find-symbol +load-file-hooks-symbol-name+ package)))
      (and symbol (boundp symbol)))))

(defun add-load-file-hook (hook &key where)
  (let ((hooks-symbol (get-load-file-hooks :where where)))
    (set hooks-symbol (cons hook (symbol-value hooks-symbol)))))

(defun end-of-load-file ()
  (let ((hooks-symbol (get-load-file-hooks)))
    (dolist (hook (symbol-value hooks-symbol))
      (funcall hook))
    (unintern hooks-symbol *package*)))

;;; ** -- Load trackers -------------------------------------------------------------------------------------|

;; TODO package-from-indication, with-package-from-indication

(defun remove-load-tracker (name &key where)
  (if (not where)
      (setf where *package*)
      (if (symbolp where)
	  (setf where (symbol-package where))))
  (let ((symbol (find-symbol name where)))
    (if symbol
	(unintern symbol where))))

(defun create-load-tracker (name &key where init)
  (if (not where)
      (setf where *package*)
      (if (symbolp where)
	  (setf where (symbol-package where))))
  (let ((symbol (intern name where)))
    (if (not (boundp symbol))
	(eval `(defparameter ,symbol (quote ,init)))
	(set symbol init))
    (add-load-file-hook #'(lambda () (remove-load-tracker name :where where)))
    symbol))

(defun get-load-tracker (name &key where)
    (if (not where)
      (setf where *package*)
      (if (symbolp where)
	  (setf where (symbol-package where))))
  (let ((symbol (find-symbol name where)))
    (assert symbol nil "file tracker not found: ~A" name)
    symbol))

(defmacro with-load-tracker ((symbol name &key where) &body body)
  `(let ((,symbol (get-load-tracker ,name :where ,where)))
     ,@body))

(defmacro with-load-tracker-value ((symbol name &key where) &body body)
  (with-gensyms (tracker-symbol)
    `(with-load-tracker (,tracker-symbol ,name :where ,where)
       (let ((,symbol (symbol-value ,tracker-symbol)))
	 ,@body))))

(defun get-load-tracker-value (name &key where)
  (with-load-tracker-value (value name :where where)
    value))
;;; ** -- Load mode -----------------------------------------------------------------------------------------|

(defun guess-load-scope ()
  (if (load-file-hooks-exist-p)
      :file
      (if (or *compile-file-pathname* *load-pathname*)
	  (progn
	    (warn "load scope unclear")
	    :unknown)
	  :form)))

(defun file-load-scope-p ()
  (eq :file (guess-load-scope)))
