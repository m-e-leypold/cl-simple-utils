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

;;; ---- System -----------------------------------------------------------------------------|

(defsystem "de.m-e-leypold.cl-simple-utils"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Primitive utilities required to build up infrastructure for UNCOMMON-LISP development"  
  :components ((:file "simple-utils")))

(defsystem "de.m-e-leypold.cl-simple-utils/wrapped-streams"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Simple implementations of wrapped streams, e.g. for indented output"
  :components ((:file "wrapped-streams")))

(defsystem "de.m-e-leypold.cl-simple-utils/basic-test"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :depends-on ("de.m-e-leypold.cl-simple-utils" "de.m-e-leypold.cl-simple-utils/wrapped-streams")
  :description "An untested test framework"
  :components ((:file "basic-test")))


(defsystem "de.m-e-leypold.cl-simple-utils/tests"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Testing CL-SIMPLE-UTILS"
  :depends-on ("de.m-e-leypold.cl-simple-utils"
	       "de.m-e-leypold.cl-simple-utils/wrapped-streams"
	       "de.m-e-leypold.cl-simple-utils/basic-test")
  :components ((:file "tests")))

(defsystem "de.m-e-leypold.cl-simple-utils/load-all"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Load all systems in CL-SIMPLE-UTILS"
  :depends-on ("de.m-e-leypold.cl-simple-utils"
	       "de.m-e-leypold.cl-simple-utils/wrapped-streams"
	       "de.m-e-leypold.cl-simple-utils/basic-test"))

(defsystem "de.m-e-leypold.cl-simple-utils/prerequisites"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :depends-on ()
  :description "Just all external prerequisites"
  :components ())

