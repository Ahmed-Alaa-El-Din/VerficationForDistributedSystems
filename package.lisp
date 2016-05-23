(defpackage #:utils
	(:use #:common-lisp)
	(:export #:flatten-list))

(defpackage #:statement
	(:use #:common-lisp #:utils)
	(:export #:statement #:block-id #:construct-statement-vector))

(defpackage #:block
	(:use #:common-lisp #:statement #:utils)
	(:export #:construct-basic-blocks #:frst-stmt #:last-stmt))

(defpackage #:dataflow
	(:use :common-lisp #:statement #:block #:utils))

(defpackage #:main
	(:use :common-lisp #:statement #:block)
	(:export #:main))
