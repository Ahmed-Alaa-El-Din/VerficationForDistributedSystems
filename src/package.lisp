(in-package :cl-user)
(defpackage utils
	(:use :cl)
	(:export #:flatten-list))

(defpackage statement
	(:use :cl :utils)
	(:export #:statement #:block-id #:construct-statement-vector
					 #:pred #:gen #:kill #:in #:out))

(defpackage block
	(:use :cl :statement :utils)
	(:export #:construct-basic-blocks #:frst-stmt #:last-stmt))

(defpackage dataflow
	(:use :cl :statement :block :utils))

(defpackage main
	(:use :cl :statement :block)
	(:export #:main))
