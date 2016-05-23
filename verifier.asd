#|
	This file is a part of verifier project.
	Copyright (c) 2016 Osama Ahmed, Moamen Hosam, Ahmed Alaa El-Din Gad Ziada, Mohammad Alaggan (mohammad.nabil.h@gmail.com)
|#

#|
	Author: Osama Ahmed, Moamen Hosam, Ahmed Alaa El-Din Gad Ziada, Mohammad Alaggan (mohammad.nabil.h@gmail.com)
|#

(in-package :cl-user)
(defpackage verifier-asd
	(:use :cl :asdf))
(in-package :verifier-asd)

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
(ql:quickload 'iterate))
|#

(setf sb-impl::*default-external-format* :utf-8)

(declaim (optimize (compilation-speed 0) (speed 0) (space 0) (debug 3) (safety 3)))

(defsystem verifier
	:version "0.1"
	:author "Osama Ahmed, Moamen Hosam, Ahmed Alaa El-Din Gad Ziada, Mohammad Alaggan"
	:license "MIT"
	:depends-on () ; :iterate
	:components ((:module "src"
												:serial t
												:components
												((:file "package")
												 (:file "utils")
												 (:file "statement")
												 (:file "basic-block")
												 (:file "dataflow")
												 (:file "main"))))
	:description ""
	:long-description
	#.(with-open-file (stream (merge-pathnames
														 #p"README.org"
														 (or *load-pathname* *compile-file-pathname*))
														:if-does-not-exist nil
														:direction :input)
			(when stream
				(let ((seq (make-array (file-length stream)
															 :element-type 'character
															 :fill-pointer t)))
					(setf (fill-pointer seq) (read-sequence seq stream))
					seq)))
	:in-order-to ((test-op (test-op verifier-test))))
