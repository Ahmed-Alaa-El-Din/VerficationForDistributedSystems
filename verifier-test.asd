#|
	This file is a part of verifier project.
	Copyright (c) 2016 Osama Ahmed, Moamen Hosam, Ahmed Alaa El-Din Gad Ziada, Mohammad Alaggan (mohammad.nabil.h@gmail.com)
|#

(in-package :cl-user)
(defpackage verifier-test-asd
	(:use :cl :asdf))
(in-package :verifier-test-asd)

(defsystem verifier-test
	:author "Osama Ahmed, Moamen Hosam, Ahmed Alaa El-Din Gad Ziada, Mohammad Alaggan"
	:license "MIT"
	:depends-on (:verifier
							 :prove)
	:components ((:module "t"
								:components
								((:test-file "verifier"))))
	:description "Test system for verifier"
	:defsystem-depends-on (:prove-asdf)
	:perform (test-op :after (op c)
										(funcall (intern #.(string :run-test-system) :prove-asdf) c)
										;(asdf:clear-system c)
										))
