;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;	(ql:quickload 'iterate))

(setf sb-impl::*default-external-format* :utf-8)

(declaim (optimize (compilation-speed 0) (speed 0) (space 0) (debug 3) (safety 3)))

(asdf:defsystem #:verifier
	;;:depends-on (:iterate)
	:components
	((:module :src
						:serial t
						:components ((:file "package")
												 (:file "utils")
												 (:file "statement")
												 (:file "basic-block")
												 (:file "dataflow")
												 (:file "main")))))
