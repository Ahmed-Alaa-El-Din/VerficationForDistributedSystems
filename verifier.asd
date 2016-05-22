;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;	(ql:quickload 'iterate))

(declaim (optimize (compilation-speed 0) (speed 0) (space 0) (debug 3) (safety 3)))

(asdf:defsystem :verifier
	;;:depends-on (:iterate)
	:serial t
	:components ((:file "package")
							 (:file "utils")
							 (:file "CFG")
							 (:file "basic-block")
							 (:file "main")))
