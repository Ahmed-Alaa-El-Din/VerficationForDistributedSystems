;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;	(ql:quickload 'iterate))

(declaim (optimize (compilation-speed 0) (speed 3) (space 0) (debug 0) (safety 0)))

(asdf:defsystem :verifier
	;;:depends-on (:iterate)
	:serial t
	:components ((:file "package")
							 (:file "CFG")
							 (:file "main")))
