(in-package :cl-user)
(defpackage verifier-test
	(:use :cl
				:main
				:prove))
(in-package :verifier-test)

;; NOTE: To run this test file, execute `(asdf:test-system :verifier)' in your Lisp.

(plan 1)

(subtest "normal case"
	(main)
	(ok t
			"Sample project was generated")
	(ok t
			"Can load the new project"))

;; blah blah blah.

(finalize)
