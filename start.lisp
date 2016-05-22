#!/bin/sh
#|
exec sbcl --dynamic-space-size 25600 --script "$0" $@
|#

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
																			 (user-homedir-pathname))))
	(when (probe-file quicklisp-init)
		(load quicklisp-init)))
(load (compile-file "verifier.asd"))
(asdf::load-system 'verifier)
(cfg::main SB-EXT:*POSIX-ARGV*)

;; Local Variables:
;; mode: common-lisp
;; End:
