(in-package #:main)

(defparameter parsed-code '((setq x 12)
														(setq y 21)
														(if (= y 12)
																(setq y (* y 2))
																(setq x 42))
														(setq w 12)
														(+ 2 3)
														(setf z 22)
														(when (> x 20)
															(setq x (+ x 1)))
														(setq zz 18)
														(* 3 w)
														(unless (> y 30)
															(print "y = 21"))
														(square y)
														(* x 2)
														(unless (> y 30)
															(print "y = 21"))
														))


(defun main (&optional (argv nil))
	(declare (ignore argv))
	(let* ((statements
					(cfg::construct-statement-vector
					 parsed-code)))
		(format t "~a~%" statements))	)
