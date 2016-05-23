(in-package :cl-user)
(defpackage verifier-test
	(:use :cl
				:main
				:statement
				:prove))
(in-package :verifier-test)

(plan 1)

(defvar *parsed-code* '((setq x 12)
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
														;; (unless (> y 30)
														;;	(print "y = 21"))
												))

(defvar *statements*
	(list
	 (make-instance 'stmt-block :statement  '(BEGIN)           :id   0    :pred   NIL        :next  '(1)      :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SETQ X 12)       :id   1    :pred   '(0)       :next  '(2)      :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SETQ Y 21)       :id   2    :pred   '(1)       :next  '(3)      :block-id NIL)
	 (make-instance 'stmt-block :statement  '(IF (= Y 12))     :id   3    :pred   '(2)       :next  '(4 5)    :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SETQ Y (* Y 2))  :id   4    :pred   '(3)       :next  '(6)      :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SETQ X 42)       :id   5    :pred   '(3)       :next  '(6)      :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SETQ W 12)       :id   6    :pred   '(4 5)     :next  '(7)      :block-id NIL)
	 (make-instance 'stmt-block :statement  '(+ 2 3)           :id   7    :pred   '(6)       :next  '(8)      :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SETF Z 22)       :id   8    :pred   '(7)       :next  '(9)      :block-id NIL)
	 (make-instance 'stmt-block :statement  '(WHEN (> X 20))   :id   9    :pred   '(8)       :next  '(10 11)  :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SETQ X (+ X 1))  :id   10   :pred   '(9)       :next  '(11)     :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SETQ ZZ 18)      :id   11   :pred   '(9 10)    :next  '(12)     :block-id NIL)
	 (make-instance 'stmt-block :statement  '(* 3 W)           :id   12   :pred   '(11)      :next  '(13)     :block-id NIL)
	 (make-instance 'stmt-block :statement  '(UNLESS (> Y 30)) :id   13   :pred   '(12)      :next  '(14 15)  :block-id NIL)
	 (make-instance 'stmt-block :statement  '(PRINT y = 21)    :id   14   :pred   '(13)      :next  '(15)     :block-id NIL)
	 (make-instance 'stmt-block :statement  '(SQUARE Y)        :id   15   :pred   '(13 14)   :next  '(16)     :block-id NIL)
	 (make-instance 'stmt-block :statement  '(* X 2)           :id   16   :pred   '(15)      :next  '(17)     :block-id NIL)
	 (make-instance 'stmt-block :statement  '(END)             :id   17   :pred   '(16)      :next  NIL       :block-id NIL)))

(subtest "normal case"
	(let ((result (construct-statement-vector *parsed-code*)))
		(format t "~a~%~%~a~%~%" *statements* result)
		(ok (equalp (format nil "~a" *statements*) (format nil "~a" result))
				"Statements was generated correctly")
		(ok t
				"Can load the new project")))

(finalize)
