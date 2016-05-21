(defparameter *basic-blocks* (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter *statements*     (make-array 1 :fill-pointer 0 :adjustable t))

(defclass statement-block ()
	((statement								:accessor statement								:initarg :statement )
	 (gen											:accessor gen)
	 (kill										:accessor kill	:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (in											:accessor in		:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (out											:accessor out		:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (predecessor-statements	:accessor predecessor-statements	:initarg :predecessor-statements)))

(defmethod print-object ((this statement-block) out)
	(print-unreadable-object (this out :type t)
		(format out ":statement \"~a\" :predecessor-statements \"~a\""
						(statement this) (predecessor-statements this))))

;; (defun push-statement (stmt predecessors)
;;	(vector-push-extend (make-instance 'statement-block
;;																		 :statement stmt
;;																		 :predecessor-statements predecessors)
;;											*statements*))
(defun make-statement-block (stmt predecessors)
	(make-instance 'statement-block :statement stmt :predecessor-statements predecessors))

;; these convert a stmt to several statement-blocks
(defun convert-if2 (stmt)
	(destructuring-bind (if cond yes no) stmt
		`(,(make-statement-block `(,if ,cond)	'(-1))
			 ,(make-statement-block yes					'(-1))
			 ,(make-statement-block no					'(-2)))))

(defun convert-if1 (stmt)
	(destructuring-bind (if cond yes) stmt
			`(,(make-statement-block `(,if ,cond) '(-1))
				,(make-statement-block yes '(-1)))))

(defun convert-stmt (stmt)
	(destructuring-bind (head . rest) stmt
			(case head
				(if (convert-if2 stmt))
				((when unless) (convert-if1 stmt))
				(otherwise `(,(make-statement-block stmt '(-1)))))))

(defun construct-statement-vector (program-code)
	(mapcar #'convert-stmt
					program-code))

(defclass basic-block ()
	((frst-stmt						:accessor frst-stmt						:initarg :frst-stmt)
	 (last-stmt						:accessor last-stmt						:initarg :last-stmt)
	 (next-block					:accessor next-block)
	 (predecessor-blocks	:accessor predecessor-blocks	:initarg :predecessor-blocks)))


(defun frst-stmt-predecessors ()
	"loop through basic blocks w a5ali l predecessors bto3 l frst-stmt bta3 l block yeb2o vector of last-stmt-indexes of predecessors"
	(loop
		 :for basic-block
		 :being :the :element :of *basic-blocks*
		 :do (let ((pre (make-array 1
																:fill-pointer 0
																:adjustable t)))
					 (loop
							:for predecessor
							:being :the :element :of (predecessor-blocks basic-block)
							:do (vector-push-extend (last-stmt (elt *basic-blocks* predecessor)) pre))
					 (setf (predecessor-statements
									(elt *statements* (frst-stmt basic-block))) pre))))


(defun push-basic-block (predecessors)
	(vector-push-extend (make-instance 'basic-block
																		 :frst-stmt *frst-stmt*
																		 :last-stmt *last-stmt*
																		 :predecessor-blocks predecessors)
											*basic-blocks*)
	(setf *frst-stmt* (incf *last-stmt*)))

(defun construct-basic-blocks ()
	(let ((*frst-stmt* 0)
				(*last-stmt* 0))
		(declare (special *frst-stmt* *last-stmt*))
		(loop
			 :for stmt :in *statements*
			 :for x :from 0
			 :do (cond ((eq (car stmt) 'if)
									(push-basic-block (vector-union (predecessors 1 *basic-blocks*)
																									(predecessors 2 *basic-blocks*)));;maybe wrong
									(push-basic-block (predecessors 1 *basic-blocks*))
									(push-basic-block (predecessors 2 *basic-blocks*))
									(incf x 2))
								 ((or (eq (car stmt) 'unless) (eq (car stmt) 'when))
									(push-basic-block (vector-union (predecessors 1 *basic-blocks*)
																									(predecessors 2 *basic-blocks*)))
									(push-basic-block (predecessors 1 *basic-blocks*))
									(incf x))
								 (t (incf *last-stmt*))))
		(push-basic-block (progn (decf *last-stmt*) (vector-union (predecessors 1 *basic-blocks*)
																															(predecessors 2 *basic-blocks*))))))

;;;;; DFG ;;;;;

(defun gen-and-initial-out ()
	(loop
		 :for stmt :being :the :element :of *statements*
		 :for x :from 0
		 :do (let ((stmt-out (make-array 1 :fill-pointer 0 :adjustable t)))
					 (when (or (eq (car stmt) 'setq)(eq (car stmt) 'setf))
						 (setf (gen stmt) (cons (nth 1 stmt) x))
						 (vector-push-extend (gen stmt) stmt-out)
						 (setf (out stmt) stmt-out)))))

(defun vector-union (v w)
	(remove-duplicates (merge 'vector v w #'equalp) :test #'equalp))

(defun vector-difference (v w)
	(loop
		 :for x
		 :being :the :element :of w
		 :do (setq v (remove x v :test #'equalp)))
	v)

(defun reaching-definitions ()
	(let ((change t)
				(old-out))
		(loop
			 :while change
			 :do (setf change nil)
			 (loop
					:for stmt
					:being :the :element :of *statements*
					:do (setf (in stmt) (in-set stmt))
					(setf old-out (out stmt))
					(setf (out stmt) (vector-union (gen stmt) (vector-difference (in stmt)(kill stmt))))
					(unless (equalp (out stmt) old-out);;without consedring sorting
						(setf change t))))))

(defun in-set (statement) ;;block .. magarabthash le7ad delwa2ti
	(let ((in ))
		(loop for predecessor-index being the element of (predecessor-statements statement)
			 do (setf in (vector-union in (out (elt *statements* predecessor-index)))))
		in))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
														(* x 2)))

(construct-statement-vector parsed-code)
(construct-basic-blocks)
(frst-stmt-predecessors basic-blocks)
(gen-and-initial-out )
;;(reaching-definitions *statements*)
