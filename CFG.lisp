(defparameter *basic-blocks* (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter *statements*   (make-array 1 :fill-pointer 0 :adjustable t))
(defclass stmt-block ()
	((statement								:accessor statement								:initarg :statement )
	 (predecessor-statements	:accessor predecessor-statements	:initarg :predecessor-statements)
	 (gen											:accessor gen)
	 (kill										:accessor kill	:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (in											:accessor in		:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (out											:accessor out		:initform (make-array 1 :fill-pointer 0 :adjustable t))))
(defmethod print-object ((this stmt-block) out)
	(print-unreadable-object (this out :type t)
		(format out ":stmt ~20a :pred ~a"
						(statement this) (predecessor-statements this))))
(defun make-stmt-block (stmt predecessors)
	(make-instance 'stmt-block :statement stmt :predecessor-statements predecessors))
;; these convert a stmt to several stmt-blocks
(defun convert-if2  (stmt)
	(destructuring-bind (if cond yes no) stmt
		`(,(make-stmt-block `(,if ,cond)	'(-1))
			 ,(make-stmt-block yes					'(-1))
			 ,(make-stmt-block no						'(-2)))))
(defun convert-if1  (stmt)
	(destructuring-bind (if cond yes) stmt
		`(,(make-stmt-block `(,if ,cond) '(-1))
			 ,(make-stmt-block yes         '(-1)))))
(defun convert-stmt (stmt)
	(destructuring-bind (head . rest) stmt
		(case head
			(if            (convert-if2 stmt))
			((when unless) (convert-if1 stmt))
			(otherwise `(,(make-stmt-block stmt '(-1)))))))
(defun construct-statement-vector (program-code)
	(apply #'append
				 (mapcar #'convert-stmt
								 program-code)))

;; (defun push-statement (stmt predecessors)
;;	(vector-push-extend (make-instance 'stmt-block
;;																		 :statement stmt
;;																		 :predecessor-statements predecessors)
;;											*statements*))

(defclass basic-block ()
	((frst-stmt						:accessor frst-stmt						:initarg :frst-stmt)
	 (last-stmt						:accessor last-stmt						:initarg :last-stmt)
	 (next-block					:accessor next-block)
	 (predecessor-blocks	:accessor predecessor-blocks	:initarg :predecessor-blocks)))
(defmethod print-object ((this basic-block) out)
	(print-unreadable-object (this out :type t)
		(format out ":range ~3a ~3a :pred ~8a"
						(frst-stmt this) (last-stmt this) (predecessor-blocks this))))
(defun make-basic-block (predecessors)
	(prog1
			(make-instance 'basic-block
										 :frst-stmt *frst-stmt*
										 :last-stmt *last-stmt*
										 :predecessor-blocks predecessors)
		(setf *frst-stmt* (incf *last-stmt*))))

;; (defun push-basic-block (predecessors)
;;	(vector-push-extend (make-instance 'basic-block
;;																		 :frst-stmt *frst-stmt*
;;																		 :last-stmt *last-stmt*
;;																		 :predecessor-blocks predecessors)
;;											*basic-blocks*)
;;	(setf *frst-stmt* (incf *last-stmt*)))

(defun if2-to-bb ()
	(mapcar #'make-basic-block '((-1) (-2)))) ; yes bb and no bb

(defun if1-to-bb ()
	(make-basic-block '(-1)))							; yes bb

(defun finish-bb ()
	(make-basic-block '(-1 -2)))					; -1 and -2 since the previous block must have been a branch. first bb is special case (todo)

(defun stmt-to-basic-block (stmt)
	(case (car (statement stmt))		; if branch, finialize the current basic block
		(if							(apply #'append `(,(list (finish-bb)) ,(if2-to-bb))))
		((unless when)	`(,(finish-bb) ,(if1-to-bb)))
		(otherwise												; keep slurping next stmt into current block until we meet a branch
		 (incf *last-stmt*))))

(defun construct-basic-blocks (statements)
	(let ((*frst-stmt* 0)
				(*last-stmt* 0))
		(declare (special *frst-stmt* *last-stmt*))
		(let ((bblocks (apply #'append (remove-if #'numberp (mapcar #'stmt-to-basic-block statements)))))
			(decf *last-stmt*)
																				;(append bblocks (make-basic-block '(-1 -2)))
			bblocks
			)))

(defun frst-stmt-predecessors ()
	"loop through basic blocks w a5ali l predecessors bto3 l frst-stmt bta3 l block yeb2o vector of last-stmt-indexes of predecessors"
	(loop
		 :for basic-block
		 :accross *basic-blocks*
		 :do (let ((pre (make-array 1
																:fill-pointer 0
																:adjustable t)))
					 (loop
							:for predecessor
							:accross (predecessor-blocks basic-block)
							:do (vector-push-extend (last-stmt (elt *basic-blocks* predecessor)) pre))
					 (setf (predecessor-statements
									(elt *statements* (frst-stmt basic-block))) pre))))

;;;;; DFG ;;;;;

(defun gen-and-initial-out ()
	(loop
		 :for stmt :accross *statements*
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
		 :accross w
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
					:accross *statements*
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

(#<BASIC-BLOCK :range 0   2   :pred (-1 -2) >
 #<BASIC-BLOCK :range 3   3   :pred (-1)    >
 #<BASIC-BLOCK :range 4   4   :pred (-2)    >
 #<BASIC-BLOCK :range 5   10  :pred (-1 -2) >
 #<BASIC-BLOCK :range 11  11  :pred (-1)    >
 #<BASIC-BLOCK :range 12  15  :pred (-1 -2) >
 #<BASIC-BLOCK :range 16  16  :pred (-1)    >)

 0 (#<STMT-BLOCK :stmt (SETQ X 12)          :pred (-1)>
 1  #<STMT-BLOCK :stmt (SETQ Y 21)          :pred (-1)>
 2  #<STMT-BLOCK :stmt (IF (= Y 12))        :pred (-1)>

 3  #<STMT-BLOCK :stmt (SETQ Y (* Y 2))     :pred (-1)>

 4  #<STMT-BLOCK :stmt (SETQ X 42)          :pred (-2)>

 5  #<STMT-BLOCK :stmt (SETQ W 12)          :pred (-1)>
 6  #<STMT-BLOCK :stmt (+ 2 3)              :pred (-1)>
 7  #<STMT-BLOCK :stmt (SETF Z 22)          :pred (-1)>
 8  #<STMT-BLOCK :stmt (WHEN (> X 20))      :pred (-1)>
 9  #<STMT-BLOCK :stmt (SETQ X (+ X 1))     :pred (-1)>
10  #<STMT-BLOCK :stmt (SETQ ZZ 18)         :pred (-1)>

11  #<STMT-BLOCK :stmt (* 3 W)              :pred (-1)>

12  #<STMT-BLOCK :stmt (UNLESS (> Y 30))    :pred (-1)>
13  #<STMT-BLOCK :stmt (PRINT y = 21)       :pred (-1)>
14  #<STMT-BLOCK :stmt (SQUARE Y)           :pred (-1)>
15  #<STMT-BLOCK :stmt (* X 2)              :pred (-1)>)
