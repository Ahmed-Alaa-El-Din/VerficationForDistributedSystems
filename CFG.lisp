(defclass statement-block ()
	((statement								:accessor statement								:initarg :statement )
	 (gen											:accessor gen)
	 (kill										:accessor kill	:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (in											:accessor in		:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (out											:accessor out		:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (predecessor-statements	:accessor predecessor-statements	:initarg :predecessor-statements)))

(defclass basic-block ()
	((leader							:accessor leader							:initarg :leader)
	 (last-stmt						:accessor last-stmt						:initarg :last-stmt)
	 (next-block					:accessor next-block)
	 (predecessor-blocks	:accessor predecessor-blocks	:initarg :predecessor-blocks)))

(defparameter basic-blocks (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter statements   (make-array 1 :fill-pointer 0 :adjustable t))

(defun return-condition (condition-statement) ;;like (if (= y 12))
	;;(when (eq (car condition-statement) 'if)
	(cadr condition-statement))



(defun construct-statement-vector (program-code)
	(mapcar (lambda (stmt)
						(cond ((eq (car stmt)
											 'if)
									 (push-statement (list (nth 0 stmt)
																				 (nth 1 stmt))
																	 (predecessors 1 statements))
									 (push-statement (nth 2 stmt)
																	 (predecessors 1 statements))
									 (push-statement (nth 3 stmt)
																	 (predecessors 2 statements)))
									((or (eq (car stmt)
													 'when)
											 (eq (car stmt)
													 'unless))
									 (push-statement (list (nth 0 stmt)
																				 (nth 1 stmt))
																	 (predecessors 1 statements))
									 (push-statement (nth 2 stmt)
																	 (predecessors 1 statements)))
									(t (push-statement stmt
																		 (predecessors 1 statements))))))
	program-code)


(defun leader-predecessors (basic-blocks)
	"loop through basic blocks w a5ali l predecessors bto3 l leader bta3 l block yeb2o vector of last-stmt-indexes of predecessors"
	(loop :for basic-block :being :the :element :of basic-blocks
		 :do (let ((pre (make-array 1
															 :fill-pointer 0
															 :adjustable t)))
					(loop :for predecessor :being :the :element :of (predecessor-blocks basic-block)
						 :do (vector-push-extend (last-stmt (elt basic-blocks predecessor)) pre))
					(setf (predecessor-statements (elt statements (leader basic-block))) pre))))

(defun predecessors (number v) ;;v : statemenets or basic-blocks
	(if (zerop (length v))
			(vector (- (length v) number))
			(vector))
	;; (let ((predecessor-vector (make-array 1 :fill-pointer 0 :adjustable t)))
	;;	(unless (eq (length v) 0)
	;;		(vector-push-extend (- (length v) number) predecessor-vector))
	;;	predecessor-vector)
	)

(defun push-statement (stmt predecessors)
	(vector-push-extend (make-instance 'statement-block
																		 :statement stmt
																		 :predecessor-statements predecessors)
											statements))

(defun push-basic-block (ldr lastt predecessors)
	(vector-push-extend (make-instance 'basic-block
																		 :leader ldr
																		 :last-stmt lastt
																		 :predecessor-blocks predecessors)
											basic-blocks)
	(setf leader (incf last-stmt)))

(defun construct-basic-blocks ()
	(let ((leader 0)
				(last-stmt 0))
		(declare (special leader last-stmt))
		(loop
			 :for stmt :in statements
			 :for x :from 0
			 do (cond ((eq (car stmt) 'if)
								 (push-basic-block leader last-stmt (vector-union (predecessors 1 basic-blocks)
																																	(predecessors 2 basic-blocks)));;maybe wrong
								 (push-basic-block leader last-stmt (predecessors 1 basic-blocks))
								 (push-basic-block leader last-stmt (predecessors 2 basic-blocks))
								 (incf x 2))
								((or (eq (car stmt) 'unless) (eq (car stmt) 'when))
								 (push-basic-block leader last-stmt (vector-union (predecessors 1 basic-blocks)
																																	(predecessors 2 basic-blocks)))
								 (push-basic-block leader last-stmt (predecessors 1 basic-blocks))
								 (incf x))
								(t (incf last-stmt))))
		(push-basic-block leader (decf last-stmt) (vector-union (predecessors 1 basic-blocks)
																														(predecessors 2 basic-blocks)))))

;;(defun next-block (statement) ;;statement block le7ad delwa2ti
	;;(if (return-condition statement) ;;if condition is true
		;;  (progn



;;;;; DFG ;;;;;

(defun gen-and-initial-out ()
	(loop for x from 0 to (1- (length statements))
		 do
			 (let ((stmt (statement (elt statements x)))
						 (stmt-block (elt statements x))
						 (stmt-out (make-array 1 :fill-pointer 0 :adjustable t)))
				 (when (or (eq (car stmt) 'setq)(eq (car stmt) 'setf))
					 (setf (gen stmt-block) (cons (nth 1 stmt) x))
					 (vector-push-extend (gen stmt-block) stmt-out)
					 (setf (out stmt-block) stmt-out)))))
;;(find '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => (1 2)

(defun vector-union (v w)
	(remove-duplicates (merge 'vector v w #'equalp) :test #'equalp))

(defun vector-difference (v w)
	(loop for x being the element of w
		 do (setq v (remove x v :test #'equalp)))
	v)

(defun reaching-definitions (statements)
	(let ((change t)
				(old-out))
		(loop while change do
				 (setf change nil)
				 (loop for stmt being the element of statements
						do (setf (in stmt) (in-set stmt))
							(setf old-out (out stmt))
							(setf (out stmt) (vector-union (gen stmt) (vector-difference (in stmt)(kill stmt))))
							(unless (equalp (out stmt) old-out);;without consedring sorting
								(setf change t))))))

(defun in-set (statement) ;;block .. magarabthash le7ad delwa2ti
	(let ((in ))
		(loop for predecessor-index being the element of (predecessor-statements statement)
			 do (setf in (vector-union in (out (elt statements predecessor-index)))))
		in))
