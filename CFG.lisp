;; (defparameter *basic-blocks* (make-array 1 :fill-pointer 0 :adjustable t))
;; (defparameter *statements*   (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter *kill-hash-table* (make-hash-table))

(defclass stmt-block ()
	((id                      :accessor id                      :initarg :id)
	 (statement								:accessor statement								:initarg :statement)
	 (predecessor-statements	:accessor predecessor-statements	:initarg :predecessor-statements)
	 (gen											:accessor gen   :initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (kill										:accessor kill	:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (in											:accessor in		:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (out											:accessor out		:initform (make-array 1 :fill-pointer 0 :adjustable t))))
(defmethod print-object ((this stmt-block) out)
	(print-unreadable-object (this out :type t)
		(format out ":id ~3a :stmt ~20a :pred ~a"
						(id this) (statement this) (predecessor-statements this))))
(defvar *id* 0)
(defun make-stmt-block (stmt predecessors)
	(make-instance 'stmt-block :id (1- (incf *id*)) :statement stmt :predecessor-statements predecessors))
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
	(case (car stmt)
		(if            (convert-if2 stmt))
		((when unless) (convert-if1 stmt))
		(otherwise `(,(make-stmt-block stmt '(-1))))))
(defun construct-statement-vector (program-code)
	(apply #'append
				 (mapcar #'convert-stmt
								 program-code)))

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
														;; (unless (> y 30)
														;;	(print "y = 21"))
														))

(let* ((statements (construct-statement-vector parsed-code)))
	(format t "~a~%" statements))

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

(defvar *frst-stmt*)
(defvar *last-stmt*)
(defvar *index*)
(defvar *length*)
(defun make-basic-block (predecessors)
	(prog1
			(make-instance 'basic-block
										 :frst-stmt *frst-stmt*
										 :last-stmt (min *last-stmt* (1- *length*))
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
	(make-basic-block '(-1)))					; yes bb
(defun finish-bb ()
	(unless (>= *frst-stmt* *length*)
		(make-basic-block '(-1 -2)))) ; -1 and -2 since the previous block must have been a branch. first bb is special case (todo)
(defun stmt-to-basic-block (stmt)
	(prog1
			(when (eql *last-stmt* *index*)
				(case (car (statement stmt)) ; if branch, finialize the current basic block
					(if            (apply #'append `(,(list (finish-bb)) ,(if2-to-bb))))
					((unless when) `(,(finish-bb) ,(if1-to-bb)))
					(otherwise	; keep slurping next stmt into current block until we meet a branch
					 (incf *last-stmt*))))
		(incf *index*)))
(defun construct-basic-blocks (statements)
	(let ((*frst-stmt* 0)
				(*last-stmt* 0)
				(*index*     0)
				(*length* (length statements)))
		(declare (special *frst-stmt* *last-stmt* *index* *length*))
		(let ((bblocks (mapcar #'stmt-to-basic-block
													 statements)))
			(apply #'append (remove-if-not (lambda (x) (and (listp x) (not (null x)))) (append bblocks (list  (finish-bb))))))))

(defparameter parsed-code '(
														(setq x 12)
														))

(let* ((statements (construct-statement-vector parsed-code))
			 (bblocks (construct-basic-blocks statements)))
	(format t "~a~%~a~%" statements bblocks))

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
	(loop :for x :from 0 :to (1- (length statements))
		 :do (let ((stmt (statement (elt statements x)))
							 (stmt-block (elt statements x))
							 (stmt-gen-out (make-array 1 :fill-pointer 0 :adjustable t)))
					 (when (or (eq (car stmt) 'setq)(eq (car stmt) 'setf))
						 (vector-push-extend  (cons (nth 1 stmt) x) stmt-gen-out)
						 (setf (gen stmt-block) stmt-gen-out)
						 (setf (out stmt-block) stmt-gen-out)))))

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

(defun construct-kill-hashtable () ;;loop through each statement in statements if size of gen
	;;shoof (car gen) mawgod fel hashtable ? push-back fel associated vector (cdr gen) ;; : e3mel key fel hashtable bel car w e3mel array feeha cdr"
	(loop :for stmt :accross statements :do
		 (unless (zerop (length (gen stmt)))
			 (let ((associated-vector (make-array 1 :fill-pointer 0 :adjustable t))
						 (gen-cons (elt (gen stmt) 0)))
				 (if (gethash (car gen-cons) *kill-hash-table*)
						 (progn (setf associated-vector (gethash (car gen-cons) *kill-hash-table*))
										(vector-push-extend (cdr gen-cons) associated-vector)
										(setf (gethash (car gen-cons) *kill-hash-table*) associated-vector))
						 (progn (vector-push-extend (cdr gen-cons) associated-vector)
										(setf (gethash (car gen-cons) *kill-hash-table*) associated-vector)))))))
(defun kill-set ()
	(loop :for stmt :accross statements :do
		 (let ((kill-vector (make-array 1 :fill-pointer 0 :adjustable t)))
			 (unless (zerop (length (gen stmt)))
				 (loop :for x :accross (gethash (car (elt (gen stmt) 0)) *kill-hash-table*)
						:do
						(vector-push-extend (cons (car (elt (gen stmt) 0)) x) kill-vector))
				 (setf (kill stmt) kill-vector)))))

(defun value-circle (statement-index)
	(let ((inputs (cdr (nth 2 (statement (elt statements statement-index)))))
				(value-hash-table (make-hash-table))
				(in-in-set? nil))
		(loop :for input :in inputs :do
			 (loop :for in-element :accross  (in (elt statements statement-index)) :do
					(when (eq input (car in-element))
						(setf (gethash in-element value-hash-table) (value-circle (cdr in-element)))
						(setf in-in-set? t)))
			 (unless in-in-set?
				 (setf (gethash (cons input statement-index)value-hash-table)nil)))
		value-hash-table))

(defun traverse-hash-table (ht)
	(if ht
			(maphash #'(lambda (key associated-value)
									 (format t "~a: ~%" key)
									 (if ht (traverse-hash-table associated-value) (print "nil")))ht)
			(print "nil")))

;;main

(defparameter aggan-parsed-code
	'((setq input (+ 2 3))
		(send-to-other-process input)
		(setq received (recv-from other-process))
		(setq decision (min input received))
		(return decision)))

(construct-statement-vector aggan-parsed-code)
(construct-basic-blocks)
(leader-predecessors basic-blocks)
(gen-and-initial-out )
(construct-kill-hashtable)
(kill-set)
(reaching-definitions statements)

(loop
	 :for stmt :across statements
	 :for x :from 0
	 :do
	 (format t "statement ~a: " x)
	 (format t "~%gen: ") (map nil (lambda (x) (format "~a " x)) (gen stmt))
	 (format t "~%kill: ")(map nil (lambda (x) (format "~a " x)) (kill stmt))
	 (format t "~%in: ")  (map nil (lambda (x) (format "~a " x)) (in stmt))
	 (format t "~%out: ") (map nil (lambda (x) (format "~a " x)) (out stmt))
	 (format t"~%"))

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
														(* x 2)
														;; (unless (> y 30)
														;;	(print "y = 21"))
														))

(let* ((statements (construct-statement-vector parsed-code))
			 (bblocks (construct-basic-blocks statements)))
	(format t "~a~%~a~%" statements bblocks))
(frst-stmt-predecessors basic-blocks)
(gen-and-initial-out )
;;(reaching-definitions *statements*)
