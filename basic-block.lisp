(in-package #:cfg)

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
					(if            (list (finish-bb) (if2-to-bb)))
					((unless when) (list (finish-bb) (if1-to-bb)))
					(otherwise	; keep slurping next stmt into current block until we meet a branch
					 (prog1 nil (incf *last-stmt*)))))
		(incf *index*)))

(defun construct-basic-blocks (statements)
	(let ((*frst-stmt* 0)
				(*last-stmt* 0)
				(*index*     0)
				(*length* (length statements)))
		(let ((bblocks (mapcar #'stmt-to-basic-block
													 statements)))
			(utils::flatten-list (append bblocks (list  (finish-bb)))))))

(defparameter parsed-code '(
														(setq x 12)
														))

(defun frst-stmt-predecessors (*basic-blocks* *statements*)
	"loop through basic blocks w a5ali l predecessors bto3 l frst-stmt bta3 l block yeb2o vector of last-stmt-indexes of predecessors"
	(map nil (lambda (basic-block)
						 (setf (pred (elt *statements* (frst-stmt basic-block)))
									 (mapcar (lambda (predecessor) (last-stmt (elt *basic-blocks* predecessor)))
													 (predecessor-blocks basic-block))))
			 *basic-blocks*))
