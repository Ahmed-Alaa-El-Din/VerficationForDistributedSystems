(in-package #:statement)

(defclass stmt-block ()
	((id                      :accessor id                      :initarg :id)
	 (statement								:accessor statement								:initarg :statement)
	 (pred										:accessor pred										:initarg :pred)
	 (next										:accessor .next  :initform nil)
	 (block-id                :accessor block-id :initform nil)
	 (gen											:accessor gen   :initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (kill										:accessor kill	:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (in											:accessor .in		:initform (make-array 1 :fill-pointer 0 :adjustable t))
	 (out											:accessor out		:initform (make-array 1 :fill-pointer 0 :adjustable t))))

(defmethod print-object ((this stmt-block) out)
	(print-unreadable-object (this out :type t)
		(format out ":s ~17a :id ~3a :p ~8a :n ~2a :b ~a"
						(statement this) (id this) (pred this) (.next this) (block-id this))))

(defvar *id*)
(defvar *next-pred*)

(defun make-stmt-block (stmt &key (predecessors *next-pred*))
	(prog1 (make-instance 'stmt-block
												:id (1- (incf *id*))
												:statement stmt
												:pred (if (equalp predecessors '(-1)) nil predecessors))
		(setf *next-pred* (list (1- *id*)))))

;; these convert a stmt to several stmt-blocks
(defun convert-if2  (stmt)
	(destructuring-bind (if cond yes no) stmt
		(let* ((condition  (make-stmt-block (list if cond)))
					 (yes-branch (make-stmt-block yes :predecessors (list (id condition))))
					 (no-branch  (make-stmt-block no  :predecessors (list (id condition)))))
			(setf *next-pred* (list (id yes-branch) (id no-branch)))
			(list condition yes-branch no-branch))))

(defun convert-if1  (stmt)
	(destructuring-bind (if cond yes) stmt
		(let* ((condition  (make-stmt-block (list if cond)))
					 (yes-branch (make-stmt-block yes :predecessors (list (id condition)))))
			(setf *next-pred* (list (id condition) (id yes-branch)))
			(list condition yes-branch))))

;; (defvar *stmt-dispatch* `((if     . ,#'convert-if2)
;;													(unless . ,#'convert-if1)
;;													(when   . ,#'convert-if1)))

;; (defun dispatch (key args dispatch)
;;	(let ((fn (cdr (assoc key dispatch))))
;;		(when fn (funcall fn args))))

;; (defun convert-stmt (stmt)
;;	(or (dispatch (car stmt) stmt *stmt-dispatch*)
;;			(list (make-stmt-block stmt))))

(defun convert-stmt (stmt)
	(case (car stmt)
		(if (convert-if2 stmt))
		((when unless) (convert-if1 stmt))
		(t (list (make-stmt-block stmt)))))

(defun find-pred-references (&key to in)
	(flet ((refers-to-id (stmt) (member to (pred stmt))))
		(mapcar #'id (remove-if-not #'refers-to-id in))))

(defun convert-program (program-code)
	(flatten-list (mapcar #'convert-stmt program-code)))

(defun update-next-refs! (statements)
	(flet ((update! (stmt)
					 (setf (.next stmt)
								 (find-pred-references :to (id stmt)
																			 :in statements))))
		(mapc #'update! statements)))

(defun surrogate-code (code)
	(append '((BEGIN))
					code
					'((END))))

(defun construct-statement-vector (program-code)
	(let ((*id* 0)
				 (*next-pred* (list -1)))
		(update-next-refs! (convert-program (surrogate-code program-code)))))
