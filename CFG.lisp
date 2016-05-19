(defclass statement-block ()
  ( (statement :accessor statement
	       :initarg :statement )
   (gen :accessor gen
	:initform (make-array 1 :fill-pointer 0 :adjustable t))
    (kill :accessor kill
	  :initform (make-array 1 :fill-pointer 0 :adjustable t))
    (in :accessor in
	:initform (make-array 1 :fill-pointer 0 :adjustable t))
    (out :accessor out
	 :initform (make-array 1 :fill-pointer 0 :adjustable t)) ))

(defclass basic-block ()
  ( (leader :accessor leader
	    :initarg :leader)
   (last-stmt :accessor last-stmt
	      :initarg :last-stmt)
    (next-block :accessor next-block) ))

(defparameter basic-blocks (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter statements (make-array 1 :fill-pointer 0 :adjustable t))


(defun return-condition (condition-statement) ;;like (if (= y 12))
  ;;(when (eq (car condition-statement) 'if)
  (cadr condition-statement))

(defun construct-statement-vector (program-code)
  (loop for stmt  in program-code
     do
       (cond ( (eq (car stmt) 'if)
	           (push-statement (list (nth 0 stmt)(nth 1 stmt)))
	           (push-statement (nth 2 stmt)) 
	           (push-statement (nth 3 stmt)))
	     ( (or (eq (car stmt) 'when) (eq (car stmt) 'unless))
		   (push-statement (list (nth 0 stmt) (nth 1 stmt)))
	       (push-statement (nth 2 stmt)))
             (t (push-statement stmt)))))

(defun push-statement (stmt)
  (vector-push-extend (make-instance 'statement-block :statement stmt) statements))

(defun construct-basic-blocks ()
  (defparameter leader 0)
  (defparameter last-stmt 0)
  (loop for x from 0 to (1- (length statements))
     do
       (let ( (stmt (statement (elt statements x)))) 
       (cond ( (eq (car stmt) 'if)
	      (push-basic-block leader last-stmt)
	      (push-basic-block leader last-stmt)
	      (push-basic-block leader last-stmt)
	      (setf x (+ x 2)))
	     ( (or (eq (car stmt) 'unless) (eq (car stmt) 'when))
	       (push-basic-block leader last-stmt)
	       (push-basic-block leader last-stmt)
	       (incf x))
             (t (incf last-stmt)))))
  (push-basic-block leader (decf last-stmt)))
	      
       
       
(defun push-basic-block (ldr lastt )
  (vector-push-extend (make-instance 'basic-block :leader ldr :last-stmt lastt) basic-blocks)
	       (setf leader (incf last-stmt)))
	 

;;(defun next-block (statement) ;;statement block le7ad delwa2ti
  ;;(if (return-condition statement) ;;if condition is true
    ;;  (progn
      
  
