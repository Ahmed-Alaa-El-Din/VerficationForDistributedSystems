(defclass statement-block ()
  ( (statement :accessor statement
	       :initarg :statement )
   (gen :accessor gen :initform (make-array 1 :fill-pointer 0 :adjustable t))
    ;; initform was nil 
    (kill :accessor kill
	  :initform (make-array 1 :fill-pointer 0 :adjustable t))
    (in :accessor in
	:initform (make-array 1 :fill-pointer 0 :adjustable t))
    (out :accessor out
	 :initform (make-array 1 :fill-pointer 0 :adjustable t))

    (predecessor-statements :accessor predecessor-statements
			    :initarg :predecessor-statements)))
    

(defclass basic-block ()
  ( (leader :accessor leader
	    :initarg :leader)
   (last-stmt :accessor last-stmt
	      :initarg :last-stmt)
    (next-block :accessor next-block)
    (predecessor-blocks :accessor predecessor-blocks
			:initarg :predecessor-blocks)))

(defparameter basic-blocks (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter statements (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter kill-hash-table (make-hash-table))


(defun return-condition (condition-statement) ;;like (if (= y 12))
  ;;(when (eq (car condition-statement) 'if)
  (cadr condition-statement))


(defun construct-statement-vector (program-code)
  (loop for stmt  in program-code
     do
       (cond ( (eq (car stmt) 'if)
	           (push-statement (list (nth 0 stmt)(nth 1 stmt)) (predecessors 1 statements))
	           (push-statement (nth 2 stmt)(predecessors 1 statements)) 
	           (push-statement (nth 3 stmt)(predecessors 2 statements)))
	     ( (or (eq (car stmt) 'when) (eq (car stmt) 'unless))
		   (push-statement (list (nth 0 stmt)(nth 1 stmt))(predecessors 1 statements))
	       (push-statement (nth 2 stmt)(predecessors 1 statements)))
             (t (push-statement stmt (predecessors 1 statements))))))


(defun leader-predecessors (basic-blocks)
  ;; loop through basic blocks w a5ali l predecessors bto3 l leader bta3 l block yeb2o vector of last-stmt-indexes of predecessors
  (loop for basic-block being the element of basic-blocks
     do
       (let ((pre (make-array 1 :fill-pointer 0 :adjustable t)))
	 (loop for predecessor being the element of (predecessor-blocks basic-block)
	  do
	      (vector-push-extend (last-stmt (elt basic-blocks predecessor)) pre)
	      )
	 (setf (predecessor-statements (elt statements (leader basic-block))) pre))))

(defun predecessors (number v) ;;v : statemenets or basic-blocks 
  (defparameter predecessor-vector (make-array 1 :fill-pointer 0 :adjustable t))
  (unless (eq (length v) 0)
    (vector-push-extend (- (length v) number) predecessor-vector))
  predecessor-vector)

(defun push-statement (stmt predecessors)
  (vector-push-extend (make-instance 'statement-block :statement stmt :predecessor-statements predecessors) statements))

(defun construct-basic-blocks ()
  (defparameter leader 0)
  (defparameter last-stmt 0)
  (loop for x from 0 to (1- (length statements))
     do
       (let ( (stmt (statement (elt statements x)))) 
       (cond ( (eq (car stmt) 'if)
	      (push-basic-block leader last-stmt (vector-union (predecessors 1 basic-blocks)(predecessors 2 basic-blocks)));;maybe wrong 
	      (push-basic-block leader last-stmt (predecessors 1 basic-blocks))
	      (push-basic-block leader last-stmt (predecessors 2 basic-blocks))
	      (setf x (+ x 2)))
	     ( (or (eq (car stmt) 'unless) (eq (car stmt) 'when))
	       (push-basic-block leader last-stmt (vector-union (predecessors 1 basic-blocks)(predecessors 2 basic-blocks)))
	       (push-basic-block leader last-stmt (predecessors 1 basic-blocks))
	       (incf x))
             (t (incf last-stmt)))))

  (push-basic-block leader (decf last-stmt) (vector-union (predecessors 1 basic-blocks)(predecessors 2 basic-blocks))))
	      
       
       
(defun push-basic-block (ldr lastt predecessors)
  (vector-push-extend (make-instance 'basic-block :leader ldr :last-stmt lastt :predecessor-blocks predecessors) basic-blocks )
	       (setf leader (incf last-stmt)))
	 

;;(defun next-block (statement) ;;statement block le7ad delwa2ti
  ;;(if (return-condition statement) ;;if condition is true
    ;;  (progn



;;;;; DFG ;;;;;
  
(defun gen-and-initial-out ()
  (loop for x from 0 to (1- (length statements))
     do
       (let ((stmt (statement (elt statements x))) 
	     (stmt-block (elt statements x))
	      (stmt-gen-out (make-array 1 :fill-pointer 0 :adjustable t))) 
	 (when (or (eq (car stmt) 'setq)(eq (car stmt) 'setf))
	   ;;(setf (gen stmt-block) (cons (nth 1 stmt) x))
	   (vector-push-extend  (cons (nth 1 stmt) x) stmt-gen-out)
	   ;;(vector-push-extend (gen stmt-block) stmt-out)
	   (setf (gen stmt-block) stmt-gen-out)
	   (setf (out stmt-block) stmt-gen-out)))))
;;(find '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => (1 2)

(defun vector-union (v w)
  (remove-duplicates (merge 'vector v w #'equalp) :test #'equalp))

(defun vector-difference (v w)
  (loop for x being the element of w
     do
       (setq v (remove x v :test #'equalp)))
  v)

(defun reaching-definitions (statements)
  (let ((change t)
	(old-out))
    (loop while change do
	 (setf change nil)
	 (loop for stmt being the element of statements
	    do
	      (setf (in stmt) (in-set stmt))
	      (setf old-out (out stmt))
	      (setf (out stmt) (vector-union (gen stmt) (vector-difference (in stmt)(kill stmt))))
	      (unless (equalp (out stmt) old-out);;without consedring sorting
		(setf change t)
		)))))
	 
  
  


(defun in-set (statement) ;;block .. magarabthash le7ad delwa2ti
  (let ((in ))
    (loop for predecessor-index being the element of (predecessor-statements statement)
       do
	 (setf in (vector-union in (out (elt statements predecessor-index)))))
    (return-from in-set in)))



(defun construct-kill-hashtable () ;;loop through each statement in statements if size of gen 
;;shoof (car gen) mawgod fel hashtable ? push-back fel associated vector (cdr gen) ;; : e3mel key fel hashtable bel car w e3mel array feeha cdr"
  (loop :for stmt :being :the :element :of statements :do
     (unless (eq 0 (length (gen stmt)))
       (let ((associated-vector (make-array 1 :fill-pointer 0 :adjustable t))
	     (gen-cons (elt (gen stmt) 0)))
	 (if (gethash (car gen-cons) kill-hash-table)
	     (progn
	       (setf associated-vector (gethash (car gen-cons) kill-hash-table))
	       (vector-push-extend (cdr gen-cons) associated-vector)
	       (setf (gethash (car gen-cons) kill-hash-table) associated-vector)
	       )
	     (progn
	       (vector-push-extend (cdr gen-cons) associated-vector)
	       (setf (gethash (car gen-cons) kill-hash-table) associated-vector)))))))

(defun kill-set ()
       (loop :for stmt :being :the :element :of statements :do
	  (let ((kill-vector (make-array 1 :fill-pointer 0 :adjustable t)))
	    (unless (eq 0 (length (gen stmt)))
	      (loop :for x :being :the :element :of (gethash (car (elt (gen stmt) 0)) kill-hash-table) :do
		 (vector-push-extend (cons (car (elt (gen stmt) 0)) x) kill-vector)
		 )
	      (setf (kill stmt) kill-vector)
	      ))))








;;main


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
(leader-predecessors basic-blocks)
(gen-and-initial-out )
(construct-kill-hashtable)
(kill-set)
(reaching-definitions statements)




(loop :for x :from 0 :to (1- (length statements)) :do
	    (format t "statement ~a: ~%gen: " x)
	    (loop :for g :being :the :element :of (gen (elt statements 
x)) :do
	       (format t "~a " g))
	    (format t "~%kill: ")
	    (loop :for k :being :the :element :of (kill (elt statements 
x)) :do
	       (format t "~a " k))
	    (format t "~%in: ")
	    (loop :for i :being :the :element :of (in (elt statements 
x)) :do
	       (format t "~a " i))
	    (format t "~%out: ")
	    (loop :for o :being :the :element :of (out (elt statements 
x)) :do
	       (format t "~a " o)) 
	    (format t"~%"))
