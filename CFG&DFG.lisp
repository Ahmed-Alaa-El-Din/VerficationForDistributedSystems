(defclass statement-block ()
  ( (statement :accessor statement
	       :initarg :statement )
    (gen :accessor gen :initform (make-array 1 :fill-pointer 0 :adjustable t))
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
(defparameter linkage-hash-table (make-hash-table))


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

(defun equalp-considering-sorting (v w)"assuming unique elements in each vector"
  (eq (length v) (length (vector-union v w))))

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
	      ;;(unless (equalp (out stmt) old-out);;without consedring sorting
	      (unless (equalp-considering-sorting (out stmt) old-out)
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


(defun value-circle (statement-index)
  (let ((inputs (if (atom (nth 2 (statement (elt statements statement-index))))
		    (list (nth 2 (statement (elt statements statement-index))))
		    (cdr (nth 2 (statement (elt statements statement-index))))))
	(value-hash-table (make-hash-table))
	(in-in-set? nil))
    ;; ana ha3oz l value-circle fel setq,setf,send-to-other-process .. l variable elly ana ba3oz l value circle bta3o bieb2a (nth 2),(nth 2),(nth 1)
    (when (eq (car (statement (elt statements statement-index))) 'send-to-other-process ) (setq inputs (list(nth 1 (statement (elt statements statement-index)))))) ;; assuming that the parameter of send-to.. isn't a function "full view communication"
    (mapcar (lambda (input)    
       (loop :for in-element :being :the :element :of  (in (elt statements statement-index)) :do
	  (when (eq input (car in-element))
	    (setf (gethash in-element value-hash-table) (value-circle (cdr in-element)))
	    (setf in-in-set? t)))
       (if in-in-set?
	   (setf in-in-set? nil)
	   (setf (gethash (cons input statement-index)value-hash-table)nil)))inputs)
    value-hash-table))
	       
	      
(defun traverse-hash-table (ht)
  (if ht 
  (maphash #'(lambda (key associated-value)
	       (format t "new hashtable : ~a: ~%" key)
	       (if (eq key 'decision-function) ;;for linkage-hash-table only
		   (format t "~a ~%" associated-value)
		   (traverse-hash-table associated-value)))ht)
  (format t "nil ~%")))


(defun fill-linkage-hash-table ()
  (loop :for stmt :being :the :element :of statements :do
     (cond ((eq (car (statement stmt)) 'send-to-other-process)
	    (loop :for in-element :being :the :element :of (in stmt) :do
	       (when (eq (car in-element) (nth 1 (statement stmt)))
		 (setf (gethash 'input1 linkage-hash-table) (value-circle (cdr in-element))) (setf (gethash 'input2 linkage-hash-table) (value-circle (cdr in-element))))))
		      ;;nth 2 3ashan mafrod tkon l form (setq x (recv-from,,,))
	    ((eq (car (nth 2 (statement stmt))) 'recv-from-other-process)
	    (setf (gethash 'recieved1 linkage-hash-table) (gethash 'input2 linkage-hash-table))
	     (setf (gethash 'recieved2 linkage-hash-table) (gethash 'input1 linkage-hash-table)))))

  ;; pushing decision function
  (let ((stmt (if (eq (car (statement (elt statements (1- (length statements))))) 'return)
		  ;;(cdr (statement (elt statements (1- (length statements))))) "returns list even if one element"
		  (nth 1 (statement (elt statements (1- (length statements)))))
		  (statement (elt statements (1- (length statements)))))))
    (if (atom stmt)
	(setf (gethash 'decision-function linkage-hash-table) #'_identity)
        (setf (gethash 'decision-function linkage-hash-table) (car stmt)))))
	
  

(defun _identity (x)
  x)
	    
     






;;main

(defparameter aggan-parsed-code '(
(setq x (* 2 3))
(setq y (+ 44 55))
(setq w (square x))
(setq z (* y w 4))
(setq input (+ z w))
(send-to-other-process input)
(setq received (recv-from-other-process))
(setq decision (min input received))
(return decision)))
			     


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

(construct-statement-vector aggan-parsed-code)
(construct-basic-blocks)
(leader-predecessors basic-blocks)
(gen-and-initial-out )
(construct-kill-hashtable)
(kill-set)
(reaching-definitions statements)
(fill-linkage-hash-table)
(traverse-hash-table linkage-hash-table)




(loop :for x :from 0 :to (1- (length statements)) :do
	    (format t "statement ~a \"~a \": ~%gen: " x (statement (elt statements x)))
	    (loop :for g :being :the :element :of (gen (elt statements x)) :do
	       (format t "~a " g))
	    (format t "~%kill: ")
	    (loop :for k :being :the :element :of (kill (elt statements x)) :do
	       (format t "~a " k))
	    (format t "~%in: ")
	    (loop :for i :being :the :element :of (in (elt statements x)) :do
	       (format t "~a " i))
	    (format t "~%out: ")
	    (loop :for o :being :the :element :of (out (elt statements x)) :do
	       (format t "~a " o)) 
	    (format t"~%"))
