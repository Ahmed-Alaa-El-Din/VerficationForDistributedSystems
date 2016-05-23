(in-package :dataflow)
;;;;; DFG ;;;;;

(defun gen-and-initial-out (statements)
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
		 :across w
		 :do (setq v (remove x v :test #'equalp)))
	v)

(defun reaching-definitions (statements)
	(let ((change t)
				(old-out))
		(loop
			 :while change
			 :do (setf change nil)
			 (loop
					:for stmt
					:across statements
					:do (setf (in stmt) (in-set stmt))
					(setf old-out (out stmt))
					(setf (out stmt) (vector-union (gen stmt) (vector-difference (in stmt)(kill stmt))))
					(unless (equalp (out stmt) old-out);;without consedring sorting
						(setf change t))))))

(defun in-set (statement statements) ;;block .. magarabthash le7ad delwa2ti
	(let ((in ))
		(loop for predecessor-index being the element of (pred statement)
			 do (setf in (vector-union in (out (elt statements predecessor-index)))))
		in))

(defparameter *kill-hash-table* (make-hash-table))

(defun construct-kill-hashtable (statements) ;;loop through each statement in statements if size of gen
	;;shoof (car gen) mawgod fel hashtable ? push-back fel associated vector (cdr gen) ;; : e3mel key fel hashtable bel car w e3mel array feeha cdr"
	(loop :for stmt :across statements :do
		 (unless (zerop (length (gen stmt)))
			 (let ((associated-vector (make-array 1 :fill-pointer 0 :adjustable t))
						 (gen-cons (elt (gen stmt) 0)))
				 (if (gethash (car gen-cons) *kill-hash-table*)
						 (progn (setf associated-vector (gethash (car gen-cons) *kill-hash-table*))
										(vector-push-extend (cdr gen-cons) associated-vector)
										(setf (gethash (car gen-cons) *kill-hash-table*) associated-vector))
						 (progn (vector-push-extend (cdr gen-cons) associated-vector)
										(setf (gethash (car gen-cons) *kill-hash-table*) associated-vector)))))))
(defun kill-set (statements)
	(loop :for stmt :across statements :do
		 (let ((kill-vector (make-array 1 :fill-pointer 0 :adjustable t)))
			 (unless (zerop (length (gen stmt)))
				 (loop :for x :across (gethash (car (elt (gen stmt) 0)) *kill-hash-table*)
						:do
						(vector-push-extend (cons (car (elt (gen stmt) 0)) x) kill-vector))
				 (setf (kill stmt) kill-vector)))))

(defun value-circle (statement-index statements)
	(let ((inputs (cdr (nth 2 (statement (elt statements statement-index)))))
				(value-hash-table (make-hash-table))
				(in-in-set? nil))
		(loop :for input :in inputs :do
			 (loop :for in-element :across  (in (elt statements statement-index)) :do
					(when (eq input (car in-element))
						(setf (gethash in-element value-hash-table) (value-circle (cdr in-element) statements))
						(setf in-in-set? t)))
			 (unless in-in-set?
				 (setf (gethash (cons input statement-index)value-hash-table)nil)))
		value-hash-table))

(defun traverse-hash-table (ht)
	(when ht
			(maphash #'(lambda (key associated-value)
									 (format t "~a: ~%" key)
									 (when associated-value (traverse-hash-table associated-value)))
							 ht)))

;;main

;; (defparameter aggan-parsed-code
;;	'((setq input (+ 2 3))
;;		(send input)
;;		(setq received (recv))
;;		(setq decision (min input received))
;;		(return decision)))

;; (construct-statement-vector aggan-parsed-code)
;; (construct-basic-blocks)
;; (leader-predecessors basic-blocks)
;; (gen-and-initial-out )
;; (construct-kill-hashtable)
;; (kill-set)
;; (reaching-definitions statements)

;; (loop
;;	 :for stmt :across statements
;;	 :for x :from 0
;;	 :do
;;	 (format t "statement ~a: " x)
;;	 (format t "~%gen: ") (map nil (lambda (x) (format t "~a " x)) (gen stmt))
;;	 (format t "~%kill: ")(map nil (lambda (x) (format t "~a " x)) (kill stmt))
;;	 (format t "~%in: ")  (map nil (lambda (x) (format t "~a " x)) (in stmt))
;;	 (format t "~%out: ") (map nil (lambda (x) (format t "~a " x)) (out stmt))
;;	 (format t"~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defparameter parsed-code '((setq x 12)
;;														(setq y 21)
;;														(if (= y 12)
;;																(setq y (* y 2))
;;																(setq x 42))
;;														(setq w 12)
;;														(+ 2 3)
;;														(setf z 22)
;;														(when (> x 20)
;;															(setq x (+ x 1)))
;;														(setq zz 18)
;;														(* 3 w)
;;														(unless (> y 30)
;;															(print "y = 21"))
;;														(square y)
;;														(* x 2)
;;														;; (unless (> y 30)
;;														;;	(print "y = 21"))
;;														))

;; (let* ((statements (construct-statement-vector parsed-code))
;;			 (bblocks (construct-basic-blocks statements)))
;;	(format t "~a~%~a~%" statements bblocks))
;; (frst-stmt-predecessors basic-blocks)
;; (gen-and-initial-out )
;; ;;(reaching-definitions statements)
