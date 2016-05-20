(defclass vertix ()
  ( (id :accessor vertix-id   ;;slot
	  :initarg :id      ;;slot option   
	  )
   (self :accessor self
	:initarg :self
	  )
    (other :accessor other
	:initarg :other
	)))

(defun construct-vertix (id self other)
  (make-instance 'vertix :id id :self self :other other))

(defclass simplix ()
  ( (id :accessor vertices   ;;slot
	  :initarg :vertices      ;;slot option   
	  )))

(defun construct-simplix (vertix &rest vertices)
(defparameter vec (make-array 1 :fill-pointer 0 :adjustable t))
(vector-push-extend vertix vec)
(loop  for x in vertices
do
(vector-push-extend x vec))
(make-instance 'simplix :vertices vec))

(defclass complix ()
( (simplices :accessor simplices ;;slot
	     :initarg  simplices ;;slot option
)))

(defun construct-complix (&rest simplices)
(defparameter vec (make-array 1 :fill-pointer 0 :adjustable t))
(loop for x in simplices
do
(vector-push-extend x vec))
(make-instance 'complix :simplices vec))


(defun in-complex? (complex simplex)
  (loop for x in complex
     do
       (when (equalp x simplex)(return-from in-complex? t))
       )
  )


(defun add-to-complex-if-not-already-exist (complex simplex)
  (if (in-complex? complex simplex)
		   (return-from add-to-complex-if-not-already-exist complex)
		    (vector-push-extend simplex complex)))

(defvar first-process )
(defvar second-process)

(defun carrier-map (simplex)

  (setq first-process (elt(vertices simplex)0))
  (setq second-process (elt(vertices simplex)1))
  (setf (other first-process)(self second-process))
  (setf (other second-process)(self first-process))
  (construct-simplix first-process second-process)
  )

(defun decision-map (vertex decision-function)
  (setf (self vertex) (funcall decision-function (self vertex) (other vertex)))
  (setf (other vertex) nil)
  )

(defun minimum (x y)
  (if (< x y) x y))

(defvar global-decision-function) ;; will be assigned through the main function
  
(defun simplicial-decision-map (simplex)
  (setq first-process (elt(vertices simplex)0))
  (setq second-process (elt(vertices simplex)1))
  (construct-simplix (decision-map first-process global-decision-function)
		     (decision-map second-process global-decision-function))
  )

(defparameter p/o-complex (construct-complix))

(defun construct-p/o-complex (i/p-complex map) ;;p"protocol" i"input" o"output"
  (setq p/o-complex nil)
  (loop for simplex in i/p-complex
       do
       (setq p/o-complex
	     (add-to-complex-if-not-already-exist p/o-complex
						  (funcall map simplex)))
       )
  p/o-complex
  )
 
