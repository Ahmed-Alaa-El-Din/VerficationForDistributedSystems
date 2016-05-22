(defpackage :cfg
	(:use :common-lisp))

(defpackage :main
	(:use :common-lisp :cfg)
	(:export main))
