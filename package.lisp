(defpackage :utils
	(:use :common-lisp))

(defpackage :cfg
	(:use :common-lisp :utils))

(defpackage :main
	(:use :common-lisp :cfg)
	(:export main))
