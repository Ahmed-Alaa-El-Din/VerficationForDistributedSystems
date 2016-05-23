(in-package :utils)

(defun flatten-list (list)
	(cond ((null list) nil)
				((atom list) (list list))
				(t (mapcan #'flatten-list list))))
