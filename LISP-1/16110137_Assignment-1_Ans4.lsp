(defun fifth (a)
(if (> 5 (length a)) (return-from fifth NIL))
(car (cddddr a)))