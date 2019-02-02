(defun fact (x)
(if (equal x 1) (return-from fact 1))          ;base condition
(if (equal x 0) (return-from fact 1))          ;for corner case when x = 0
(if (< x 0) (return-from fact NIL))
(if (not (typep x 'integer)) (return-from fact NIL)) ;check whether input variable is interger or not
(* x (fact (- x 1)))                           ;calculates the factorial value by making recursive call to the funtion fact 
)
