(defun my-member (x y)
(if (equal (length y) 0) (return-from my-member nil))
(if (equal x (car y)) (return-from my-member y))
(my-member x (cdr y)))
