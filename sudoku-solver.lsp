(defun sudoku-solver(matrix)
	(solver matrix 1)
) ;;; main function

(defun solver(matrix num)
    (if (not (car (findEmpty matrix 0 0)))
		(print matrix)
		(if (> (+ 1 num) 4)
			nil ;;; backtrack
			(if (posSol matrix (car (cdr (findEmpty matrix 0 0))) (cdr (cdr (findEmpty matrix 0 0))))
				(progn 
					(rewrite matrix (car (cdr (findEmpty matrix 0 0))) (cdr (cdr (findEmpty matrix 0 0))) num)
					(if (solver matrix num)
						t ;;; the answer was correct
					)
					(rewrite matrix (car (cdr (findEmpty matrix 0 0))) (cdr (cdr (findEmpty matrix 0 0))) 0)
				) 
				(solver matrix (+ 1 num)) ;;; check out next number
			)
		) 
	)
) ;;; solver

(defun findEmpty(matrix row col)
	(if (equal 0 (nth col(nth row matrix)))
		(list 't row col)
		(cond 
			((< (+ 1 col) 4) (findEmpty matrix row (+ 1 col)))
			((< (+ 1 row) 4) (findEmpty matrix (+ 1 row) col))			
			(t (list 'nil))
		)
	)
)

(defun rowCheck(matrix row col num) ;;; col = 0 initially
	(if (equal num (nth col(nth row matrix)))
		t
		(if (< (+ 1 col) 4) (rowCheck matrix row (+ 1 col) num)
			nil
		)
	)
)

(defun colCheck(matrix row col num) ;;; row = 0 init
	(if (equal num (nth col(nth row matrix)))
		t
		(if (< (+ 1 row) 4) (colCheck matrix (+ 1 row) col num)
			nil
		)
	)
)

(defun boxCheck(matrix boxStartRow boxStartCol num)
	(if (equal num (nth (+ col boxStartCol) (nth (+ row boxStartRow) matrix)))
		t
		(cond 
			((< (+ 1 col) 2) (findEmpty matrix row (+ 1 col)))
			((< (+ 1 row) 2) (findEmpty matrix (+ 1 row) col))			
			(t nil)			
		)
	)
)

(defun posSol(matrix row col num) 
	(if (and (not (rowCheck matrix row 0 num))
			 (not (colCheck matrix 0 col num))
			 (not (boxCheck matrix (- row (mod row 2)) (- col (mod col 2)) num))
		)
		t
		nil
	)
)

(defun rewrite()
)
