(defun sudoku-solver(matrix)
	(if (solver matrix 1) 
		(cons t (cons matrix'()))
		(cons nil (cons matrix'()))
	)
	;;(append original (cons matrix '()))
	;;(if (solver matrix 1)
		;;(print matrix)
		;;(print original))
) ;;; main function

(defun solver(matrix num)
    (if (not (car (findEmpty matrix 0)))
		t
		(if (> (+ 1 num) 4)
			nil ;;; backtrack
			(if (posSol matrix (car (cdr (findEmpty matrix 0))) (car (cdr (cdr (findEmpty matrix 0)))) num)
				(progn 
					(setf (nth (car (cdr (cdr (findEmpty matrix 0)))) (nth (car (cdr (findEmpty matrix 0))) matrix)) num)
					(if (solver matrix num)
						t ;;; the answer was correct
						(setf (nth (car (cdr (cdr (findEmpty matrix 0)))) (nth (car (cdr (findEmpty matrix 0))) matrix)) 0)
					)
				) 
				(solver matrix (+ 1 num)) ;;; check out next number
			)
		) 
	)
) ;;; solver

(defun findEmpty(matrix index)
	(if (equal 0 (nth (mod index 4)(nth (floor index 4) matrix)))
		(list 't (floor index 4) (mod index 4))
		(cond 
			((< (+ 1 index) 16) (findEmpty matrix (+ index 1)))	
			(t (list 'nil))
		)
	)
)

(defun rowCheck(matrix row colb num) ;;; col = 0 initially
	(if (equal num (nth colb(nth row matrix)))
		t
		(if (< (+ 1 colb) 4) (rowCheck matrix row (+ 1 colb) num)
			nil
		)
	)
)

(defun colCheck(matrix row colc num) ;;; row = 0 init
	(if (equal num (nth colc(nth row matrix)))
		t
		(if (< (+ 1 row) 4) (colCheck matrix (+ 1 row) colc num)
			nil
		)
	)
)

(defun boxCheck(matrix index num)
	(if (equal num (nth (mod 4 index) (nth (floor 4 index) matrix)))
		t
		(cond 
			((< (+ 1 index) 16) (boxCheck matrix (+ 1 index) num))		
			(t nil)			
		)
	)
)

(defun posSol(matrix row colf num) 
	(if (and (not (rowCheck matrix row 0 num))
			 (not (colCheck matrix 0 colf num))
			 (not (boxCheck matrix (+ colf (* row 4)) num))
		)
		t
		nil
	)
)
