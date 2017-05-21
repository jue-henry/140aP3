(defun prime(end)
  (cond
    ((< end 2) nil)
    ((< end 3) (cons 2 '() ))
    ((= end 3) (list 2 3 ))
    (t (nextPrime (list 2 3) 5 end))
  )
)

(defun nextPrime(l cur end)
  (if (> cur end)
    l
    (if (isPrime cur 2)
      (nextPrime (append l (cons cur '())) (+ 2 cur) end)
      (nextPrime l (+ 2 cur) end)
    ) 
  ) 
)

(defun isPrime(cur div)
  (cond
    ((= cur div) t)
    ((= (mod cur div) 0) nil)
    ((= div 2) (isPrime cur (+ div 1)))
    (t (isPrime cur (+ div 2)))
  )  
) 

