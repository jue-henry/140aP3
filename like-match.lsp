(defun like-match(s1 s2)
  (cond
    ((= (length s1) 0) (list nil nil))
    ((= (length s2) 0) (list nil nil))
    ((char= (char s1 0) #\_) 
        (next (cons (subseq s2 0 1) '()) (subseq s1 1) (subseq s2 1)))
    ((char= (char s1 0) #\%) (percent nil s1 s2 nil))
    ((char= (char s1 0) (char s2 0)) (next nil (subseq s1 1) (subseq s2 1)))
    (t (list nil nil))   
  )
)

(defun next(lis s1 s2) 
  (cond
    ((= (length s1) 0) 
      (if (= (length s2) 0)
        (list t lis) 
        (list nil nil)
      )
    )
    ((= (length s2) 0) (list nil nil))  
    ((char= (char s1 0) #\_) 
        (next (append lis (cons (subseq s2 0 1) '())) (subseq s1 1) (subseq s2 1)))
    ((char= (char s1 0) #\%) (percent lis s1 s2 nil))
    ((char= (char s1 0) (char s2 0)) (next lis (subseq s1 1) (subseq s2 1)))
    (t (list nil nil))   
  )    
)

(defun percent(final s1 s2 temp)
  (cond
    ((= (length s2) 0) 
      (if (= (length s1) 1)
        (list t (append final (cons temp '())))
        (list nil nil)
      )
    )
    ((= (length s1) 1) 
      (if (not temp)
        (percent final s1 (subseq s2 1) (subseq s2 0 1))
        (percent final s1 (subseq s2 1) (concatenate 'string temp (subseq s2 0 1)))
      )
    )
    ((char= (char s1 1) #\%) 
      (percent (append final (cons temp '())) (subseq s1 1) s2 nil))
    ((char= (char s1 1) #\_)
      (next (append final (cons temp '())) (subseq s1 1) s2))
    ((char= (char s1 1) (char s2 0)) 
      (next (append final (cons temp '())) (subseq s1 2) (subseq s2 1)))
    ((not temp) (percent final s1 (subseq s2 1) (subseq s2 0 1)))
    (t (percent final s1 (subseq s2 1) (concatenate 'string temp (subseq s2 0 1))))
  )
)
