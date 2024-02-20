(defun .member(a l)
    (if l
        (if (eql a (car l))
            T
            (.member a (cdr l))
        )
        NIL
    ))

(defun .remove-all(a l)
	(if l
		(if (eql a (car l))
			(.remove-all a (cdr l))
			(cons (car l) (.remove-all a (cdr l)))
			)
		NIL
		)
	)

(defun .foldl(l f v)
    (if
     (null l) v
     (.foldl(cdr l) f (funcall f v(car l)))
     )
    )


(defun .sum (L)
   (if L
      (+ (car L) (.sum (cdr L))) 
      0)
)


(defun .alreadyExist(a b)
	(if a
		(if (eql (car a) b)
			T
			(.alreadyExist (cdr a) b)
			)
		NIL
	)
	)

(defun .insert(a l)
	(if (.alreadyExist l a)
		l
		(cons a l)
		)
)

(defun .intersection(a b)
    (if a
        (if (.alreadyExist b (car a))
            (cons (car a) (.intersection (cdr a) b))
            (.intersection (cdr a) b)
        )
    )
)


(defun .subsetp(a b)
	(if a
	    (if (.alreadyExist b (car a))
		(.subsetp (cdr a) b)
		NIL
		)
	T
	)
)

(defun .supersetp(a b)
    (.subsetp b a))

(defun .cardinality(a)
    (if a
        (+ 1 (.cardinality (cdr a)))
        0
    )
)

(defun .factorial (a) 
	(if (eql a 0)
		1
		(* a (.factorial (- a 1)))
	)
)

(defun .gcd(a b)
    (if (> a b)
        (if (eql b 0)
            a
            (.gcd b (- a b))
        )
        (if (eql a 0)
            b
            (.gcd a (- b a))
        )
    )
)



(defun .pow(x y)
    (if (= y 0) 1
         (if (> y 0) (* x (.pow x (- y 1)))
             (/ 1 (.pow x (- 0 y)))
         )
    )
)



(defun .with-annual-interest(p r n)
    (* p (.pow (+ r 1) n))
)

(princ "CSC173 Project3")
(terpri)
(terpri)
(princ "ZHEYI FU 31982405")
(terpri)
(princ "Ruoxuan Lin 32022292")
(terpri)
(princ "Yuxuan Lei 31983570")
(terpri)
(terpri)


