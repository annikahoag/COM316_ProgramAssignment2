(define sqr-list
	(lambda (lst)
		(cond 
			[(null? lst) '()]
			[ (cons (* (car lst) (car lst)) (sqr-list (cdr lst)))])))

;--------------------------------------------------------------------------------------

(define place
	(lambda (x lst)
		(if (null? lst)
			(list x)
			;else
			(place2 x (car lst) (cdr lst)))))

(define place2
	(lambda (x carlst cdrlst)
		(if (or (null? carlst) (null? cdrlst)) x)
			(cond
				[(and (>= x carlst) (< x (car cdrlst))) (list carlst x cdrlst)]
				[else (place2 x (car cdrlst) (cdr cdrlst))])))

;--------------------------------------------------------------------------------------

(define change
	(lambda (x)
		(cond 
			((< x 0) (* x x))
			((= x 0) 0)
 			((> x 0) (+ x 1)))))

;--------------------------------------------------------------------------------------

(define change-list
	(lambda (lst)
		(if (null? lst) '()
			(cons (change (car lst)) (change-list (cdr lst)) ))))

;--------------------------------------------------------------------------------------

(define closest-point
	(lambda (point lst)
		(let ([dst (dist point (car lst))])
			(closest-point-help point lst dst (car lst)))))

(define closest-point-help
	;pt=point we're comparing, dst=the dist we're comparing with, best-pt=current best point
     (lambda (pt lst dst best-pt)
        (cond
           [(null? lst) best-pt]
           [(null? (cdr lst))      
			(if (< dst (dist best-pt pt))
     		          (closest-point-help pt (cdr lst) (dist pt (car lst)) (car lst))
             	         ;else
              	        (closest-point-help pt (cdr lst) (dist pt (car lst)) best-pt)))]
           [else 
             (begin                                                                   
              (if (< dst (dist best-pt pt))
                 (closest-point-help pt (cdr lst) (dist pt (cadr lst)) (car lst))
              ;else
              (closest-point-help pt (cdr lst) (dist pt (cadr lst)) best-pt)))])))


(define dist
	(lambda (pointa pointb)
		(sqrt (+ (* (- (car pointb) (car pointa))
				(- (car pointb) (car pointa)) )
			  (* (- (cadr pointb) (cadr pointa))
				(- (cadr pointb) (cadr pointa)) )))))

;--------------------------------------------------------------------------------------

(define add-list
	(lambda (lst1 lst2)
		(add-list-help (cdr lst1) (cdr lst2) (+ (car lst1) (car lst2)))))

(define add-list-help
	(lambda (lst1 lst2 sum)
		(if (null? lst1) 
			(list sum)
			(cons sum
				(add-list-help (cdr lst1) (cdr lst2) (+ (car lst1) (car lst2)))))))

;--------------------------------------------------------------------------------------

(define delete-lists
	(lambda (lst)
		(if (null? lst) '()
		;else
		(cond 
			[(not (list? (car lst))) (append (car lst) (delete-lists (cdr lst)))]
			[else (delete-lists (cdr lst))]))))

;--------------------------------------------------------------------------------------

(define flatten 
	(lambda (lst)
		(if (null? lst) '()
			(cond
				[(list? (car lst)) (append (unlist (car lst)) (flatten (cdr lst)))]
				[else (cons (car lst) (flatten (cdr lst)))]))))

(define unlist
	(lambda (lst)
		(if (null? lst) '()
			(cond 
				[(list? (car lst)) (append (unlist (car lst)) (cdr lst))]
				[else (cons (car lst) (unlist (cdr lst)))]))))

;--------------------------------------------------------------------------------------

;20 Commands from Chapter 6 and my testing of them, the results are commented below each command

;Command 1:
(eq? "A" "A")
;#f
(eq? 1 1)
;#t
(eq? "A" "a")
;f
(eq? + +)
;#t

;Command 2:
(eqv? "A" "A")
;#f
(eqv? 1 1)
;#t
(eqv? "a" "A")
;#f
(eqv? + +)
;#t

;Command 3:
(equal? "A" "A")
;#t
(equal? 1 1)
;#t
(equal? "a" "A")
;#f
(equal? + +)
;#t 

;Command 4:
(let ([x (list 'a 'b 'c)])
	(set-car! x 1)
	x)
;(1 b c)
(let ([x (list 'a 'b 'c)])
	(set-car! x 1)
	(car x))
;1

;Command 5:
(let ([x (list 'a 'b 'c)])
	(set-cdr! x 1)
	x)
;(a.1)
(let ([x (list 'a 'b 'c)])
	(set-cdr! x 1)
	(cdr x)
;1

;Command 6:
(caar '((a)))
;a
(cadr '(a b c))
;b
(cdddr '(a b c d))
;(d)
(cadadr '(a b c)))
;c

;Command 7:
(define list-ref 
	(lambda (ls n)
		(if (= n 0)
			(car ls)
			(list-ref (cdr ls) (- n 1)))))
(list-ref '(a b c) 0)
;a
(list-ref '(a b c) 1)
;b
(list-ref '(a b c) 2)
;c

;Command 8:
(define reverse 
	(lambda (ls)
		(let rev ([ls ls] [new '()])
			(if (null? ls)
				new 
				(rev (cdr ls) (cons (car ls) new))))))
(reverse '())
;()
(reverse '(a b c))
;(c b a)

;Command 9:
(define memq
	(lambda (x ls)
		(cond 
			[(null? ls) #f]
			[(eq? (car ls) x) ls]
			[else (memq x (cdr ls))])))
(memq 'a '(b c a d e))
;(a d e)
(memq 'a '(b c d e g))
;#f
(memq 'a '(b a c a d a))
;(a c a d a)

;Command 10:
(remq 'a '(b c d))
;(b c d)
(remv 'a '(b c d))
;(b c d)
(remv 1/2 '(1.2 1/2 0.5 3/2 4))
;(1.2 0.5 3/2 4)
(remove '(b) '((a) (b) (c)))
;((a) (c))

;Command 11:
(filter odd? '(1 2 3 4))
;( 1 3)
(filter even? '(1 2 3 4))
;(2 4)
(filter 
	(lambda (x) (and (> x 0) (< x 10)))
	'(-5 15 3 14 -20 6 0 -9))
;(3 6)
(filter 
	(lambda (x) (= x 14))
	'(-5 15 3 14 -20 6 0 -9))
;(14)

;Command 12:
(vector)
;#()
(vector 'a 'b 'c)
;#(a b c)

;Command 13:
(make-vector 0)
;#()
(make-vector 0 '#(a))
;#()
(make-vector 5 '#(a))
;#(#(a) #(a) #(a) #(a) #(a))
(make-vector 5)
;#(0 0 0 0 0)

;Command 14:
(vector-length '#())
;0
(vector-length '#(a b c))
;3
(vector-length (vector 1 '(2) 3 '#(4 5)))
;4
(vector length (make-vector 300))
;300

;Command 15:
(vector-ref '#(a b c) 0)
;a
(vector-ref '#(a b c) 1)
;b
(vector-ref '#(x y z w) 3)
;w

;Command 16:
(let ([v (vector 'a 'b 'c 'd 'e)])
	(vector-set! v 2 'x)
	v)
;#(a b x d e)

;Command 17:
(define vector-fill!
	(lambda (v x)
		(let ([n (vector-length v)])
			(do ([i 0 (+ i 1)])
				((= i n))
				(vector-set! v i x)))))
(let ([v (vector 1 2 3)])
	(vector-fill! v 0)
	v)
;#(0 0 0)
(let ([v (vector 1 2 3)])
	(vector-fill! v "a")
	v)
;#("a" "a" "a")

;Command 18:
(define vector->list
	(lambda (s)
		(do ([i (- (vector-length s) 1) (- i 1)]
			[ls '() (cons (vector-ref s i) ls)])
		((< i 0) ls)))
(vector->list (vector))
;()
(vector->list (vector))
;(a b c)
(let ((v '#(1 2 3 4 5))
	(apply * (vector->list v)))
;120

;Command 19:
(define list->vector
	(lambda (ls)
		(let ([s (make-vector (length ls))])
			(do ([ls ls (cdr ls)] [i 0 (+ i 1)])
				((null? ls) s)
				(vector-set! s i (car ls))))))
(list->vector '())
;#()
(list->vector '(a b c))
;#(a b c)
(let ([v '#(1 2 3 4 5)])
	(let ([ls (vector->list v)])
		(list->vector (map * ls ls))))
;#(1 4 9 16 25)

;Command 20:
(vector-sort < '#(3 4 2 1 2 5))
;#(1 2 2 3 4 5)
(vector-sort > '#(0.5 1/2))
;#(0.5 1/2)
(vector-sort > '#(1/2 0.5))
;#(1/2 0.5)
(let ([v (vector 3 4 2 1 2 5)])
	(vector-sort! < v)
	v)
;#(1 2 2 3 4 5)