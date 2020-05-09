#lang racket
(provide expr-compare)
(define lambda2 (string->symbol "\u03BB"))
#| don't forget to include the lines above!
	otherwise there might be troubles
	this file is directly runnable by either
	    racket FILENAME.ss
	or, open it in DrRacket and click run

    Also, it can be loaded from racket using

    (require "FILENAME.ss")

    for basic syntax introduction, please see slides and hello.ss
|#

; hint on judging lambda
(define (lambda? x)
  (member x '(lambda λ)))

(define (symbol-checker ogx ogy x y)
  (cond [(equal? (car x) (car y)) (symbol-checker ogx ogy (cdr x) (cdr y))]
	[(not (equal? (car x) (car y))) (list 'if '% ogx ogy)]
	))

(define (param-checker x y)
 (cond [(equal? (length x) 0) '()]
  [(if (and(list? (car x))(list? (car y)))
     (begin
	(if (and (>(length x) 0) (>(length y) 0))
	    
	    (cons(list(string->symbol (string-append (symbol->string (car(car x))) "!" (symbol->string (car(car y))))))(list(param-checker (cdr x) (cdr y))))
	    (list(string->symbol (string-append (symbol->string (car(car x))) "!" (symbol->string (car(car y))))))))
     (if (> (length x) 0)
	 (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
	 (string->symbol (string-append (symbol->string  x) "!" (symbol->string y)))))]))

(define (diff x y)
  (cond [(xor (equal? x 'if) (equal? y 'if))
	 #t]
	[(xor (equal? x 'quote) (equal? y 'quote))
	 #t]
	[(xor (equal? (lambda? x) 'lambda) (equal? (lambda? y) 'lambda)) #t]
   )
  )
  
  
(define (is-lam x)
  (cond[(or (equal? x 'lambda) (equal? x 'λ)) 1]
       [else
	0]))

(define (lam-equal x y)
  (if (not (equal? x y)) (list lambda2) (list x))
  )

(define (lam-type x y)
  (let(
       [lamx (list-ref x 0)]
       [lamy (list-ref y 0)]
       [argx (cadr x)]
       [argy (cadr y)]
       [arggx (car(cdr(cdr x)))]
       [arggy (car(cdr(cdr y)))]
       )
      
    (if (> (length argx) 1)
      (append (lam-equal lamx lamy) (append ( list (list-checker argx argy #t #f)) (list(list-checker arggx arggy #t #f))))
      (append (lam-equal lamx lamy) (append ( list (list-checker argx argy #f #f)) (list(list-checker arggx arggy #t #f)))))
    )
  )
(define (list-checker x y flag flag2)
  (cond [(and (equal? (length x) 0) (equal? (length y) 0)) '()]
	[(not (equal? (length x) (length y))) (list 'if '% x y)]
	[(and (equal? (car x) 'quote) (equal? (car y) 'quote)) (symbol-checker x y x y)]
	[(xor (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y)]
	[(xor (equal? (car x) 'if) (equal? (car y) 'if)) (list 'if '% x y)]
	[(xor (equal? (lambda? (car x)) 'lambda) (equal? (lambda? (car y)) 'lambda)) (list 'if '% x y)]
	 ;[(equal? (diff (car x) (car y)) #t)
         ;(list 'if '% x y)]
	[(and (boolean? (car x)) (boolean? (car y)))
         
         (if (car x) (cons(list '%)(list-checker (cdr x) (cdr y) #f #f)) (cons(list 'not '%)(list-checker (cdr x)(cdr y) #f
#f)))]

	
   [(and (list? (car x)) (list? (car y))) 
		   (if (and(equal? flag #t) (not(equal? (car x) (car y))))
		       (begin
			 (if (and(not(pair? x)) (not(pair? y)))
			     (cons(param-checker x y)(list-checker (cdr x) (cdr y) #f #f))
			     (param-checker x y)))
		       (cons (list-checker (car x) (car y) #f #f) (list-checker (cdr x) (cdr y) #f #f)))]
	[(equal? (car x) (car y))
	  (if (or(and (equal? (is-lam (car x)) 1) (equal?(is-lam (car y)) 1))(equal? flag #t))
	      (cons (car x) (list-checker (cdr x) (cdr y) #t #f))
	      (cons (car x) (list-checker (cdr x) (cdr y) #f #f )))]
	[(not (equal? (car x) (car y)))

	 (if (and(> (length x) 1) (> (length y) 1))
	     (begin

	     (cond[(and(equal? (is-lam (car x)) 1) (equal? (is-lam (car y)) 1))
		    (lam-type x y)]
		  [else

		   (if (equal? flag #t)
		       (begin

			 (if (and(not (pair? x)) (not (pair? y)))
			     (begin
			     (if(equal? (length x) 3)
				(cons(param-checker x y)(list-checker (cdr x)(cdr y) #t #t))
			     (cons(param-checker x y)(list-checker (cdr x) (cdr y) #f #f))))

			     (if(equal? flag2 #t)
				(cons (param-checker  x y)(list-checker (cdr x) (cdr y) #t #f))
				(cons (list 'if '% (car x) (car y)) (list-checker (cdr x) (cdr y) #f #f)))))

		       (if (equal? flag2 #t)
			   (cons (param-checker  x y)(list-checker (cdr x) (cdr y) #t #f))
		       (cons (list 'if '% (car x) (car y)) (list-checker (cdr x) (cdr y) #f #f))))]))
	     (cond [(equal? flag #t)

		    (cons(param-checker x y)(list-checker (cdr x) (cdr y) #f #f))]
		   [(equal? flag2 #t)
                                (cons(param-checker x y)(list-checker (cdr x) (cdr y) #t #f))]

		   [else

		    (cons (list 'if '% (car x) (car y)) (list-checker (cdr x) (cdr y) #f #f))]))]
	
	[else
	 (list 'if '% x y)]
	))

(define (expr-compare x y )
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) 
         (if x '% '(not %))]
        ; if one of them is not list - which means that not function
        [(or (not (list? x)) 
             (not (list? y)))
         (list 'if '% x y)]

       
	
         ; and below here it is your work to figure out how to judge every case
         ; but! please pay attention: this is NOT the only structure you could have for solving this homework
         ;     we actually encourage you to come up with OTHER designs if you can!
         ; please only follow this starting hint when you REALLY don't know where to start!
	[(and (list? x) (list? y)) (list-checker x y #f #f)]
	
	))
; compare and see if the (expr-compare x y) result is the same with x when % = #t
;                                                 and the same with y when % = #f
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

; WARNING: IT MUST BE A SINGLE TEST CASE
; You need to cover all grammars including:
;     constant literals, variables, procedure calls, quote, lambda, if
(define test-expr-x
  ;`(cons 12 ((lambda (a) (+ a 1)) 2)))
  '(list 12 #f quote((lambda (a) (+ a b)) 3)) 
)
(define test-expr-y
  ;`(cons 11 ((lambda (a) (+ a 2)) 3)))
 '(list 20 #t quote((lambda (b) (+ a c)) 3))
)


; the following line can be tested from interpreter
;     (eval test-expr-x)
;     (test-expr-compare test-expr-x test-expr-y))
;           test-expr-compare should return #t after you finish its implementation
;     (expr-compare 'a '(cons a b)) 
;     (expr-compare '(cons a b) '(cons a b))
;     (lambda? 'λ)
