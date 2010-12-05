(load "let_here.scm")

(define make-flipflop (lambda (begin-test end-test . inclusives)
	(let*
		(
			(state 'not-active)
			(inclusive-begin (if (eq? inclusives '()) #t (car inclusives)))
			(inclusive-end (if (eq? inclusives '()) #t (if (eq? (cdr inclusives) '()) #t (car (cdr inclusives)))))
		)
		(lambda (x)
			(if (eq? state 'not-active)
				(if (begin-test x)
					(begin 
						(set! state 'active)
						inclusive-begin
					)
					#f
				)
				(if (end-test x)
					(begin
						(set! state 'not-active)
						inclusive-end
					)
					#t
				)
			)
		)
	)
))

(define make-static-flipflop (lambda (begin-val end-val . inclusives)
	(apply make-flipflop `(,(lambda (x) (equal? begin-val x)) ,(lambda (x) (equal? end-val x)) ,@inclusives))
))

(define __current #f)

;(let-here ( (x value) ) code)

(define-syntax flipflop (syntax-rules ()
	(
		(flipflop begin-test end-test)
		(let-here ( (the-flipflop (make-flipflop begin-test end-test)) )
			(the-flipflop __current)
		)
	)
	(
		(flipflop begin-test end-test expr)
		(let-here ( (the-flipflop (make-flipflop begin-test end-test)) )
			(the-flipflop expr)
		)
	)
))

(define-syntax flipflop-static (syntax-rules ()
	(
		(flipflop-static begin-val end-val)
		(let-here ( (the-flipflop (make-static-flipflop begin-val end-val)) )
			(the-flipflop __current)
		)
	)
	(
		(flipflop-static begin-val end-val expr)
		(let-here ( (the-flipflop (make-static-flipflop begin-val end-val)) )
			(the-flipflop expr)
		)
	)
))


