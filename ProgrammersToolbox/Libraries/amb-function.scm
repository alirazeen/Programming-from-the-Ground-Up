;GUILE crap
;(use-syntax (ice-9 syncase))
;(define call/cc call-with-current-continuation)

(declare (unit amb))

;AMB definition
(define amb-fail '*)

(define initialize-amb-fail
	(lambda x
		(set! amb-fail
			(if (null? x)
				(lambda () (error "amb tree exhausted!"))
				(car x)))))

(initialize-amb-fail)

(define amb
	(lambda alternatives
		(letrec 
			( (amb-internal 
					(lambda (sk alts)
						(if (null? alts)
							(prev-amb-fail)
							(begin
								(call/cc
									(lambda (fk)
										(set! amb-fail
											(lambda ()
												(set! amb-fail prev-amb-fail)
												(fk 'fail)))
										(sk (car alts))))
								(amb-internal sk (cdr alts))))))
				(prev-amb-fail amb-fail))
			(call/cc
				(lambda (sk)
					(amb-internal sk alternatives)
					(prev-amb-fail))))))
(define amb-assert-failure
	(lambda () (amb)))


(define amb-assert
	(lambda (pred)
		(if (not pred) (amb-assert-failure))))

(define collect-amb-possibilities
	(lambda (x)
		(let ((the-values '()))
			(call/cc
				(lambda (k)
					(initialize-amb-fail (lambda () (k the-values)))
					(set! the-values (cons (x) the-values ))
					(amb-assert-failure))))))

(define-syntax collect-amb-possibilities-m
	(syntax-rules ()
		(
			(collect-amb-possibilities-m expr ...)
			(collect-amb-possibilities (lambda () (begin expr ...)))
		)
	)
)
