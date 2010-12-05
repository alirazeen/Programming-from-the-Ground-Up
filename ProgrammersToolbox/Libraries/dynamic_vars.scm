
(define __dynamic-vars '())

(define-syntax dynamic-set!
	(syntax-rules ()
		(
			(dynamic-set! var value body)
			(let
				(
					(__old-dynamic-vars __dynamic-vars)
				)
				(begin
					(dynamic-wind 
						(lambda () 
							(set! __dynamic-vars
								(cons (cons (quote var) value) __dynamic-vars)
							)
						)
						(lambda ()
							body
						)
						(lambda ()
							(set! __dynamic-vars __old-dynamic-vars)
						)
					)
				)
			)
		)
	)
)

(define-syntax dynamic-get
	(syntax-rules ()
		(
			(dynamic-get var)
			(__dynamic-get (quote var))
		)
	)
)

(define __dynamic-get (lambda (var . dynamic-exception)
	(let
		(
			(result (assq var __dynamic-vars))
		)
		(if (pair? result)
			(cdr result)
			(if (pair? dynamic-exception)
				((car dynamic-exception) var)
				'()
			)
		)
	)
))


