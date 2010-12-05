(use-syntax (ice-9 syncase))


(load "dynamic_vars.scm")

(define __is-in-exception? #f)

(define-syntax try
	(syntax-rules (except finally)
		(
			(try proc (except exception-var exception-handler-body))
			(let 
				(
					(exception-handler (lambda (exception-var)
						exception-handler-body
					))
					(result (call-with-current-continuation
						(lambda (cont)
							(dynamic-set! __current-exception-handler cont
								(cons 'no-exception proc)
							)
						)
					))
				)
				(if (pair? result)
					(if (eq? (car result) 'no-exception)
						(cdr result)
						(begin
							(set! __is-in-exception? #f)
							(exception-handler result)
						)
					)
				)
			)
		)
		(
			(try proc (finally clean-up-body))
			(let*
				(
					(exit-susp (delay clean-up-body))
					(exit-proc (lambda () (force exit-susp)))
				)
				(dynamic-wind
					(lambda () #f)
					(lambda ()
						(begin
							proc
							(exit-proc)
						)
					)
					(lambda ()
						(if __is-in-exception?
							(exit-proc)
							#f
						)
					)
				)
			)
		)
	)
)

(define-syntax raise
	(syntax-rules ()
		(
			(raise exception exc-value ...)
			(begin
				(set! __is-in-exception? #t)
				((dynamic-get __current-exception-handler) (list (quote exception) exc-value ...))
			)
		)
	)
)
