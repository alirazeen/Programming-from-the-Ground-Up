;(use-syntax (ice-9 syncase))

(define __let_here_globals '())

(define-syntax let-here
	(lambda (x)
		(syntax-case x ()
			(
				(let-here () expr ...)
				(syntax (begin expr ...))
			)
			(
				(let-here ( (var val) (var2 val2) ...) expr ...)
				(with-syntax ( (var-translated (datum->syntax-object (syntax let-here) (list 'quote (generate-temporaries '(only-need-one))))) ) 
					(syntax
						(let
							(
								(var #f)
							)
							(dynamic-wind
								(lambda ()
									(let
										(
											(tmp (assoc var-translated __let_here_globals))
										)
										(if (pair? tmp)
											(set! var (cdr tmp))
											(set! var val)
										)
									)
								)
								(lambda ()
									(let-here ( (var2 val2) ...) expr ...)
								)
								(lambda ()
									(let
										(
											(tmp (assoc var-translated __let_here_globals))
										)
										(if (pair? tmp)
											(set-cdr! tmp var)
											(set! __let_here_globals (cons (cons var-translated var) __let_here_globals))
										)
									)
								)
							)
						)
					)
				)
			)
		)
	)
)	

