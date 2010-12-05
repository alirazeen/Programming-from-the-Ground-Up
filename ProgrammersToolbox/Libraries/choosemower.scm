(declare (uses amb extras))

;;Global Variables
(define questions 
	'(
		(yardsize . ("How big is your yard?"
			(1acre . "1 Acre")
			(2acre . "2 Acres")
			(morethan2acre . "More than 2 Acres")
		))
		(mowfrequency . ("How often do you mow?"
			(everyweek . "Every Week")
			(everyday . "Every Day")
		))
	)
)

(define mowers
	'(
		(z80 . ( (mowfrequency . (everyweek)) (yardsize . (1acre))))
		(z20 . ( (mowfrequency . (everyweek everyday)) (yardsize . (2acre))))
		(A98 . ( (mowfrequency . (everyday)) (yardsize . (2acre))))
	)
)

(define answers '())

(define read-answer
	(lambda (q-struct)
		(let* ( (question-sym (car q-struct))
				(question-text (cadr q-struct))
				(q-answers (cdr (cdr q-struct)))
				(cur-question 0))
			(display question-text)
			(newline)
			(for-each (lambda (x)
					(display cur-question)
					(display ": ")
					(display (cdr x))
					(newline)
					(set! cur-question (+ cur-question 1)))
				q-answers)
			(set! answers (cons (cons question-sym (car (list-ref q-answers (string->number (read-line))))) answers)))))
			
(for-each read-answer questions)

;(display answers)
;(newline)

;(define answers
;	'( (mowfrequency . everyday) (yardsize . 2acre))
;)

;;Assertion Tester
(define mowerlist (map (lambda (x) (car x) ) mowers))
(define valid-mowers
	(collect-amb-possibilities
		(lambda ()
		(let 
			( (mymower (apply amb mowerlist)))

			(let ( (mower-answers (cdr (assq mymower mowers))))

				(for-each 
					(lambda (x)
						(let* 
							( 
								(question (car x))
								(answer (cdr x))
								(mower-question-answers (assq question mower-answers)))
							(amb-assert (member answer mower-question-answers))))
					answers)
				mymower)))))

(display valid-mowers)
(newline)
