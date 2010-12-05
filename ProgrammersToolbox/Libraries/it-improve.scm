(define iterative-improve (lambda (check-guess improve-guess)
	(lambda (guess)
		(iterative-improve-recursive check-guess improve-guess guess)
	)
))
			

(define iterative-improve-recursive (lambda (check-guess improve-guess current-guess)
	(if (check-guess current-guess)
		current-guess
		(iterative-improve-recursive 
			check-guess 
			improve-guess 
			(improve-guess current-guess) 
		)
	)
))

(define square-root (lambda (number precision)
	(let
		(
			(check-guess (lambda (guess)
				(if (> (abs (- (* guess guess) number)) precision)
					#f
					#t
				)
			))
			(improve-guess (lambda (guess)
				(/ (+ guess (/ number guess)) 2)
			))
		)
		((iterative-improve check-guess improve-guess) 5)
	)
))
		


;;;Exercise 2.16
(define interval-create (lambda (value tolerance . used-intervals)
	(vector 
		value 
		(+ 1.0 tolerance) 
		(if (null? used-intervals) '() (car used-intervals))
	)
))

(define interval-create-bounds (lambda (low high)
	(let*
		(
			(avg (/ (+ low hight) 2))
			(tolerance-value (- avg low))
			(tolerance (/ tolerance-value avg))
		)
		(interval-create avg tolerance)
	)
))

(define interval-centerpoint (lambda (x)
	(vector-ref x 0)
))

(define interval-tolerance (lambda (x)
	(- (vector-ref x 1) 1.0)
))

(define interval-tolerance-value (lambda (x)
	(* (vector-ref x 0) (vector-ref x 1))
))

(define interval-width (lambda (x)
	(- (interval-upper-bound x) (interval-lower-bound x))
))

(define interval-lower-bound (lambda (x)
	(- (interval-centerpoint x) (interval-tolerance-value x))
))

(define interval-upper-bound (lambda (x)
	(+ (interval-centerpoint x) (interval-tolerance-value x))
))

(define interval-accumulated-intervals (lambda (x)
	(vector-ref x 2)
))

(define interval-uses-interval? (lambda (x int)
	(memq int (interval-accumulated-intervals x))
))

(define interval-combine-accumulations append)

(define interval-add (lambda (x y)
	(interval-create 
		(+ (interval-centerpoint x) (interval-centerpoint y))
		(if
			(or (interval-uses-interval x y) (interval-uses-interval y x))
			
		(+ ( x) (interval-upper-bound y))
		(interval-combine-accumulations (interval-accumulated-intervals x) (interval-accumulated-intervals y) (list x y))
	)
))

(define interval-mul (lambda (x y)
	(let
		(
			(p1 (* (interval-lower-bound x) (interval-lower-bound y)))
			(p2 (* (interval-lower-bound x) (interval-upper-bound y)))
			(p3 (* (interval-upper-bound x) (interval-lower-bound y)))
			(p4 (* (interval-upper-bound x) (interval-upper-bound y)))
		)
		(interval-create
			(min p1 p2 p3 p4)
			(max p1 p2 p3 p4)
			(interval-combine-accumulations (interval-accumulated-intervals x) (interval-accumulated-intervals y) (list x y))
		)
	)
))
			

(define interval-div (lambda (x y)
	(interval-mul 
		x
		(interval-invert y)
	)
))

(define interval-invert (lambda (x)
	(interval-create
		(/ 1.0 (interval-upper-bound x))
		(/ 1.0 (interval-lower-bound x))
		(interval-combine-accumulations (interval-accumulated-intervals x) (list x))
	)
))
			
			
			
