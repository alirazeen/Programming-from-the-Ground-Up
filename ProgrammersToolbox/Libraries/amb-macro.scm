(declare (uses syntax-case))

;AMB definition

;Riastradh's definitions
(define-syntax amb
  (syntax-rules ()
    ((amb) (fail))
    ((amb ?x) ?x)
    ((amb ?x ...)
     ((call-with-current-continuation
         (lambda (k-success)
          (call-with-current-continuation
            (lambda (k-failure)
              (set! fail k-failure)
              (k-success (lambda () ?x))))
          ...
          fail))))))
(define (fail) (error "uninitialized AMB"))
(define (reset-amb!) (set! fail (lambda () (error "AMB search tree exhausted"))))
(define (amb-asset condition) (if (not condition) (fail)))



