(define fact
    (lambda (accum start)
        (if (equal? start 0) accum (fact (* accum start) (- start 1) ))
    )
)

(define bad_fact
    (lambda (start)
        (if (equal? start 0) 1 (* start (bad_fact (- start 1))))
    )
)

(define factorial
    (lambda (x)
        (fact 1 x)))