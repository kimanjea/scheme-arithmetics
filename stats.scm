(define (port->lines port)
  (let loop ((line (read-line port)) (lines '()))
    (if (eof-object? line)
        (reverse lines)
        (loop (read-line port) (cons line lines))
    )
  )
)

(define (maketoken l)
  (let loop ((t '())
             (l l))
    (if (pair? l)
        (let ((c (car l)))
          (if (char=? c #\,) ;
              (cons (list->string (reverse t)) (loop '() (cdr l)))
              (loop (cons c t) (cdr l))
          )
        )
        (if (not (null? t))
            (list (list->string (reverse t)))
            '()
        )
    )
  )
)

(define (split-string s)
  (maketoken (string->list s))
)

(define (read-csv file headers column)
  (with-input-from-file file
    (lambda ()
      (let ((headers-list (if headers (read-line (current-input-port)) '())))
        (let loop ((lines (port->lines (current-input-port)))  
                   (result '())
                   )
          (if (null? lines)
              (reverse result)
              (let ((line (car lines))
                    (rest (cdr lines)))
                (let ((col-value (string->number (list-ref (split-string line) column))))
                  (loop rest (cons col-value result))))))))))
; adds all values
(define (summation values) 
    (if (null? values)
        0
    (apply + values)
)
)
; my very own mean function 

(define (mean values)
    (/(summation values) (length values)))

;defining covariance and variance helps me not do redundant code
;covariance function
(define (covariance xvalues yvalues) ; centralized the covariance function
  (define m (summation xvalues))
  (define a (summation yvalues))
  (define n (length xvalues))
  (/ (apply + (map (lambda (x y) (* (- x (/ m n)) (- y (/ a n)))) xvalues yvalues)) (- n 1)))

;variance function
(define (variance values) ; centralized the variance function 
  (define m (summation values))
  (define n (length values))
  (/ (apply + (map (lambda (x) (* (- x (/ m n)) (- x (/ m n)))) values)) (- n 1)))


(define (regressiona xvalues yvalues)
    (define cov (covariance xvalues yvalues))
     (define varx (variance xvalues))
    ;calculate slope after calling covariance
    (define slope (/ cov varx)) 
     slope ; return the slope
)

(define (regressionb xvalues yvalues)
    (define cov (covariance xvalues yvalues))
     (define varx (variance xvalues))
    ;calculate slope after calling covariance
     (define slope (/ cov varx)) 
    (define m (summation xvalues))
    (define a (summation yvalues))
    (define n (length xvalues))
    (define bvalue (- (/ a n) (* slope (/ m n)))) ; bvalue: mean(y) - slope * mean(x)
     bvalue ; return the b value
)

;correlation function
(define (correlation xvalues yvalues)
    (define cov (covariance xvalues yvalues))
  (define varx (variance xvalues))
  (define varx2 (variance yvalues))
  (define square_root (sqrt (* varx varx2)))
  (define correlation_val (/ cov square_root))
  (abs correlation_val) ; we try to get the absolute value
)


(define (stddev values)
(define z (summation values)) ; Calculate the sum of values
(define n (length values))
(define variance (/ (apply + (map (lambda (x) (* (- x (/ z n)) (- x (/ z n)))) values)) n)) ; Calculate variance
(define standard_deviation (sqrt variance)) ; Calculate standard deviation
standard_deviation
)

;extra credit on go
;(define (apply-regression sat gpa test)
 ; (define linear (map (lambda (x) (roundoff (+ (* x (regressiona sat gpa))(regressionb sat gpa)) 4)) test))
  ;linear)