

(define (port->lines port) ;start of read csv
  (let loop ((line (read-line port)) (lines '()))
    (if (eof-object? line)
        (reverse lines)
        (loop (read-line port) (cons line lines))
    )
  )
)

(define (maketoken l) ; tookenizes read 
  (let loop ((t '())
             (l l))
    (if (pair? l)
        (let ((c (car l)))
          (if (char=? c #\,) 
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

(define (split-string s) ; splits
  (maketoken (string->list s))
)

;read csv file 
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
;rounding
(define (roundoff a b)
  (let* ((scale (expt 10 b))
    (scaled (* a scale))
    (rounded (round scaled)))
    (/ rounded scale ))
)


; adds all values
(define (summation values) 
    (if (null? values)
        0
    (apply + values)
)
)


; my mean function 
(define (mean values)
    roundoff(/(summation values) (length values))
)


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
    (define slope (roundoff(/ cov varx) 4) )
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
    (define bvalue (roundoff(- (/ a n) (* slope (/ m n))) 4)) ; bvalue: mean(y) - slope * mean(x)
     bvalue ; return the b value
)


;correlation function
(define (correlation xvalues yvalues)
    (define cov (covariance xvalues yvalues))
  (define varx (variance xvalues))
  (define varx2 (variance yvalues))
  (define square_root (sqrt (* varx varx2)))
  (define correlation_val (roundoff(/ cov square_root) 4))
  (abs correlation_val) ; we try to get the absolute value
)



(define (stddev values)
(define z (summation values)) ; Calculate the sum of values
(define n (length values))
(define variance (/ (apply + (map (lambda (x) (* (- x (/ z n)) (- x (/ z n)))) values)) n)) ; Calculate variance
(define standard_deviation (roundoff(sqrt variance) 4)) ; Calculate standard deviation
standard_deviation

)


;extra credit on go
(define (apply-regression sat gpa test)
(define linear (map (lambda (x) (roundoff (+ (* x (regressiona sat gpa))(regressionb sat gpa)) 4)) test))
  linear)


; Make sure to concatenate your stats.scm to this file
; Instructions:
; cat stats.scm test.scm > mytest.scm
; Then run:
; mit-scheme --load mytest.scm
; 11 test total, 1 extra credit

(newline)
(newline)

(define (double-equals a b)
  (< (abs (- a b)) 0.0001)
)

; DATA
(define sat (list 1714 1664 1760 1685 1693 1764 1764 1792 1850 1735))
(define gpa (list 2.4 2.52 2.54 2.74 2.83 3 3 3.01 3.01 3.02))


; Test Average
(display(double-equals 17421/10 (mean sat))) ;should be #t
(newline)

(display(double-equals 2.8070 (mean gpa))) ;should be #t
(newline)

; Test Standard Deviation
(display(double-equals 52.9367 (stddev sat))) ;should be #t
(newline)
(display(double-equals 0.2295 (stddev gpa))) ;should be #t
(newline)

; Test Correlation
(display(double-equals 0.5823 (correlation sat gpa))) ;should be #t
(newline)

; Test Regression
(display(double-equals 0.0025 (regressiona sat gpa)))    ;should be #t
(newline)

(display(double-equals -1.5909 (regressionb sat gpa)))    ;should be #t
(newline)

; Test Read CSV File
(define x (read-csv "data1.csv" #f 0)) ; 
(display(equal? (list 10 11 12 14 9) x)) ;should be #t
(newline)

(define y (read-csv "data1.csv" #f 1))  ; 
(display(equal? (list 21 15 23 27 18) y)) ;should be #t
(newline)

; Test Read CSV File with Header
(define x (read-csv "data2.csv" #t 0))
(display(equal? (list 10 11 12 14 9) x)) ;should be #t
(newline)

(define y (read-csv "data2.csv" #t 1))
(display(equal? (list 21 15 23 27 18) y)) ;should be #t
(newline)

; Test Extra Credit

(define test (list 1200 1650 1705 1800))
(define result (apply-regression sat gpa test)) ;should be #t

(display(equal? (list 1.4091	2.5341	2.6716	2.9091) result)) ; should be #t
(newline)
(newline)




