#lang racket
(require "complex.rkt")
(require "fft.rkt")
(require racket/date)


; Given two numbers x and y encoded as lists and that can be decoded back into
; a long, this function checks whether the FFT algorithm computed correctly their
; product, for example to multiply 1234 and 5678 then provide the lists
; '(1 2 3 4) and '(5 6 7 8)
; this function returns a boolean, #t or #f indicating whether the behavior is correct
(define (correct-ans x y)
        (eq? (* (poly-eval (reverse x) 10)
                (poly-eval (reverse y) 10))
             (poly-eval
                 (map (lambda (z)
                              (inexact->exact (round (re z))))
                      (multiply x y)) 10)))


; Given a number n, this function returns a list of digits, each between 0 and 9
; (inclusive). This list has length n
(define (random-bigint n)
        (build-list n (lambda (_) (random 10))))


; This function runs (10 by default) tests to check if the algorithm is executing
; correctly. It returns an array of length the number of tests whose members are
; booleans and indicating whether each test passed or not
(define (tester [n 10] [start (current-milliseconds)])
        (map (lambda (_)
                     (let ([s (expt 2 (+ 1 (random 3)))])
                          (let ([x (random-bigint s)]
                                [y (random-bigint s)])
                               (begin
                                 (display "[TEST]: ")
                                 (display (- (current-milliseconds) start))
                                 (display "ms\t")
                                 (display (poly-eval (reverse x) 10))
                                 (display " * ")
                                 (display (poly-eval (reverse y) 10))
                                 (newline)
                                 (correct-ans x y)))))
             (range 0 n)))



(foldl (lambda (a b) (and a b)) #t (tester))
