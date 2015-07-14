#lang racket
(require "complex.rkt")


; Just like the filter function but this one filters based on the index instead
; of filtering based on the value
(define (index-filter select? a)
        (map (lambda (e) (car (cdr e)))
             (filter (lambda (e) (select? (car e)))
                     ; A fancy way to do a zip
                     (map list (range 0 (length a)) a))))


; Computes a polynomual of cooeficient `cooef` at a point `x`
; the polynomial P(x) = \sum cooef_i x^i
(define (poly-eval cooef x)
        (if (= (length cooef) 1) (car cooef)
            (+ (* x (poly-eval (index-filter odd? cooef) (* x x)))
               (poly-eval (index-filter even? cooef) (* x x)))))


; Given a sequence `a`, this function computes its DFT using the FFT algorithm
; Precondition: the length of `a` must be a power of two
(define (fft a)
        (if (= (length a) 1)
            ; If the list is one element then simply return this element as
            ; a complex number if it is not already one
            (if (pair? (car a))
                a
                (list (complex (car a) 0)))
            ; Otherwise find the FFT of the sublists and combine them as needed
            (let ([C (fft (index-filter even? a))]
                  [W (nth-roots-unity (length a))]
                  [D (fft (index-filter odd? a))])
                 (map (lambda (c d w)
                              (+complex c (*complex w d)))
                      (append C C) (append D D) W))))



; Given a sequence `a`, this function computes its inverse DFT using the inverse
; FFT algorithm
; Precondition: the length of `a` must be a power of two
(define (fft^-1 a)
        (if (= (length a) 1)
            ; If the list is one element then simply return this element as
            ; a complex number if it is not already one
            (if (pair? (car a))
                a
                (list (complex (car a) 0)))
            ; Otherwise find the inverse FFT of the sublists and combine them as needed
            (let ([C (fft^-1 (index-filter even? a))]
                  [W (nth-roots-unity (length a))]
                  [D (fft^-1 (index-filter odd? a))])
                 (map (lambda (c d w)
                              (+complex c (*complex (conjugate w) d)))
                      (append C C) (append D D) W))))



; This function renormalizes the answers of the inverse fft, this is needed
; since we have that F^-1(F(a)) = |a| * a
; Precondition: there exist some b such that |b| = |a| and a = fft^-1(b)
(define (normalize-fft a)
        (map (lambda (z)
                     (*complex (complex (/ 1 (length a)) 0) z)) a))


; Given a list of numbers that could end with zeros, this function returns a
; copy of this array without the ending zeroes
(define (strip-of-zeros a)
        (cond [(= (length a) 0) a]
              [(= (round (norm-complex (last a))) 0) (strip-of-zeros (take a (sub1 (length a))))]
              [#t a]))

; given two large numbers X and Y then this function computes X.Y
; x = (X_0, X_1, ... , X_n)
; y = (Y_0, Y_1, ... , Y_m)
; the answer is also encoded in such fashion
; Precondition: |x| = |y| = 2^n for some n, also for every i, 0 <= x_i < 10 a number
(define (multiply x y)
        (reverse (strip-of-zeros (normalize-fft
            (fft^-1 (map (lambda (x y) (*complex x y))
                (fft (append x (make-list (length x) 0)))
                (fft (append y (make-list (length y) 0)))))))))


(define ans (map (lambda (z) (inexact->exact (round (re z))))
                 (multiply '(4 3 1 1) '(6 8 7 4))))


(poly-eval ans 10)
