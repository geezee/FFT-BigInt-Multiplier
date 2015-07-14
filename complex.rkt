#lang racket

; Construct a complex number from 2 given expressions x and y
(define (complex x y)
                (cons x y))

; Get the real part of a complex number
(define (re z)
           (car z))

; Get the imaginary part of a complex number
(define (im z)
           (cdr z))

; Compute the complex conjugate of a complex number
(define (conjugate z)
        (complex (re z) (* -1 (im z))))

; Get the norm of a complex number
(define (norm-complex z)
        (sqrt (+ (* (re z) (re z)) (* (im z) (im z)))))

; Get the angle of a complex number
(define (angle-complex z)
        (atan (/ (im z) (re z))))

; Add two comlpex numbers
(define (+complex z1 z2)
        (complex (+ (re z1) (re z2)) (+ (im z1) (im z2))))

; Multiply two complex numbers
(define (*complex z1 z2)
        (complex (- (* (re z1) (re z2)) (* (im z1) (im z2)))
                 (+ (* (re z1) (im z2)) (* (im z1) (re z2)))))

; Complex to r<theta (polar)
(define (to-polar z)
        (if (= (re z) 0)
            (cons (norm-complex z) (* (/ (im z) (norm-complex z)) (/ pi 2)))
            (cons (norm-complex z) (angle-complex z))))

; Polar to complex
(define (to-complex z)
        (complex (* (re z) (cos (im z)))
                 (* (re z) (sin (im z)))))

; Display a complex number
(define (display-complex z)
        (display (string-append
                   (format "~s" (/ (round (* 1000 (re z))) 1000))
                   " + ("
                   (format "~s" (/ (round (* 1000 (im z))) 1000))
                   ")i\n")))

; Display a polar complex number
(define (display-polar z)
        (display (string-append
                   (format "~s" (/ (round (* 1000 (re z))) 1000))
                   "<"
                   (format "~s" (/ (round (* 1000 (im z))) 1000))
                   "\n")))

; Find the nth roots of unity, this function returns them as a list
; the orientation of generation is counter clockwise, i.e. in the positive
; direction
(define (nth-roots-unity n)
        (map (lambda (k)
                     (to-complex (complex 1 (/ (* -2 pi k) n))))
             (range 0 n)))



; export all functions
(provide (all-defined-out))
