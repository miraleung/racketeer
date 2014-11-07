#lang racket

;; Currently supports test, test/exn, test/pred, check-expect, check-error
;; Expected behaviour (not all fully implemented)
;; - Test should change colour status when you edit it anywhere.
;; - Each test should be distinctly highlighted with its respective test status.
;; - Should be able to open/load files from disk.

;; NOTES
;; test-exn does not behave the same way as Racket's. Ours passes whenever any
;; kind of exception is thrown, whereas Racket's passes only for explicit user-thrown
;; exceptions. e.g. (test/exn (/ 1 0) "") in Racket would fail, whereas it passes for ours.


;; Expected: <colour-code>.

;; Passed.

(test 1 1)
(test (+ 1 2) (+ 3 0))
(test (map add1 (list 1 2 3)) (list 2 3 4))
(test (local [(define (foo x) (* 100 x))] (foo 2)) (- 1000 800))
(test (((lambda (x) (lambda (y) (+ x y))) 300) 11) 311)

(test/pred 1 number?)
(test/pred 123 (lambda (x) (= 123 x)))

(test/exn (error "") "")
(test/exn (raise-user-error "a") "")
(test/exn (local []) "") ; <- with better syntax error detection, this shouldn't be passing 

;; anything that works for test should work here too
(check-expect 1 1)
(check-expect (+ 1 2) (+ 3 0))
(check-expect (map add1 (list 1 2 3)) (list 2 3 4))

(check-error (error ""))
(check-error (raise-user-error "a"))
(check-error (asdf)) ; <- this should actually be a syntax error; s-exp should fix this

;; Syntax Errors.

(test 311)
(test ((lambda (x) (+ y 3)) 1) 1)
(test (local []) 3)

(test/pred 311 number)

(test/exn (error ""))
(test/exn (error "") 311)
(test/exn (error "") (lambda (x) x))

;; ...and everything working for test will work for check-expect
(check-expect 311)

(check-error (error "") 0)

;; Out-of-memory.
(test (local [(define (foo x) (foo x))] (foo 3)) 11) ; wait 3 seconds

(check-expect (local [(define (foo x) (foo x))] (foo 3)) 11)

;; Failed.

(test 1 2)
(test ((lambda (x) (+ 3 x)) 1) 1)
;; Add more failure test cases here

(test/pred 1 boolean?)

(test/exn (error "")) ; <- actually a syntax error, needs fixing
(test/exn (local [(define (foo x) (foo x))] (foo 3)) "") ; <- not sure where this should actually go

(check-expect (raise-error "a") "a")

(check-error (raise-user-error "a") "")
(check-error 1)

;; Exceptions. NOT IMPLEMENTED YET, JUST SYNTAX ERRORS FOR NOW.

(test (error "") 1)

(check-expect (error "a") 1)
