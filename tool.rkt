#lang racket
(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         racket/match
         mrlib/switchable-button
  	     test-engine/racket-tests)
(provide tool@)

; Hook into current namespace; for eval.
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define bolddelta (make-object style-delta% 'change-weight 'bold))
;(send bolddelta set-weight-on 'bold)
;(send bolddelta set-weight-off 'normal)
; Doesn't work for switching styles: sets it statically
;(define whitetextdelta (send bolddelta set-delta-foreground "white"))
;(define blacktextdelta (send bolddelta set-delta-foreground "black"))
;(define failstyledelta (send whitetextdelta set-delta-background "red"))

; Doesn't work either
#;
(define pass-styledelta
  (local [(define blacktextdelta (send bolddelta set-delta-foreground "black"))]
    (send blacktextdelta set-delta-background "green")))
#;
(define fail-styledelta
  (local [(define whitetextdelta (send bolddelta set-delta-foreground "white"))]
    (send whitetextdelta set-delta-background "red")))

(define (boolean->string b) (if b "#t" "#f"))

;(define secret-key "(test (+ 2 1) (+ 1 2))")
;(define secret-key2 "(test (+ 2 1) (+ 1 1))")
;(define test-start "(test")
(define test-length 50)
;; TODO: Test keyword support is currently not abstract enough.
;(define test-keywords (list "test" "check-expect"))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define easter-egg-mixin
      (mixin ((class->interface text%)) ()

        (inherit begin-edit-sequence
                 end-edit-sequence
                 insert
                 change-style
                 get-text
                 set-styles-sticky)

        ;; TODO: Get start to be the starting index of the new line.
        ;; TODO: Stop the highlighting at the end of a test line.
        (define/augment (on-insert start len)
          (begin-edit-sequence))
        (define/augment (after-insert start len)
          (check-range (max 0 (- start test-length))
                       (+ start len))
          (end-edit-sequence))

        (define/augment (on-delete start len)
          (begin-edit-sequence))
        (define/augment (after-delete start len)
          (check-range (max 0 (- start test-length))
                       start)
          (end-edit-sequence))

        (define/augment (on-change-style start len)
          (begin-edit-sequence))
        (define/augment (after-change-style start len)
          (check-range (max 0 (- start test-length))
                       start)
          (end-edit-sequence))

        (define/private (check-range start stop)
          (let/ec k
            (for ((x (in-range start stop)))
              (define after-x
                (get-text x (+ x test-length)))
                (when (done-test? after-x)
                  (local [(define test-rc (test-passes? after-x))]
                    (change-style
                      ; This has to be inline - if it's defined at the top, styledelta will be set statically.
                         (cond  [(void? test-rc) ; syntax error
                                 (send
                                  (send bolddelta ; (make-object style-delta% 'change-weight 'base)
                                        set-delta-foreground "black")
                                   set-delta-background "yellow")]
                                [test-rc ; pass
                                  (send
                                    (send bolddelta set-delta-foreground "black")
                                  set-delta-background "green")]
                               [(not test-rc) ; fail
                                (send
                                  (send bolddelta set-delta-foreground "white")
                                  set-delta-background "red")])
                               x (+ x (string-length (newline-begins-substring after-x)))))
                  ;; fyi: this has no effect
                  #;
                  (send
                    (send (make-object style-delta% 'change-weight 'base) set-delta-foreground "black")
                    set-delta-background "white")
                  ))))

        (super-new)))

    ; string -> (or/c boolean void)
    ; Takes a string containing a test expression, i.e. "(test exp1 exp2)"
    ; Returns #t if exp1 and exp2 evaluate to the same value, #f if not, 
    ; and (void) if either of exp1 or exp2 have bad syntax.
    ; TODO: handle exceptions
    (define (test-passes? str)
      (define test-exp (read (open-input-string str)))
      (match test-exp
        [(list (or 'check-expect 'test) expected actual)
         (local [(define (try-eval expr)
                   (with-handlers [(exn:fail? (lambda (e) (void)))] (eval expr ns)))
                 (define chk (try-eval expected))
                 (define ept (try-eval actual))]
                (if (or (void? chk) (void? ept))
                  (void)
                  (equal? chk ept)))]
         [else #f]))

;; string -> boolean
;; Returns true if str is a test-expression with matching closed parentheses.
;; Does not check for syntax of internal expressions.
;; TODO: Abstract to {}, [].
(define (done-test? str)
  (and (or
         (and (> (string-length str) 5)
              (string=? (substring str 0 5) "(test"))
         (and (> (string-length str) 13)
              (string=? (substring str 0 13) "(check-expect")))
       (parens-closed? str)))

;; string -> boolean
;; Returns true if str starts with an open paren and
;; has a matching number of parens.
(define (parens-closed? str)
  (and (> (string-length str) 0)
       (string=? (substring str 0 1) "(")
       (parens-closed-helper str 0 0)))

;; string Int Int -> boolean
;; helper for parens-closed?
(define (parens-closed-helper str l r)
  (cond  [(= 0 (string-length str)) (= l r)]
        [(string=? (substring str 0 1) "(")
         (parens-closed-helper (substring str 1) (+ l 1) r)]
        [(string=? (substring str 0 1) ")")
         (parens-closed-helper (substring str 1) l (+ r 1))]
        [else (parens-closed-helper (substring str 1) l r)]))

;; list list -> boolean
;; Returns true if l1 is equal to l2
(define (list-equals? l1 l2)
  (and (= (length l1) (length l2))
       (or (and (empty? l1) (empty? l2))
           (and (= (first l1) (first l2)) (list-equals? (rest l1) (rest l2))))))

;; Returns the index of a character in the given list
;; e.g. (indexof #\newline (string->list "adsf
;        (test"))) -> 4
(define (indexof elem lst)
  (indexof-h elem lst char=? 0))

;; erm... Haskell notation since not sure of the Racket equivalent
;; a -> [a] -> (a -> Bool) -> Int -> Int
;; Helper for indexof ^
;; Returns -1 if element can't be found
(define (indexof-h elem lst pred acc)
  (if (empty? lst)
    -1
    (if (pred (first lst) elem)
      acc
      (indexof-h elem (rest lst) pred (add1 acc)))))

;; string -> string
;; Returns the string from the first newline
;; e.g. (newline-begins-substring "asdf
;        (test") -> "(test"
(define (newline-begins-substring str)
  (define txt-list (string->list str))
  (define mbr (member #\newline txt-list))
  (if (not mbr)
    str
    (list->string (drop mbr 1))))

;; string -> string
;; Returns line between two newlines
;; e.g. (newline-between "adsf
;        (test
;        qwerty") -> "(test"
(define (newline-between str)
  (local [(define newline-substr-1 (newline-begins-substring str))
          (define newline-idx-2 (indexof #\newline (string->list newline-substr-1)))]
         (if (< newline-idx-2 0)
           newline-substr-1
           (substring newline-substr-1 0 newline-idx-2))))

;; list list -> boolean
;; Returns true if l1 is a subsequence of l2
;; e.g. (subseq? '(1 2 3) '(5 1 2 3 4)) -> #t
;       (subseq? '() '(1 2 3 4)) -> #t
;       (subseq? '(1 2 3) '(1 2 4 3)) -> #f
(define (subseq? l1 l2)
  (cond [(empty? l1) #t] ; not necessary
        [(= (first l1) (first l2)) (list-equals? l1 (take l2 (length l1)))]
        [else (and (>= (length l2) (length l1)) (subseq? l1 (rest l2)))]))


;; PLUGIN FUNCTIONS ==============
    (define reverse-button-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
        (inherit register-toolbar-button)

        (let ((btn
               (new switchable-button%
                    (label "Reverse Definitions")
                    (callback (λ (button)
                                (reverse-content
                                 (get-definitions-text))))
                    (parent (get-button-panel))
                    (bitmap reverse-content-bitmap))))
          (register-toolbar-button btn #:number 11)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))

    (define reverse-content-bitmap
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "blue" 'solid)
        (send bdc draw-ellipse 2 2 8 8)
        (send bdc set-brush "red" 'solid)
        (send bdc draw-ellipse 6 6 8 8)
        (send bdc set-bitmap #f)
        bmp))

    (define (reverse-content text)
      (for ((x (in-range 1 (send text last-position))))
        (send text split-snip x))
      (define snips
        (let loop ((snip (send text find-first-snip)))
          (if snip
              (cons snip (loop (send snip next)))
              '())))
      (define released-snips
        (for/list ((snip (in-list snips))
                   #:when (send snip release-from-owner))
          snip))
      (for ((x (in-list released-snips)))
        (send text insert x 0 0)))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-definitions-text easter-egg-mixin)
    (drracket:get/extend:extend-unit-frame reverse-button-mixin)))
