#lang racket
(require drracket/tool
scribble/eval
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
;(define whitetextdelta (send bolddelta set-delta-foreground "white"))
;(define blacktextdelta (send bolddelta set-delta-foreground "black"))
;(define failstyledelta (send whitetextdelta set-delta-background "red"))

#;
(define pass-styledelta 
  (local [(define blacktextdelta (send bolddelta set-delta-foreground "black"))]
    (send blacktextdelta set-delta-background "green")))
#;
(define fail-styledelta 
  (local [(define whitetextdelta (send bolddelta set-delta-foreground "white"))]
    (send whitetextdelta set-delta-background "red")))

(define secret-key "(test 1 1)")
(define secret-key2 "(test 1 2)")

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
        
        (define/augment (on-insert start len)
          (begin-edit-sequence))
        (define/augment (after-insert start len)
          (check-range (max 0 (- start (string-length secret-key)))
                       (+ start len))
          (end-edit-sequence))
        
        (define/augment (on-delete start len)
          (begin-edit-sequence))
        (define/augment (after-delete start len)
          (check-range (max 0 (- start (string-length secret-key)))
                       start)
          (end-edit-sequence))
        
        (define/augment (on-change-style start len)
          (begin-edit-sequence))
        (define/augment (after-change-style start len)
          (check-range (max 0 (- start (string-length secret-key)))
                       start)
          (end-edit-sequence))
        
        (define/private (check-range start stop)
          (let/ec k
            (for ((x (in-range start stop)))
              (define after-x
                (get-text x (+ x (string-length secret-key))))
                (when (or (string=? after-x secret-key) (string=? after-x secret-key2)) 
		     (change-style (if (test-passes? after-x) 
                       ; This has to be inline - if it's defined at the top, styledelta will be set statically.
                       (send (send bolddelta set-delta-foreground "black") set-delta-background "green")
                       (send (send bolddelta set-delta-foreground "white") set-delta-background "red"))
                           x (+ x (string-length secret-key)))))))
        
        (super-new)))
    

    ; string -> boolean
    ; Takes a string containing a test expression, i.e. "(test exp1 exp2)"
    ; and returns #t if exp1 and exp2 evaluate to the same value.
    ; TODO: handle exceptions
    (define (test-passes? str) 
      (define test-exp (read (open-input-string str)))
     (match test-exp
        [(list 'test expected actual) (equal? (eval expected ns) (eval actual ns))]
        [else #f]))
    
;; string -> boolean
;; Returns true if str is a test-expression with matching closed parentheses.
;; Does not check for syntax of internal expressions.
;; TODO: Abstract to {}, [].
(define (done-test? str) 
  (and (> (string-length str) 5)
       (string=? (substring str 0 5) "(test")
       (parens-closed? str)))

;; string -> boolean
;; Returns true if str starts with an open paren and 
;; has a matching number of parens.
(define (parens-closed? str)
  (and (> (string-length str) 0)
       (string=? (substring str 0 1) ")")
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
