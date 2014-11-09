#lang racket
(require drracket/tool
         drracket/tool-lib
         racket/class
         racket/gui/base
         racket/gui
         racket/unit
         racket/match
         mrlib/switchable-button
  	 test-engine/racket-tests
         racket/sandbox
         framework
         unstable/function )
(provide tool@)

; Hook into current namespace; for eval.
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

;; Test/error structs
(define-struct passed-test (void))
(define-struct failed-test (void))
(define-struct syntax-error (void))
(define-struct out-of-memory-error (void))
(define-struct other-error (void)) ; meant for exceptions

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
(define pass-test-msg "; yay, it passed")
(define fail-test-msg "; yay, it failed")
(define syn-error-msg "; < syntax error")
(define oom-error-msg "; out-of-memory ")
(define oth-error-msg "; error occurred ")
(define test-msg-len (string-length pass-test-msg))

;(define secret-key "(test (+ 2 1) (+ 1 2))")
;(define secret-key2 "(test (+ 2 1) (+ 1 1))")
;(define test-start "(test")
(define test-length 70)
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
                 find-newline
                 last-position
                 line-start-position
                 line-end-position
                 get-text
                 set-styles-sticky)

        ;; TODO: Get start to be the starting index of the new line.
        ;; TODO: Stop the highlighting at the end of a test line.
        (define/augment (on-insert start len)
          (begin-edit-sequence))
        (define/augment (after-insert start len)
;(message-box "a" (string-append (number->string start) " " (number->string len) " " (number->string (or (find-newline 'backward) -1)) " " (number->string (or (find-newline) -1))))
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
             (define alltext (get-text 0 (last-position)))
; (message-box "b" (string-append (number->string start) " " (number->string x) " " (number->string x-end) alltext))
;            (for ((x (in-range start stop)))
            ; (for ((x (list "(test" "(test/exn" "(test/pred" "(check-expect" "(check-error")))
              (define-values (x x-end) (start-match-end-idx alltext stop))
              (when (and (> x 0) (not (= x x-end)))
              (define after-x
                (get-text x x-end))
                  (when (done-test? after-x)
;(message-box "a" (string-append (number->string x) " "(number->string x-end) " " after-x))
                  (local [(define idx (+ 1 (strindex alltext #\newline))) ; eliminate the first line : lang line
                          (define test-rc (test-passes? after-x (substring alltext idx)))
                          (define test-msg (get-test-msg test-rc))]
                     (change-style
                      ; This has to be inline - if it's defined at the top, styledelta will be set statically.
                         (cond  [(syntax-error? test-rc) ; syntax error
                                 (send
                                  (send bolddelta ; (make-object style-delta% 'change-weight 'base)
                                        set-delta-foreground "black")
                                   set-delta-background "yellow")]
                                [(other-error? test-rc) ; exception
                                 (send
                                  (send bolddelta
                                        set-delta-foreground "white")
                                   set-delta-background "purple")]
                                [(out-of-memory-error? test-rc) ; exception
                                 (send
                                  (send bolddelta
                                        set-delta-foreground "white")
                                   set-delta-background "blue")]
                                [(passed-test? test-rc) ; pass
                                  (send
                                    (send bolddelta set-delta-foreground "black")
                                  set-delta-background "green")]
                               [(failed-test? test-rc) ; fail
                                (send
                                  (send bolddelta set-delta-foreground "white")
                                  set-delta-background "red")])
                                ; x stop)
                                (or (find-newline 'backward x 'eof) x) (or (find-newline 'forward x 'eof) stop))
                  ;; fyi: this has no effect
                  #;
                  (send
                    (send (make-object style-delta% 'change-weight 'base) set-delta-foreground "black")
                    set-delta-background "white")
                  #;
                  (if (string=? test-msg oom-error-msg) ;; this works, but pops up like 10 times
                      (message-box "Racketeer" "Out of memory!")
                      (void))
                  #;
                  (define insert-x (get-text (+ x test-length))) ; TODO: fix infinite text-adding
                    #;
                    (unless (string=? insert-x test-msg)
                       (insert test-msg (get-forward-sexp stop) (+ 22 (string-length test-msg))))
                  ) ;; // local
                  ) ;; // when
                  ) ;; // when
             ;     ) ;; // for
                  ))

        (super-new)))


(define (run-all str) 
  (if (eval (compile (read (open-input-string str))) ns) (make-syntax-error (void)) (make-passed-test (void))))

;; TODO: Clean
(define (get-defines str) (get-def-h str (string-length str) (string-length str)  ""))
(define (get-def-h str m n sa) (if (<= n 0) sa (if (parens-closed? (substring str m n)) (if (done-test? (substring str m n)) (get-def-h (substring str 0 m) m m (string-append (substring str n) sa)) (get-def-h (substring str 0 m) m m (string-append (substring str m) sa))) (get-def-h str (- m 1) n sa))))


;; Precond: n comes after "(test"
(define (start-match-end-idx str n) 
  (if (or 
        (< (- n 5) 0) 
        (> (+ n 1) (string-length str))) 
      (values n n) 
      (start-match-end-idx-h str (- n 5) (+ n 1))))
(define (start-match-end-idx-h str l r) 
  (cond [(or (< l 0) (> r (string-length str))) (values (max 0 l) (min (string-length str) r))]         
;         [(done-test? (substring str l r)) (values l r)]        
        [(and (foldl (lambda (x y) (or y (string=? x (substring str l (min (string-length str) (+ l (string-length x))))))) #f (list "(test" "(test/exn")) (parens-closed? (substring str l r))) (values l r)]        
        [(foldl (lambda (x y) (or y (string=? x (substring str l (min (string-length str) (+ l (string-length x))))))) #f (list "(test" "(test/exn"))  (start-match-end-idx-h str l (+ r 1))]
        [else (start-match-end-idx-h str (- l 1) (+ r 1))]))

;; n is cursor
;;(define (start-end-idx str n) (if (or (< (- n 1) 0) (> (+ n 1) (string-length str))) (values n n) (start-end-idx-h str (- n 1) (+ n 1) 0 0)))
;;(define (start-end-idx-h str l r nl nr) (cond [(or (< l 0) (> r (string-length str))) (values (max 0 l) (min (string-length str) r))] [(parens-closed? (substring str l r)) (values l r)] [(and (>= nl nr) (string=? (substring str l (+ l 1)) "(")) (start-end-idx-h str l (+ r 1) (+ nl 1) nr)] [(and (>= nr nl) (string=? (substring str (- r 1) r) ")")) (start-end-idx-h str (- l 1) r nl (+ nr 1))] [else (start-end-idx-h str (- l 1) (+ r 1) nl nr)]))




(define (strindex str char) (strindex-h (string->list str) char 0))
(define (strindex-h loc c acc) (if (empty? loc) -1 (if (char=? c (first loc)) acc (strindex-h (rest loc) c (+ acc 1)))))

;; search fwds from n in str to find next closing bracket
;; returns -1 on fail
(define (get-next-closing-h str n acc)  (if (> acc (string-length str)) (string-length str) (if (not (parens-closed? (substring str n acc))) (get-next-closing-h str n (+ acc 1)) acc)))
(define (get-next-closing str n) (if (> n (string-length str)) 0 (get-next-closing-h str n n)))


;; searching bwds from the end of str, returns the first substring with a matching closing paren
;; if none, returns empty string.
(define (get-first-closing str) (get-first-closing-h str (string-length str)))
(define (get-first-closing-h str acc) (if (< acc 0) (string-length str) (if (not (parens-closed? (substring str acc))) (get-first-closing-h str (- acc 1)) acc)))
(define (get-first-closing-substr str) (substring str (get-first-closing str)))

; string -> (or/c boolean void)
; Takes a string containing a test expression, i.e. "(test exp1 exp2)"
; Returns #t if exp1 and exp2 evaluate to the same value, #f if not,
; and (void) if either of exp1 or exp2 have bad syntax.
; TODO: handle exceptions, pass in lang
(define (test-passes? str no-test-expr-str)

  (define defn-evaluator 
    (with-handlers [ (exn:fail:syntax? (lambda (e) syn-err))
                 (exn:fail:out-of-memory? (lambda (e) oom-err))
                 ; workaround for exn:fail:out-of-memory? not terminating
                 (exn:fail:resource? (lambda (e) oom-err))
                 (exn:fail? (lambda (e) other-err))]
                 (parameterize [(sandbox-eval-limits '(1 20))]
                   (make-evaluator 'racket no-test-expr-str))))
  (define test-exp 
    (with-handlers [ (exn:fail:syntax? (lambda (e) syn-err))
                 (exn:fail:out-of-memory? (lambda (e) oom-err))
                 ; workaround for exn:fail:out-of-memory? not terminating
                 (exn:fail:resource? (lambda (e) oom-err))
                 (exn:fail? (lambda (e) other-err))]
      (read (open-input-string str))))

  (define syn-err (make-syntax-error (void)))
  (define oom-err (make-out-of-memory-error (void)))
  (define other-err (make-other-error (void)))
  (define passd-test (make-passed-test (void)))
  (define faild-test (make-failed-test (void)))
  (define (try-eval expr)
                 (with-handlers [ ;TODO: Need a good syntax matcher
                                  ;(exn:fail:syntax? (lambda (e) syn-err))
                                 ; workaround for exn:fail:out-of-memory? not terminating
                                 (exn:fail:resource? (lambda (e) oom-err))
                                 (exn:fail? (lambda (e) other-err))]
                                (with-limits 0.2 0.1 (defn-evaluator expr))))
  (define (test-rc lo-expr)
    (local [(define vals (map (lambda (x) (try-eval x)) lo-expr))
            (define (foldor lst) (foldr (lambda (x y) (or x y)) #f lst))
            (define (foldand lst) (foldr (lambda (x y) (and x y)) #t lst))]
      (cond [(foldor (map syntax-error? vals)) syn-err] ; syntax error
            [(foldor (map out-of-memory-error? vals)) oom-err]    ; some other error
            [(foldor (map other-error? vals)) other-err]
            [else
              (if (foldand (map (lambda (x) (equal? x (first vals))) vals))
                passd-test
                faild-test)])))
  (match test-exp
    [(list (or 'check-expect 'test) actual expected)
     (test-rc (list actual expected))]
    [(list 'test/exn actual str)
     (if (not (string? str))
       syn-err
       (if (and (string? str) (syntax-error? (try-eval actual))) ; <- this should be other-error, once syntax-matching is fixed.
         passd-test
         faild-test))]
    [(list 'check-error actual)
     (if (syntax-error? (try-eval actual))
       passd-test
       faild-test)]
    [(list 'test/pred expr pred)
     (local [(define pred-app (try-eval pred))]
            (if (not (procedure? pred-app))
              syn-err
              (if (pred-app expr)
                passd-test
                faild-test)))]
    [else syn-err])) ;; TODO: Change this to other-error ??

(define (get-test-msg test-rc)
  (cond [(syntax-error? test-rc) syn-error-msg]
        [(out-of-memory-error? test-rc) oom-error-msg]
        [(other-error? test-rc) oth-error-msg]
        [(passed-test? test-rc) pass-test-msg]
        [(failed-test? test-rc) fail-test-msg]))

;; string -> boolean
;; Returns true if str is a test-expression with matching closed parentheses.
;; Does not check for syntax of internal expressions.
;; TODO: Abstract to {}, [].
(define (done-test? str)
  (and (or
         (and (> (string-length str) 5)
              (string=? (substring str 0 5) "(test"))
         (and (> (string-length str) 12) ; <- shortest -s c-error
              (string=? (substring str 0 7) "(check-")))
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

