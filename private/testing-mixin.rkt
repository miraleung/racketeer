#lang racket

(require drracket/tool
         drracket/tool-lib
         framework
         mrlib/switchable-button
         racket/class
         racket/gui/base
         racket/gui
         racket/unit
         racket/match
         racket/sandbox
         test-engine/racket-tests
         unstable/function
         wxme)

;; CONSTANTS
;; Handler types.
(define HANDLER_AFTER_INSERT    "after-insert")
(define HANDLER_AFTER_DELETE    "after-delete")
(define HANDLER_AFTER_LOAD_FILE "after-load-file")

(define SBAR_ALL_PASS "All tests pass.")

(define STATE_PASS 'pass)
(define STATE_FAIL 'fail)
(define STATE_ERROR 'error)

;; Test expression constants.
;; (or/c syntax? #f)
(define first-error-test-status #f)
(define default-statusbar-message "")

;; STRUCTS
;; Test/error struct
;; state is one of STATE_PASS, STATE_FAIL, STATE_ERROR
;; linenum is the editor line number as specified in text%.
;; start-posn is the starting position as specified in text%.
;; Examples:
;; Passing test: (make-test-struct STATE_PASS 10 (void) (void))
;; Failing test: (make-test-struct STATE_FAIL 9  <actual value> [<expected value>])
;; Error test:   (make-test-struct STATE_ERROR 2 <error message> (void))
(define-struct test-struct (state linenum start-posn end-posn error-or-value1 value2))
;; y-locations: the y-coordinate range of a line.
(define-struct y-locations (top bottom))

(define running-thread #f)

(define (passed-test? ts) (symbol=? STATE_PASS (test-struct-state ts)))
(define (failed-test? ts) (symbol=? STATE_FAIL (test-struct-state ts)))
(define (error-test? ts)  (symbol=? STATE_ERROR (test-struct-state ts)))

;; STYLE DEFS
(define pass-delta (send (make-object style-delta% 'change-weight 'bold)  set-delta-background "green"))
(define fail-delta (send (make-object style-delta% 'change-weight 'bold)  set-delta-background "red"))
(define error-delta (send (make-object style-delta% 'change-weight 'bold) set-delta-background "yellow"))
(define normal-delta (send (make-object style-delta% 'change-weight 'normal) set-delta-background "white"))

;(define bolddelta (make-object style-delta% 'change-weight 'bold))
#|
(define pass-delta (send (send (make-object style-delta% 'change-weight 'bold)
                               set-delta-foreground "black") set-delta-background "green"))
(define fail-delta (send (send (make-object style-delta% 'change-weight 'bold)
                               set-delta-foreground "white") set-delta-background "red"))
(define error-delta (send (send (make-object style-delta% 'change-weight 'bold)
                                    set-delta-foreground "black") set-delta-background "yellow"))
(define normal-delta (send (send (make-object style-delta% 'change-normal 'base)
                                 set-delta-foreground "black") set-delta-background "white"))
;(send bolddelta set-weight-on 'bold)
;(send bolddelta set-weight-off 'normal)
|#
(define test-table (make-hash))


(define NEW-FILE-HIGHLIGHT-DELAY 2.5) ; seconds

;; TODO: Test keyword support is currently not abstract enough.
;; TODO: Unhighlight tests when highlighting is off
;; get line nums

(provide racketeer-testing-mixin)
#;
(provide/contract
 [racketeer<%> interface?]
 [racketeer-testing-mixin
  (-> (class/c () ()))
  (class/c
   [foo (->m void?)])])

(define racketeer<%>
  (interface ()))

(define watching-mouse-events #f)


(define racketeer-testing-mixin
  ;  (mixin ((class->interface text%) editor<%>) ()
  (lambda (cls)
    (class* cls ()
      
      (inherit begin-edit-sequence
               change-style
               end-edit-sequence
               find-newline
               get-tab
               get-text
               insert
               last-position
               line-end-position
               line-length
               line-location
               line-start-position
               position-line
               position-location
               save-port
               set-styles-sticky)
      
      (define highlight-tests? (preferences:get 'drracket:racketeer-highlight-tests?))
      
      (define/public-final (highlight?) highlight-tests?)
      (define/public-final (toggle-highlight!)
        (preferences:set 'drracket:racketeer-highlight-tests? (not highlight-tests?))
        (set! highlight-tests? (not highlight-tests?))
        (if (highlight?)
            (check-range 0 (- (last-position) 1))
            ;; TODO: re-highlight with syntax check using global syntax object
            (thread (thunk (un-highlight-all-tests)))))
      
      
      ;; MOUSE EVENT HANDLER
      (define/override (on-event mouse-evt)
        (super on-event mouse-evt)
        (thread (thunk (mouseover-test-handler (send mouse-evt get-y)))))
      
      (define/private (mouseover-test-handler y-coord)
        (define cursor-expr (find-test-y-range y-coord))
        (cond [(or (not cursor-expr) (passed-test? cursor-expr))
               (set-statusbar-label (get-default-statusbar-message))]
              [(or (failed-test? cursor-expr) (error-test? cursor-expr))
               (set-statusbar-label (get-test-message cursor-expr))]))
      
      
      ;; exact-integer -> (or/c passed-test failed-test error-test #f)
      ;; Gets the test at this line.
      (define/private (find-test-y-range y-coord)
        (local [(define lo-y-locns (hash-keys test-table))
                ;; (listof y-locations) -> (or/c passed-test failed-test error-test #f)
                (define (find-y-range y-locns)
                  (cond [(empty? y-locns) #f]
                        [(<= (y-locations-top (first y-locns))
                             y-coord
                             (y-locations-bottom (first y-locns)))
                         (hash-ref test-table (first y-locns))]
                        [else (find-y-range (rest y-locns))]))]
          (if (zero? (hash-count test-table))
              #f
              (find-y-range lo-y-locns))))
      
      ;; EVENT HANDLERS.
      (define/augment (on-insert start len)
        (begin-edit-sequence))
      (define/augment (after-insert start len)
        (end-edit-sequence)
        (check-range start (+ start len)))
      
      
      (define/augment (on-delete start len)
        (begin-edit-sequence))
      (define/augment (after-delete start len)
        (end-edit-sequence)
        (check-range start (+ start len)))
      
      
      (define/augment (on-load-file loaded? format)
        (begin-edit-sequence))
      (define/augment (after-load-file loaded?)
        (end-edit-sequence)
        (first-highlight-refresh))
      
      
      (define (first-highlight-refresh)
        (sleep NEW-FILE-HIGHLIGHT-DELAY)
        (when (highlight?)
          (check-range 0 (- (last-position) 1))))
      
      (define/private (set-statusbar-label message)
        (define frame (send (get-tab) get-frame))
        (send frame set-rktr-status-message message))
      
      (define/private (check-range start stop)
        (when (thread? running-thread)
          (kill-thread running-thread))
        (set! running-thread (thread (thunk (check-range-helper start stop)))))
      
      (define/private (check-range-helper start stop)
        (when (not (highlight?))
          (send (send (get-tab) get-frame) set-rktr-status-message ""))
        
        (when (highlight?)
          (let/ec k
            ; Ignore events that trigger for the entire file.
            (when (not (and (= 0 start) (= (last-position) stop)))
              (with-handlers [(exn:fail? (lambda (e) #f))]
                (define src-out-port (open-output-bytes))
                (save-port src-out-port)
                (define test-in-port (open-input-bytes (get-output-bytes src-out-port)))
                (define eval-in-port (open-input-bytes (get-output-bytes src-out-port)))
                ; Ignore events for WXME-formatted files
                ; TODO: Handle WXME files.
                (when (and (not (is-wxme-stream? test-in-port))
                           (not (is-wxme-stream? eval-in-port)))
                  ; If anything is written to the error port while creating the evaluator,
                  ; write it to a string port
                  ; TODO: Write the contents of the string port to the status bar.
                  (define test-error-output (open-output-string))
                  (define test-output (open-output-string))
                  (define evaluator (parameterize [(sandbox-eval-limits '(10 20))]
                                      (make-module-evaluator (remove-tests eval-in-port))))
                  ; The value of evaluator is #f if the source code has syntax errors.
                  (when evaluator
                    (define tests (get-tests test-in-port))
                    
                    ;; Clear statusbar.
                    (set! first-error-test-status #f)
                    (set! default-statusbar-message "")
                    (set-statusbar-label default-statusbar-message)
                    
                    ;; Clear test hash table.
                    (set! test-table (make-hash))
                    
                    (for ([test-syn tests])
                      (define test-start (max 0 (- (syntax-position test-syn) 1)))
                      (define test-end (+ (syntax-position test-syn) (syntax-span test-syn)))
                      (define linenum (position-line test-start))
                      (define test-rc
                        (test-passes? test-syn evaluator linenum test-start test-end))
                      (define y-locns (make-y-locations (line-location linenum)
                                                        (line-location linenum #f)))
                      
                      ;; New hash table entry.
                      (hash-set! test-table y-locns test-rc)
                      
                      ;; Set the first syntax error object.
                      ;; TODO: Optimization point?
                      ;; TODO: Change to when-expression
                      (if (not (passed-test? test-rc))
                          (set! first-error-test-status test-rc)
                          (void))
                      
                      ) ;; for
                    (highlight-all-tests)
                    (thread (thunk (set-statusbar-label (get-default-statusbar-message))))
                    ) ;; when
                  ) ;; when
                ) ;; with-handlers
              ) ;; when
            ) ;; let
          ) ;; when
        ) ;; define
      
      (define/private (highlight-all-tests)
        (queue-callback highlight-all-tests-helper))
      
      (define (highlight-all-tests-helper)
        (for ([y-locn-key (hash-keys test-table)])
          (define test-rc (hash-ref test-table y-locn-key))
          (define test-start (test-struct-start-posn test-rc))
          (define test-end (test-struct-end-posn test-rc))
          (define linenum (test-struct-linenum test-rc))
          (change-style
           (cond [(error-test? test-rc)  error-delta]
                 [(passed-test? test-rc) pass-delta]
                 [(failed-test? test-rc) fail-delta])
           test-start test-end)))
      
      (define/private (un-highlight-all-tests)
        (set! test-table (make-hash))
        (change-style normal-delta 0 (last-position)))
      
      (super-new))))




(define (stringify prefix message)
  (string-append
   prefix
   (let [(o (open-output-string))]
     (parameterize [(current-output-port o)]
       (display (format ": ~a" message)))
     (get-output-string o))))

;; Get the line number of the test struct.
(define (linenum-prefix ts)
  (local [(define (format-linenum-string linenum)
            (string-append "line " (number->string (+ 1 linenum)) ": "))]
    (cond [(or (not ts)
               (passed-test? ts)) ""]
          [(failed-test? ts)
           (format-linenum-string (test-struct-linenum ts))]
          [(error-test? ts)
           (format-linenum-string (test-struct-linenum ts))])))

;; Format test messages for failed/error tests.
;; Use of stringify is due to unknown types of test subexpressions.
(define (get-test-message ts)
  (cond [(zero? (hash-count test-table)) ""]
        [(not ts) SBAR_ALL_PASS]
        [(passed-test? ts) default-statusbar-message]
        [(failed-test? ts)
         (local [(define prefix (linenum-prefix ts))
                 (define value1 (test-struct-error-or-value1 ts))
                 (define part1 (stringify "actual value" value1))
                 (define value2 (test-struct-value2 ts))
                 (define part2
                   (if (void? value2)
                       ""
                       (stringify "; expected" value2)))]
           (string-append prefix part1 part2))]
        [(error-test? ts)
         (string-append (linenum-prefix ts)
                        (stringify "exception"
                                   (test-struct-error-or-value1 ts)))]))


(define (get-default-statusbar-message)
  (set! default-statusbar-message (get-test-message first-error-test-status))
  default-statusbar-message)


(define test-statements (list 'test 'test/exn 'test/pred 'check-error 'check-expect))

(define (get-tests src-port)
  (syntax-expander (get-syntax-list src-port)))

(define (get-syntax-list src-port)
  (local [(define (get-syntax-list-helper src-port syntax-list)
            (local [(define syn (parameterize [(read-accept-reader  #t)
                                               (read-accept-lang    #t)]
                                  (read-syntax 'program src-port)))]
              (if (syntax? syn)
                  (get-syntax-list-helper src-port (cons syn syntax-list))
                  syntax-list)))]
    (get-syntax-list-helper src-port empty)))

(define (syntax-expander to-expand)
  (local [(define (syntax-expander-helper to-expand syntax-list)
            (if (empty? to-expand)
                syntax-list
                (local [(define expanded (syntax->list (first to-expand)))]
                  (if expanded
                      (if (test-syntax? (first to-expand))
                          (syntax-expander-helper (rest to-expand) (cons (first to-expand) syntax-list))
                          (syntax-expander-helper (append (filter syntax? expanded) (rest to-expand)) syntax-list))
                      (syntax-expander-helper (rest to-expand) syntax-list)))))]
    (syntax-expander-helper to-expand empty)))

(define (test-syntax? syn)
  (test-expression? (syntax->datum syn)))

(define (test-expression? expr)
  (and (list? expr)
       (not (empty? expr))
       (member (first expr) test-statements)))

(define (remove-tests src-port)
  (test-remover (parameterize [(read-accept-reader  #t)
                               (read-accept-lang    #t)]
                  (read-syntax 'program src-port))))

(define (test-remover syn)
  (local [(define (keep-expr? expr)
            (not (test-expression? expr)))
          (define (test-remover-helper expr)
            (if (list? expr)
                (map test-remover-helper (filter keep-expr? expr))
                expr))]
    (if (syntax? syn)
        (datum->syntax #f (test-remover-helper (syntax->datum syn)))
        syn)))


; syntax -> (or/c boolean void)
; Takes a syntax object containing a test expression, i.e. "(test exp1 exp2)"
; Returns #t if exp1 and exp2 evaluate to the same value, #f if not,
; and (void) if either of exp1 or exp2 have bad syntax.
; ;; TODO: Ensure that exn message contexts don't get lost on removing the subexceptions
(define (test-passes? test-syn evaluator linenum start-position end-position)
  (define defn-evaluator
    (with-handlers [(exn:fail? (lambda (e) (error-test (exn-message e))))]
      evaluator))
  
  (define test-exp
    (with-handlers [(exn:fail? (lambda (e)  (error-test (exn-message e))))]
      (syntax->datum test-syn)))
  
  (define (error-test msg)
    (make-test-struct STATE_ERROR linenum start-position end-position msg (void)))
  (define passd-test
    (make-test-struct STATE_PASS linenum start-position end-position (void) (void)))
  (define (faild-test exprval testval)
    (make-test-struct STATE_FAIL linenum start-position end-position exprval testval))
  
  (define (try-eval expr)
    (with-handlers [ ;TODO: Need a good syntax matcher
                    (exn:fail? (lambda (e) (error-test (exn-message e))))]
      (with-limits 0.2 0.1 (defn-evaluator expr))))
  
  (define (test-eq actual expected)
    (local [ ;; Make the evaluator recognize strings.
            (define (process-string raw-str)
              (string-append "\"" raw-str "\""))
            (define actual-prime
              (if (string? actual)
                  (process-string actual)
                  actual))
            (define expected-prime
              (if (string? expected)
                  (process-string expected)
                  expected))
            (define actual-val    (try-eval actual-prime))
            (define expected-val  (try-eval expected-prime))]
      (cond [(and (test-struct? actual-val)
                  (error-test? actual-val)) actual-val]
            [(and (test-struct? expected-val)
                  (error-test? expected-val)) expected-val]
            ;; Needs to be equal? instead of eq? for object comparison.
            [else (if (equal? actual-val expected-val)
                      passd-test
                      (faild-test actual-val expected-val))])))
  (local [(define (test-passes?-helper)
            (match test-exp
              [(list (or 'check-expect 'test) actual expected)
               (test-eq actual expected)]
              [(list 'test/exn actual str)
               (if (not (string? str))
                   (error-test "second expression must be a string")
                   (local [(define actual-val (try-eval actual))]
                     (if (and (test-struct? actual-val) (error-test? actual-val)) ; <- this should be other-error, once syntax-matching is fixed.
                         passd-test
                         (faild-test actual-val "an exception"))))]
              [(list 'check-error actual)
               ; TODO: If try-eval returns an oom-error, it should be an error struct, not a failed-struct.
               (if (error-test? (try-eval actual))
                   passd-test
                   (faild-test "no error raised" (void)))]
              [(list 'test/pred expr pred)
               (local [(define pred-app (try-eval pred))]
                 (if (not (procedure? pred-app))
                     (error-test "second expression must be a procedure")
                     (if (pred-app expr)
                         passd-test
                         (faild-test (pred-app expr) (void)))))]
              [else (error-test "unrecognized test variant")]))] ;; TODO: Change this to other-error ??
    
    (test-passes?-helper))
  ) ;; define (test-passes?)


