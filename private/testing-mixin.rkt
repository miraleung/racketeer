#lang racket

(require drracket/tool
         drracket/tool-lib
         (for-syntax racket/base)
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
(define-struct passed-test (void))
(define-struct failed-test (linenum first-value second-value))
(define-struct error-test (linenum error-msg))

;; state is one of STATE_PASS, STATE_FAIL, STATE_ERROR
;; result is void if it passes, expr-value if failed, and error message otherwise
(define-struct y-locations (top bottom))
;; (make-test-context <syntax object> string-index expr-string-length line-number test-status)
;; status is one of passed-test, failed-test, or error-test
(define-struct test-context (stx posn linenum status))


;; STYLE DEFS
;(define bolddelta (make-object style-delta% 'change-weight 'bold))
(define pass-delta (send (send (make-object style-delta% 'change-weight 'bold)
                               set-delta-foreground "black") set-delta-background "green"))
(define fail-delta (send (send (make-object style-delta% 'change-weight 'bold)
                               set-delta-foreground "white") set-delta-background "red"))
(define error-delta (send (send (make-object style-delta% 'change-weight 'bold)
                                    set-delta-foreground "black") set-delta-background "yellow"))
(define normal-delta (send (send (make-object style-delta% 'change-weight 'normal)
                                 set-delta-foreground "black") set-delta-background "white"))
;(send bolddelta set-weight-on 'bold)
;(send bolddelta set-weight-off 'normal)

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
                         (set! highlight-tests? (not highlight-tests?)))


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
      ;; (thread (thunk (check-range start (+ start len) HANDLER_AFTER_INSERT)))
      (end-edit-sequence)
      (check-range start (+ start len)))


    (define/augment (on-delete start len)
      (begin-edit-sequence))
    (define/augment (after-delete start len)
      ;; (thread (thunk (check-range start (+ start len) HANDLER_AFTER_DELETE)))
      (end-edit-sequence)
      (check-range start (+ start len)))


    (define/augment (on-load-file loaded? format)
      (begin-edit-sequence))
    (define/augment (after-load-file loaded?)
;      (thread (thunk (check-range 0 (last-position)))))
      ;(future (thunk first-highlight-refresh)))
      (end-edit-sequence)
      (first-highlight-refresh))

      ;(send (send (send (send (get-tab) get-frame) get-cainvas) get-editor) end-edit-sequence))

    (define (first-highlight-refresh)
      (sleep NEW-FILE-HIGHLIGHT-DELAY)
      (if (highlight?)
        (thread (thunk (check-range 0 (last-position))))
        (void)))
;      (queue-callback first-highlight-refresh-helper))
#|
    (define (first-highlight-refresh-helper)
      (if (highlight?)
        (check-range 0 (last-position))
        (void)))
|#

(define/private (set-statusbar-label message)
  (define frame (send (get-tab) get-frame))
  (send frame set-rktr-status-message message))

;; TODO: Find a refactoring to avoid checking the inner if conds every time.
    (define/private (check-range start stop)
      ;; TODO: If highlighting is set to off, don't highlight.
      (if (and (not highlight-tests?) (> stop 20))
        (send (send (get-tab) get-frame) set-rktr-status-message "")
        (void))

      (if (not highlight-tests?)
        (void)
        (let/ec k
          (define src-out-port (open-output-bytes))
          (save-port src-out-port)
          ; Ignore events that trigger for the entire file.
          (when (not (and (= 0 start) (= (last-position) stop)))
            (with-handlers [(exn:fail? (lambda (e) #f))]
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
                (define evaluator (parameterize [(sandbox-eval-limits '(1 20))]
                                    (make-module-evaluator eval-in-port)))
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
                    (define test-rc (test-passes? test-syn evaluator linenum))
                    (define y-locns (make-y-locations (line-location linenum)
                                                      (line-location linenum #f)))
                    (define (highlight-tests)
                    ;  (message-box "asdf" (format "~a ~a ~a ~a ::: ~a:~a || locn: ~a ~a" test-rc test-syn test-start test-end (position-line test-start) (position-location test-start) (line-location (position-line test-start)) (line-location (position-line test-start) #f)))
                      (change-style
                        (cond [(error-test? test-rc)        error-delta]
                              [(passed-test? test-rc)         pass-delta]
                              [(failed-test? test-rc)         fail-delta])
                        test-start test-end))

                    ;; Set the first syntax error object.
                    ;; OPT
                    (if (not (passed-test? test-rc))
                      (set! first-error-test-status test-rc)
                      (void))
                    ;; New hash table entry.
                    (hash-set! test-table y-locns test-rc)
                    (highlight-tests)
                    ;(queue-callback highlight-tests)
                    ) ;; for
                    (set-statusbar-label (get-default-statusbar-message))
                  ) ;; when
                ) ;; when
              ) ;; with-handlers
            ) ;; when
          ) ;; let
        ) ;; if
      ) ;; define
    (super-new))))

(define (stringify prefix message)
  (string-append
    prefix
    (let [(o (open-output-string))]
      (parameterize [(current-output-port o)]
        (display (format ": ~a" message)))
      (get-output-string o))))

;; Get the line number of the test struct.
(define (linenum-prefix test-struct)
  (local [(define (format-linenum-string linenum)
            (string-append "L" (number->string linenum) ": "))]
         (cond [(or (not test-struct)
                    (passed-test? test-struct)) ""]
               [(failed-test? test-struct)
                (format-linenum-string (failed-test-linenum test-struct))]
               [(error-test? test-struct)
                (format-linenum-string (error-test-linenum test-struct))])))

;; Format test messages for failed/error tests.
;; Use of stringify is due to unknown types of test subexpressions.
(define (get-test-message test-struct)
  (cond [(zero? (hash-count test-table)) ""]
        [(not test-struct) SBAR_ALL_PASS]
        [(passed-test? test-struct) default-statusbar-message]
        [(failed-test? test-struct)
         (local [(define prefix (linenum-prefix test-struct))
                 (define value1 (failed-test-first-value test-struct))
                 (define part1 (stringify "actual value" value1))
                 (define value2 (failed-test-second-value test-struct))
                 (define part2
                   (if (void? value2)
                     ""
                     (stringify "; expected" value2)))]
                (string-append prefix part1 part2))]
        [(error-test? test-struct)
         (string-append (linenum-prefix test-struct)
                        (stringify "exception"
                                   (error-test-error-msg test-struct)))]))


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
  (local [(define expr (syntax->datum syn))]
         (and (list? expr)
              (not (empty? expr))
              (member (first expr) test-statements))))


; syntax -> (or/c boolean void)
; Takes a syntax object containing a test expression, i.e. "(test exp1 exp2)"
; Returns #t if exp1 and exp2 evaluate to the same value, #f if not,
; and (void) if either of exp1 or exp2 have bad syntax.
(define (test-passes? test-syn evaluator linenum)
  (define defn-evaluator
    (with-handlers [(exn:fail:syntax?         (lambda (e) (error-test (exn-message e))))
                    (exn:fail:out-of-memory?  (lambda (e) (error-test (exn-message e))))
                    ; workaround for exn:fail:out-of-memory? not terminating
                    (exn:fail:resource?       (lambda (e) (error-test (exn-message e))))
                    (exn:fail?                (lambda (e) (error-test (exn-message e))))]
      evaluator))

  (define test-exp
    (with-handlers [ (exn:fail:syntax? (lambda (e)  (error-test (exn-message e))))
                     (exn:fail? (lambda (e)  (error-test (exn-message e))))]
                   (syntax->datum test-syn)))

  (define (error-test msg)     (make-error-test linenum msg))
  (define passd-test  (make-passed-test (void)))
  (define (faild-test exprval testval) (make-failed-test linenum exprval testval))

  (define (try-eval expr)
    (with-handlers [ ;TODO: Need a good syntax matcher
                     (exn:fail:syntax? (lambda (e) (error-test (exn-message e))))
                     (exn:fail? (lambda (e) (error-test (exn-message e))))]
                   (with-limits 0.2 0.1 (defn-evaluator expr))))

  (define (test-eq actual expected)
    (local [(define actual-val    (try-eval actual))
            (define expected-val  (try-eval expected))]
           (cond [(error-test? actual-val) actual-val]
                 [(error-test? expected-val) expected-val]
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
                 (if (and (string? str) (error-test? (try-eval actual))) ; <- this should be other-error, once syntax-matching is fixed.
                   passd-test
                   (faild-test "no exception raised" (void))))]
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
         ;;(thread (thunk (test-passes?-helper)))
         ;; Record the first failed test, if any.
         (test-passes?-helper))
  ) ;; define (test-passes?)


