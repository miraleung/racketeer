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

;; STRUCTS
;; Test/error struct
(define-struct passed-test (void))
(define-struct failed-test (void))
(define-struct syntax-error (void))
(define-struct out-of-memory-error (void))
(define-struct other-error (void)) ; meant for exceptions



;; STYLE DEFS
;(define bolddelta (make-object style-delta% 'change-weight 'bold))
(define pass-delta (send (send (make-object style-delta% 'change-weight 'bold)
                               set-delta-foreground "black") set-delta-background "green"))
(define fail-delta (send (send (make-object style-delta% 'change-weight 'bold)
                               set-delta-foreground "white") set-delta-background "red"))
(define syn-error-delta (send (send (make-object style-delta% 'change-weight 'bold)
                                    set-delta-foreground "black") set-delta-background "yellow"))
(define oom-error-delta (send (send (make-object style-delta% 'change-weight 'bold)
                                    set-delta-foreground "white") set-delta-background "blue"))
(define oth-error-delta (send (send (make-object style-delta% 'change-weight 'bold)
                                    set-delta-foreground "white") set-delta-background "purple"))
(define normal-delta (send (send (make-object style-delta% 'change-weight 'normal)
                                 set-delta-foreground "black") set-delta-background "white"))
;(send bolddelta set-weight-on 'bold)
;(send bolddelta set-weight-off 'normal)


(define (boolean->string b) (if b "#t" "#f"))
(define pass-test-msg "; yay, it passed")
(define fail-test-msg "; yay, it failed")
(define syn-error-msg "; < syntax error")
(define oom-error-msg "; out-of-memory ")
(define oth-error-msg "; error occurred ")
(define test-msg-len (string-length pass-test-msg))

(define first-test-error-status SBAR_ALL_PASS)

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
             line-start-position
             line-end-position
             save-port
             set-styles-sticky)

    (define highlight-tests? (preferences:get 'drracket:racketeer-highlight-tests?))

    (define/public-final (highlight?) highlight-tests?)
    (define/public-final (toggle-highlight!)
                         (preferences:set 'drracket:racketeer-highlight-tests? (not highlight-tests?))
                         (set! highlight-tests? (not highlight-tests?)))

    ;; EVENT HANDLERS.
    (define/augment (on-insert start len)
      (begin-edit-sequence))
    (define/augment (after-insert start len)
      ;; (thread (thunk (check-range start (+ start len) HANDLER_AFTER_INSERT)))
      (check-range start (+ start len))
      (end-edit-sequence))

    (define/augment (on-delete start len)
      (begin-edit-sequence))
    (define/augment (after-delete start len)
      ;; (thread (thunk (check-range start (+ start len) HANDLER_AFTER_DELETE)))
      (check-range start (+ start len))
      (end-edit-sequence))

    (define/augment (on-load-file loaded? format)
      (begin-edit-sequence))
    (define/augment (after-load-file loaded?)
;      (thread (thunk (check-range 0 (last-position)))))
      ;(future (thunk first-highlight-refresh)))
      (first-highlight-refresh)
      (end-edit-sequence))
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


;; TODO: Remove handler param?
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
                (define test-output (open-output-string))
                (define evaluator (parameterize [(sandbox-eval-limits '(1 20))
                                                 (current-error-port test-output)]
                                    (make-module-evaluator eval-in-port)))
                ; The value of evaluator is #f if the source code has syntax errors.
                (when evaluator
                  (define tests (get-tests test-in-port))
                  (for ([test-syn tests])
                    (define test-start (max 0 (- (syntax-position test-syn) 1)))
                    (define test-end (+ (syntax-position test-syn) (syntax-span test-syn)))
                    (define test-rc (test-passes? test-syn evaluator))
                    (define test-msg (get-test-msg test-rc))
                    (define (highlight-tests)
                      (change-style
                        (cond [(syntax-error? test-rc)        syn-error-delta]
                              [(other-error?  test-rc)        oth-error-delta]
                              [(out-of-memory-error? test-rc) oom-error-delta]
                              [(passed-test? test-rc)         pass-delta]
                              [(failed-test? test-rc)         fail-delta])
                        test-start test-end))
                    #;(message-box "test" (string-append "Testing:\n" (get-text test-start test-end) "\n\nResult:\n" test-msg))
                    (highlight-tests)
;                    (queue-callback highlight-tests)
                    ) ;; for
                  ) ;; when
                ) ;; when
              ) ;; with-handlers
            ;; PROTOTYPING STATUS BAR
            (when (> (string-length (get-text 0 (last-position))) 20)
              (define frame (send (get-tab) get-frame))
              (send frame set-rktr-status-message first-test-error-status))
            ) ;; when
          ) ;; let
        ) ;; if
      ) ;; define
    (super-new))))

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
(define (test-passes? test-syn evaluator)
  (define defn-evaluator
    (with-handlers [(exn:fail:syntax?         (lambda (e) syn-err))
                    (exn:fail:out-of-memory?  (lambda (e) oom-err))
                    ; workaround for exn:fail:out-of-memory? not terminating
                    (exn:fail:resource?       (lambda (e) oom-err))
                    (exn:fail?                (lambda (e) other-err))]
      evaluator))

  (define test-exp
    (with-handlers [ (exn:fail:syntax? (lambda (e) syn-err))
                     (exn:fail:out-of-memory? (lambda (e) oom-err))
                     ; workaround for exn:fail:out-of-memory? not terminating
                     (exn:fail:resource? (lambda (e) oom-err))
                     (exn:fail? (lambda (e) other-err))]
                   (syntax->datum test-syn)))

  (define syn-err     (make-syntax-error (void)))
  (define oom-err     (make-out-of-memory-error (void)))
  (define other-err   (make-other-error (void)))
  (define passd-test  (make-passed-test (void)))
  (define faild-test  (make-failed-test (void)))

  (define (try-eval expr)
    (with-handlers [ ;TODO: Need a good syntax matcher
                     (exn:fail:syntax? (lambda (e) syn-err))
                     ; workaround for exn:fail:out-of-memory? not terminating
                     (exn:fail:resource? (lambda (e) oom-err))
                     (exn:fail? (lambda (e) other-err))]
                   (with-limits 0.2 0.1 (defn-evaluator expr))))

  (define (test-eq actual expected)
    (local [(define actual-val    (try-eval actual))
            (define expected-val  (try-eval expected))]
           (cond [(or (syntax-error? actual-val) (syntax-error? expected-val))        syn-err] ; syntax error
                 [(or (out-of-memory-error? actual-val) (syntax-error? expected-val)) oom-err]
                 [(or (other-error? actual-val) (other-error? expected-val))          other-err]
                 [else (if (eq? actual-val expected-val)
                         passd-test
                         faild-test)])))
  (local [(define (test-passes?-helper)
            (match test-exp
              [(list (or 'check-expect 'test) actual expected)
               (test-eq actual expected)]
              [(list 'test/exn actual str)
               (if (not (string? str))
                 syn-err
                 (if (and (string? str) (other-error? (try-eval actual))) ; <- this should be other-error, once syntax-matching is fixed.
                   passd-test
                   faild-test))]
              [(list 'check-error actual)
               ; TODO: If try-eval returns an oom-error, it should be an error struct, not a failed-struct.
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
              [else syn-err]))] ;; TODO: Change this to other-error ??
         ;;(thread (thunk (test-passes?-helper)))
         (test-passes?-helper))
  ) ;; define (test-passes?)

(define (get-test-msg test-rc)
        (cond [(syntax-error? test-rc) syn-error-msg]
          [(out-of-memory-error? test-rc) oom-error-msg]
          [(other-error? test-rc) oth-error-msg]
          [(passed-test? test-rc) pass-test-msg]
          [(failed-test? test-rc) fail-test-msg]))
