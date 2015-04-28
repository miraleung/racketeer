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
         rackunit
         unstable/function
         wxme)

;; CONSTANTS
;; Messages
(define UNREC_TEST_VARIANT "unrecognized test variant")


;; Test states
(define STATE_PASS 'pass)
(define STATE_FAIL 'fail)
(define STATE_ERROR 'error)

;; Test expression constants.
;; (or/c syntax? #f)
(define first-error-test-status #f)

;; Statusbar.
(define SBAR_ALL_PASS "All tests pass")
(define default-statusbar-message "")


;; Evaluation limits.
(define EVAL_INTERVAL_SECONDS 2)
(define EVAL_LIMIT_SECONDS 0.2)
(define EVAL_LIMIT_MB 0.1)

;; Language settings (from GUI).
(define UNINITIALIZED 'uninitialized)
(define CURRENT-LIBRARY UNINITIALIZED)


;; Colour scheme: GitHub diffs.
;; COLOUR CONSTANTS
;; In bytes.
(define COLOUR_ALPHA 0.8)

;; Pass colour. #DBFFDB
(define COLOUR_PASS_RGB_R 219)
(define COLOUR_PASS_RGB_G 255)
(define COLOUR_PASS_RGB_B 219)
(define COLOUR_PASS (make-object color% COLOUR_PASS_RGB_R
                                 COLOUR_PASS_RGB_G
                                 COLOUR_PASS_RGB_B
                                 COLOUR_ALPHA))

;; Fail colour: #FFDDDD
(define COLOUR_FAIL_RGB_R 255)
(define COLOUR_FAIL_RGB_G 221)
(define COLOUR_FAIL_RGB_B 221)
(define COLOUR_FAIL (make-object color% COLOUR_FAIL_RGB_R
                                 COLOUR_FAIL_RGB_G
                                 COLOUR_FAIL_RGB_B
                                 COLOUR_ALPHA))
;; Orange error colour: FCD9B6
(define COLOUR_ERROR_RGB_R 252)
(define COLOUR_ERROR_RGB_G 217)
(define COLOUR_ERROR_RGB_B 182)
(define COLOUR_ERROR (make-object color% COLOUR_ERROR_RGB_R
                                 COLOUR_ERROR_RGB_G
                                 COLOUR_ERROR_RGB_B
                                 COLOUR_ALPHA))

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

;; Thread flags.
(define racketeer-thread #f)
(define highlight-thread #f)
(define mouse-event-thread #f)
(define clear-highlight-thread #f)

;; Editor event flags.
(define insert-event-counter 0)
(define delete-event #f)
(define focus-event #f)
(define lang-change-event #f)
(define mouse-event #f)
(define new-window-event #f)
(define file-load-event #f)
(define CURRENT-TAB #f)

(define highlighting-cleared #t)

;; Intervals of evaluating the file.
(define EVAL_INTERVAL 140) ; milliseconds
(define MOUSE_EVAL_INTERVAL 175)

;; Test status indicators.
(define (passed-test? ts) (and (test-struct? ts) (symbol=? STATE_PASS (test-struct-state ts))))
(define (failed-test? ts) (and (test-struct? ts) (symbol=? STATE_FAIL (test-struct-state ts))))
(define (error-test? ts)  (and (test-struct? ts) (symbol=? STATE_ERROR (test-struct-state ts))))

;; Text highlighting.
(define pass-delta
  (send (make-object style-delta% 'change-nothing)
        set-delta-background COLOUR_PASS))

(define fail-delta
  (send (make-object style-delta% 'change-nothing)
        set-delta-background COLOUR_FAIL))

(define error-delta
  (send (make-object style-delta% 'change-nothing)
        set-delta-background COLOUR_ERROR))

(define normal-delta
  (make-object style-delta% 'change-nothing))

;; Map of test expressions.
(define test-table (make-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide racketeer-testing-mixin)

(define racketeer<%>
  (interface ()))

(define racketeer-testing-mixin
  (lambda (cls)
    (class* cls ()

      (inherit begin-edit-sequence
               change-style
               dc-location-to-editor-location
               end-edit-sequence
               find-newline
               get-filename
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

      ;; Highlighting handlers.
      (define highlight-tests? (preferences:get 'drracket:racketeer-highlight-tests?))

      (define/public-final (highlight?) highlight-tests?)
      (define/public-final (toggle-highlight!)
        (preferences:set 'drracket:racketeer-highlight-tests? (not highlight-tests?))
        (set! highlight-tests? (not highlight-tests?))
        (when (highlight?)
          (set! focus-event #t)
          (start-racketeer))
        (when (not (highlight?))
          (stop-racketeer)
          (clear-statusbar-label)
          (un-highlight-all-tests)))

      ;; Keyboard focus handler.
      (define/override (on-focus on?)
        (super on-focus on?)
        (when (and on? (highlight?) (not (is-racketeer-running?)) (not focus-event))
          (set! focus-event #t)
          (when (not (eq? CURRENT-TAB (get-tab)))
            (set! new-window-event #t)
            (set! CURRENT-TAB (get-tab)))
          (start-racketeer))
        (when (not on?)
          (set! focus-event #f)
          (stop-racketeer)))

      (define/private (start-racketeer)
        (stop-racketeer)
        (get-gui-language)
        (set! racketeer-thread (thread (lambda () (check-range)))))

      (define/private (stop-racketeer)
        (when (is-thread-running? racketeer-thread)
          (kill-thread racketeer-thread)))

      (define/private (is-racketeer-running?)
        (is-thread-running? racketeer-thread))

      ;; File event handler.
      (define/augment (after-load-file success?)
        (set! file-load-event #t)
        (clear-statusbar-label))

      ;; Mouse event handler.
      (define/override (on-event mouse-evt)
        (super on-event mouse-evt)
        (when (and (not (send mouse-evt button-down?))
                   (not (send mouse-evt leaving?))
                   (not (boolean? first-error-test-status)) ;; Only when not all tests are passing
                   (not (is-thread-running? mouse-event-thread))) ;; and this.
          (set! mouse-event #t)
          (set! mouse-event-thread
            (thread (thunk (mouseover-test-handler (send mouse-evt get-y)))))))

      ;; GUI language selector event handler.
      (define/augment (after-set-next-settings lang-settings)
        (get-gui-language)
        (set! lang-change-event #t))

      ;; Editor event handlers.
      (define/augment (on-insert start len)
        (begin-edit-sequence))
      (define/augment (after-insert start len)
        (set! insert-event-counter (+ insert-event-counter 1))
        (end-edit-sequence))

      (define/augment (on-delete start len)
        (begin-edit-sequence))
      (define/augment (after-delete start len)
        (set! delete-event #t)
        (end-edit-sequence))

      (define/override (on-char event)
        (begin-edit-sequence)
        (super on-char event)
        (end-edit-sequence))
      (define/override (on-local-char event)
        (begin-edit-sequence)
        (super on-local-char event)
        (end-edit-sequence))
      (define/override (on-default-char event)
        (begin-edit-sequence)
        (super on-default-char event)
        (end-edit-sequence))

      (define/augment (on-change-style start len)
        (begin-edit-sequence #f #f))
      (define/augment (after-change-style start len)
        (end-edit-sequence))


      ;; ========================================================
      ;; Class-private helpers.
      ;; ========================================================

      (define/private (mouseover-test-handler y-coord)
        (when (and (highlight?)
                   (not (zero? (last-position)))
                   ;; Evaluates at an interval proportional to the size of the file.
                   (zero? (modulo (current-milliseconds) MOUSE_EVAL_INTERVAL)))
          (mouseover-helper y-coord)
          (set! mouse-event #f))
        (when mouse-event ;; Loop while the last mouse event wasn't handled yet.
          (mouseover-test-handler y-coord)))

      (define/private (mouseover-helper y-coord)
        (define cursor-expr (find-test-y-range y-coord))
        (if (not cursor-expr)
          (set-statusbar-to-default)
          (set-statusbar-label (get-test-message cursor-expr))))

      ;; exact-integer -> (or/c passed-test failed-test error-test #f)
      ;; Gets the test at this line.
      (define/private (find-test-y-range y-coord)
        (local [(define lo-y-locns (hash-keys test-table))
                ;; (listof y-locations) -> (or/c passed-test failed-test error-test #f)
                (define-values (ignore1 yc) (dc-location-to-editor-location 0 y-coord))
                (define (find-y-range y-locns)
                  (cond [(empty? y-locns) #f]
                        [(<= (y-locations-top (first y-locns))
                             yc
                             (y-locations-bottom (first y-locns)))
                         (hash-ref test-table (first y-locns))]
                        [else (find-y-range (rest y-locns))]))]
          (if (zero? (hash-count test-table))
              #f
              (find-y-range lo-y-locns))))

      ;; Get the language selected from the DrRacket IDE.
      (define/private (get-gui-language)
         (define frame (send (get-tab) get-frame))
         (define new-lib (send frame get-rktr-current-library))
         (when (not (boolean? new-lib))
           (define lib-name (string-replace (second new-lib) "-reader" ""))
           (set! CURRENT-LIBRARY (list (first new-lib) lib-name (third new-lib)))
           ) ;; when
        (when (not new-lib)
          (set! CURRENT-LIBRARY #f)
          ) ;; when
        ) ;; define

      (define/private (set-statusbar-label message)
        (define frame (send (get-tab) get-frame))
        (send frame set-rktr-status-message message))

      (define/private (clear-statusbar-label)
        (send (send (get-tab) get-frame) set-rktr-status-message ""))

      ;; Traverse all the expressions and evaluate appropriately.
      (define/private (check-range)
        (when (and (highlight?)
                   (not (zero? (last-position)))
                   (or (> insert-event-counter 0)
                       (and (or delete-event lang-change-event new-window-event)
                   ;; Evaluates at an interval proportional to the size of the file.
                            (zero? (modulo (current-milliseconds)
                                           (* EVAL_INTERVAL (max 1 (order-of-magnitude (last-position))))))))
                   ) ;; and
          (define local-insert-event-counter insert-event-counter)
          (define eval-ok (check-range-helper))
          (when (not eval-ok)
            (when (not highlighting-cleared)
              (when (is-thread-running? highlight-thread)
                (kill-thread highlight-thread))
              (when (not (is-thread-running? clear-highlight-thread))
                (set! clear-highlight-thread (thread (lambda () (un-highlight-all-tests))))
                (thread-wait clear-highlight-thread)))
            (when (or (not highlighting-cleared) file-load-event)
              (set! default-statusbar-message "syntax error in expressions")
              (set-statusbar-to-default)))
          (when eval-ok
            (when (is-thread-running? highlight-thread)
                (kill-thread highlight-thread)
                ) ;; when
              (set! highlight-thread (thread (lambda () (highlight-all-tests))))
              (thread-wait highlight-thread)
              ) ;; when eval-ok

          (if (> insert-event-counter local-insert-event-counter) ;; changed during evaluation
              (set! insert-event-counter (- insert-event-counter 1))
              (set! insert-event-counter 0))
            (set! delete-event #f)
            (set! lang-change-event #f)
            (set! new-window-event #f)

          ) ;; when
        (check-range)
        ) ;; define

      (define/private (check-range-helper)
        (define eval-successful #f)
        (let/ec k
          (with-handlers [(exn:fail? (lambda (e) #f))]
            (define src-out-port (open-output-bytes))
            (save-port src-out-port)
            (define test-in-port (open-input-bytes (get-output-bytes src-out-port)))
            (define eval-in-port (open-input-bytes (get-output-bytes src-out-port)))
            (define filename (get-filename))
            (define wxme-flag
              (or (is-wxme-stream? test-in-port)
                  (is-wxme-stream? eval-in-port)))

            ;; Handle WXME files.
            ;; If eval-in-port is a WXME-port, it will be handled by {@code synreader}.
            (when (is-wxme-stream? test-in-port)
              (set! test-in-port (wxme-port->port test-in-port))
              ) ;; when

            ; If anything is written to the error port while creating the evaluator,
            ; write it to a string port
            (define evaluator (parameterize [(sandbox-eval-limits '(10 20))
                                             (sandbox-namespace-specs (append (sandbox-namespace-specs) '(rackunit)))]
                                (make-module-evaluator
                                  (remove-tests eval-in-port filename wxme-flag))))
            (when evaluator
              (set-eval-limits evaluator EVAL_LIMIT_SECONDS EVAL_LIMIT_MB)
              (define tests (get-tests test-in-port))
              (when wxme-flag (set! tests (reverse tests)))

              ;; Clear test hash table, test status.
              (set! test-table (make-hash))
              (set! first-error-test-status #f)

              ;; Clear statusbar
              (set-statusbar-label "evaluating expressions ...")

              (for ([test-syn tests])
                (define test-start (max 0 (- (syntax-position test-syn) 1)))
                (define test-end (+ (syntax-position test-syn) (syntax-span test-syn)))
                (define linenum (position-line test-start))
                (define test-rc
                  (test-passes? test-syn evaluator linenum test-start test-end))
                (define-values (ignore1 y-top) (values 0 (line-location linenum)))
                (define-values (ignore2 y-bottom) (values 0 (line-location linenum #f)))

                (define y-locns (make-y-locations y-top y-bottom))

                ;; New hash table entry.
                (hash-set! test-table y-locns test-rc)

                ;; Set the first syntax error object.
                (when (and (not first-error-test-status) (not (passed-test? test-rc)))
                    (set! first-error-test-status test-rc))

                ) ;; for
              ;; Statusbar thread doesn't interfere with editor (canvas) events.
              (set! default-statusbar-message (get-test-message first-error-test-status))
              (thread (lambda () (set-statusbar-to-default)))
              (set! eval-successful #t)
              ) ;; when
            ) ;; with-handlers
           ) ;; let
        eval-successful
        ) ;; define

      (define/private (highlight-all-tests)
        (queue-callback highlight-all-tests-helper #f))

      (define (highlight-all-tests-helper)
        (define (hilite key-ignore test-rc)
          (define test-start (test-struct-start-posn test-rc))
          (define test-end (test-struct-end-posn test-rc))
          (when (test-struct? test-rc)
            (queue-callback (lambda () (change-style
              (cond [(error-test? test-rc)  error-delta]
                    [(passed-test? test-rc) pass-delta]
                    [(failed-test? test-rc) fail-delta])
              test-start test-end)) #f) ;; Low priority on the main eventspace thread.
            ) ;; when
          ) ;; define
        ;; Start the highlighting legwork.
        ;; Do not extract into a separate method - location critical for thread safety.
        (send (send (send (send (get-tab) get-frame) get-editor) get-canvas) enable #f) ;; Disable editor.
        (begin-edit-sequence #f #f) ;; Take this off the undo stack.
        (when (not highlighting-cleared)
          (define ch-thread (thread (lambda () (change-style normal-delta 0 (last-position)))))
          (thread-wait ch-thread))
        (hash-for-each test-table hilite)
        (end-edit-sequence)
        (send (send (send (send (get-tab) get-frame) get-editor) get-canvas) enable #t)
        (send (send (send (send (get-tab) get-frame) get-editor) get-canvas) focus) ;; Return focus.
        (set! highlighting-cleared #f)
        ) ;; define

      (define/private (un-highlight-all-tests)
        (queue-callback un-highlight-all-tests-helper #f))

      (define (un-highlight-all-tests-helper)
        (set! test-table (make-hash)) ;; Clear test table.
        ;; Unhighlight all the things.
        ;; Do not extract into a separate method - location critical for thread safety.
        (send (send (send (send (get-tab) get-frame) get-editor) get-canvas) enable #f)
        (begin-edit-sequence #f #f)
        (define ch-thread (thread (lambda () (change-style normal-delta 0 (last-position)))))
        (thread-wait ch-thread)
        (end-edit-sequence)
        (send (send (send (send (get-tab) get-frame) get-editor) get-canvas) enable #t)
        (send (send (send (send (get-tab) get-frame) get-editor) get-canvas) focus)
        (set! highlighting-cleared #t)
        ) ;; define

      (define (set-statusbar-to-default)
        (set-statusbar-label default-statusbar-message))

      (super-new))))


(define (is-thread-running? th)
  (and (thread? th) (thread-running? th)))

;; Workaround to get test results/values into a string.
(define (stringify prefix message)
  (string-append
   prefix
   (let [(o (open-output-string))]
     (parameterize [(current-output-port o)]
       (display (format ": ~a" message)))
     (get-output-string o))))

;; Get the line number of the test struct.
(define (linenum-prefix ts)
  (if (not ts)
    ""
    (string-append "line " (number->string (+ 1 (test-struct-linenum ts))) ": ")))

;; test-struct -> string
;; Format test messages for failed/error tests.
;; Use of stringify is due to unknown types of test subexpressions.
(define (get-test-message ts)
  (cond [(zero? (hash-count test-table)) ""]
        [(not ts) SBAR_ALL_PASS]
        [(passed-test? ts) (string-append (linenum-prefix ts) "test passes")]
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


(define rackunit-test-stms (list
                              'check-equal? 'check-eqv? 'check-eq?
                              'check-not-equal? 'check-not-eqv? 'check-not-eq?
                              'check-pred 'check-= 'check-true 'check-false 'check-not-false
                              'check-exn 'check-not-exn
                              'check-regexp-match 'check-match
                              'check 'fail
                              ))

(define test-statements (append
                          (list 'test 'test/pred 'test/exn
                              'check-expect 'check-error
                              'check-satisfied 'check-range 'check-member-of)
                          rackunit-test-stms))


;; Syntax-object reader.
(define (synreader src-port)
  (parameterize [(read-accept-reader  #t)
                 (read-accept-lang    #t)]
    (if (is-wxme-stream? src-port)
      (wxme-read-syntax 'program src-port)
      (read-syntax 'program src-port))))


(define (get-tests src-port)
  (reverse (syntax-expander (get-syntax-list src-port))))

(define (get-syntax-list src-port)
  (local [(define (get-syntax-list-helper src-port syntax-list)
            (local [(define syn (synreader src-port))]
              (if (syntax? syn)
                  (get-syntax-list-helper src-port (cons syn syntax-list))
                  syntax-list)))]
    (get-syntax-list-helper src-port empty)))

;; Expand all (nested) syntax-objects.
(define (syntax-expander to-expand)
  (local [(define (syntax-expander-helper to-expand syntax-list)
            (if (empty? to-expand)
              syntax-list
              (local [(define expanded (syntax->list (first to-expand)))]
                     (if expanded
                       (if (test-syntax? (first to-expand))
                          (syntax-expander-helper
                            (rest to-expand)
                            (cons (first to-expand) syntax-list))
                          (syntax-expander-helper
                            (append (filter syntax? expanded)
                                    (rest to-expand))
                            syntax-list))
                      (syntax-expander-helper (rest to-expand) syntax-list)))))]
    (syntax-expander-helper to-expand empty)))

(define (test-syntax? syn)
  (test-expression? (syntax->datum syn)))

(define (test-expression? expr)
  (and (list? expr)
       (not (empty? expr))
       (member (first expr) test-statements)))


(define (remove-tests src-port filename wxme-port-flag) ;; flag could be set to true for new files
  (define the-syntax #f)
  (if (and (path-string? filename) ;; is a metadata-header file
           (not (boolean? CURRENT-LIBRARY))
           (not wxme-port-flag))
    (set! the-syntax (datum->syntax #f (get-syntax-list src-port)))
    (set! the-syntax (synreader src-port)))
  (test-remover the-syntax filename wxme-port-flag))

;; Remove test expressions and process syntax object for evaluation.
;; There are three cases to handle:
;;  - Plain-text source (i.e. #lang declaration)
;;  - WXME-format
;;  - Files with metadata header lines (invisible from DrRacket).
(define (test-remover syn filename wxme-port-flag)
  ;; If the current language is not a GUI-selected language, get the lang def (declaration).
  (when (or (boolean? CURRENT-LIBRARY) (symbol? CURRENT-LIBRARY))
    (set! CURRENT-LIBRARY (third (syntax->list syn))))
  (define inner-syn
    (if (and (list? (syntax->datum syn))
             (list? CURRENT-LIBRARY)
             (not wxme-port-flag))
      (cons '#%module-begin (syntax->datum syn)) ;; only for metadata-header files
      (list '#%module-begin (syntax->datum syn))))

  (when (and wxme-port-flag (symbol=? 'begin (first (syntax->datum syn))))
    ;; Strip out the first 'begin included by wxme-read-syntax.
    (set! inner-syn
      (foldr cons empty (cons '#%module-begin (rest (syntax->datum syn)))))
    )

  ;; Restructuring is needed on new files only if a language is specified from the GUI.
  ;; New non-WXME files are fine as they are.
  (when (and (not filename)
             (list? CURRENT-LIBRARY))
     (set! syn
       (datum->syntax #f
                      (list 'module
                            'anonymous-module
                            CURRENT-LIBRARY
                            inner-syn)))
   ) ;; when

  ;; Process #reader directive for saved files w/ DrRacket metadata header (but not WXME file).
  (when (and (path-string? filename)
             (list? CURRENT-LIBRARY)
             (not (and (symbol? (syntax-e (first (syntax->list syn))))
                       (symbol=? (syntax-e (first (syntax->list syn))) 'module))))
    (define fileport (open-input-file filename))
    (define filesyn (synreader fileport))
    (close-input-port fileport)
    (define modname (syntax->datum (second (syntax->list filesyn))))
    (define library (syntax->datum (third (syntax->list filesyn))))

    (when (and (not (boolean? CURRENT-LIBRARY)))
      (set! library CURRENT-LIBRARY))

    (when (not (symbol? modname))
      (set! modname 'anonymous-module))

    (when (or (and (list? library)
                   (symbol? (first library)) (symbol=? (first library) 'lib))
              (symbol? library))
      (set! syn
        (datum->syntax #f
                       (list 'module
                             modname
                             library
                             inner-syn)))
      ) ;; when
    ) ;; when
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
    (with-handlers [(exn:fail? (lambda (e) (error-test (exn-message e))))]
	  (defn-evaluator expr)))

  (define (test-eq actual expected)
    (local [ ;; Make the evaluator recognize strings.
            (define (process-string raw-str)
              (string-append "\"" raw-str "\""))
			(define (process-value val)
			  (if (string? val)
			      (process-string val)
				  val))
		    (define actual-prime (try-eval (process-value actual)))
        (define expected-prime (try-eval (process-value expected)))
        (define actual-val
          (if (number? actual-prime)
            (exact->inexact actual-prime)
            actual-prime))
        (define expected-val
          (if (number? expected-prime)
            (exact->inexact expected-prime)
            expected-prime))
        ]
      (cond [(error-test? actual-val) actual-val]
            [(error-test? expected-val) expected-val]
            ;; Needs to be equal? instead of eq? for object comparison.
            [else (if (equal? actual-val expected-val)
                      passd-test
                      (faild-test actual-prime expected-prime))])))

  (local [
        (define litmus-test
            (match test-exp
              [(list 'check-expect ignore1 ignore2) '(check-expect 1 1)]
              [(list 'test ignore1 ignore2)         '(test 1 1)]
              [(list 'test/exn ignore1 ignore2)     '(test/exn (raise-user-error "a") "")]
              [(list 'check-error ignore1 ignore2)  '(check-error (/ 1 0) "/: division by zero")]
              [(list 'test/pred ignore1 ignore2)    '(test/pred 1 odd?)]
              [(list 'check-range ignore1 ignore2)  '(check-range 1 2 1)]
              [(list 'check-satisfied ignore1 ignore2) '(check-satisfied 1 odd?)]
              [(list 'check-member-of  ignore1 ignore2 ...) '(check-member-of 2 1 2)]
              [else '(check-error 1 "")])) ; no such test variant

         (define (check-test)
           (with-handlers [(exn:fail? (lambda (e) #f))] (defn-evaluator litmus-test)))

         ;; Test variant membership check for non-rackunit tests.
         (define (do-check-test fn-do-test)
           (if (false? (check-test))
             (error-test UNREC_TEST_VARIANT)
             (fn-do-test)))

         ;; For handling exceptions when checking whether we're using rackunit.
         (define-syntax rackunit-check-exn-handler
           (syntax-rules ()
             ((_ body ...)
              (with-handlers [(exn:fail:syntax? (lambda (exn) #t))
                              (exn:fail? (lambda (exn) #f))] (defn-evaluator body) ...))))

          (define (test-passes?-helper)
            (match test-exp
              [(list (or 'check-expect 'test) actual expected)
               (do-check-test
                 (lambda ()
                   (test-eq actual expected)))]
              [(list 'test/exn actual str)
               (do-check-test
                 (lambda ()
                   (if (not (string? str))
                   (error-test "second expression must be a string")
                   (local [(define actual-val (try-eval actual))]
                     (if (and (test-struct? actual-val)
                              (error-test? actual-val))
                         passd-test
                         (faild-test actual-val "an exception"))))))]
              [(list 'check-error actual)
               (do-check-test
                 (lambda ()
                   (if (error-test? (try-eval actual))
                     passd-test
                     (faild-test "no error raised" (void)))))]
              [(list 'test/pred expr pred)
               (do-check-test
                 (lambda ()
                   (local [(define pred-app (try-eval pred))]
                     (if (not (procedure? pred-app))
                         (error-test "second expression must be a procedure")
                         (if (pred-app (evaluator expr))
                             passd-test
                             (faild-test (pred-app expr) (void)))))))]
              [(list 'check-range expr min-expr max-expr)
               (do-check-test
                 (lambda ()
                   (if (and (number? expr) (number? min-expr) (number? max-expr))
                     (if (<= min-expr expr max-expr)
                       passd-test
                       (faild-test (format "~a is not between ~a and ~a, inclusive"
                                           expr min-expr max-expr)
                                   (void)))
                     (error-test "all three expressions must be numbers"))))]
              [(list 'check-satisfied expr pred)
               (do-check-test
                 (lambda ()
                   (local [(define pred-app (try-eval pred))]
                          (if (and (procedure? pred-app) (= 1 (procedure-arity pred-app)))
                            (if (false? (pred-app (evaluator expr)))
                              (faild-test #f "non-false value")
                              passd-test)
                            (if (not (procedure? pred-app))
                              (error-test (format "second expression ~a is not a function" pred-app))
                              (error-test (format "second expression must be a 1-argument function; had ~a args"
                                                  (procedure-arity pred-app))))))))]
              [(list 'check-member-of expr vars ...)
               (do-check-test
                 (lambda ()
                   (if (false? (member expr vars))
                     (faild-test (format "~a differs from all given members in ~a" expr vars) (void))
                     passd-test)))]

              ;; Rackunit tests
              [expr
               (define using-rackunit?
                 (rackunit-check-exn-handler `(namespace-variable-value 'test-suite)))
               (if using-rackunit?
                 (rackunit-test-handler expr)
                 (error-test UNREC_TEST_VARIANT))]
              [else (error-test UNREC_TEST_VARIANT)]
              )
            ) ;; define (test-passes?-helper)
          (define (rackunit-test-handler expr)
            (define maybe-test-suite (try-eval `(test-suite "test" ,expr)))
               (cond
                 [(error-test? maybe-test-suite) maybe-test-suite]
                 [else
                  (match (flatten (run-test maybe-test-suite))
                    [(list)
                     (error-test (format "~a: in ~a" UNREC_TEST_VARIANT expr))]
                    [(list (? test-success? rs) ...)
                     passd-test]
                    [(list (? test-success?) ... (and r (not (? test-success?))) _ ...)
                     (match r
                       [(test-failure name (exn:test:check msg mks chk-info-lst))
                        (match chk-info-lst
                          [(list (check-info 'actual actual) (check-info 'expected expected) _ ...)
                           (faild-test actual expected)]
                          [(list (check-info 'actual actual) _ ...)
                           (faild-test actual (void))]
                          [else
                           (faild-test (void) (void))])]
                       [(test-error name exn)
                        (error-test (exn-message exn))])])])
               ) ;; define (rackunit-test-handler)
          ]


    (test-passes?-helper))
  ) ;; define

