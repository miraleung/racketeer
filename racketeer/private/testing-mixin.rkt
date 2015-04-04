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
(define STATE_PASS 'pass)
(define STATE_FAIL 'fail)
(define STATE_ERROR 'error)

;; Test expression constants.
;; (or/c syntax? #f)
(define first-error-test-status #f)

;; Statusbar.
(define SBAR_ALL_PASS "All tests pass.")
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

; Thread flags.
(define running-thread #f)
(define clear-highlighting-thread #f)
(define highlighting-thread #f)
(define statusbar-thread #f)
(define checking-range #f)

; Test status indicators.
(define (passed-test? ts) (symbol=? STATE_PASS (test-struct-state ts)))
(define (failed-test? ts) (symbol=? STATE_FAIL (test-struct-state ts)))
(define (error-test? ts)  (symbol=? STATE_ERROR (test-struct-state ts)))

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
  (send (make-object style-delta% 'change-nothing)
        set-delta-background "white"))

;; Test expressions map.
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
        (if (highlight?)
            (check-range 0 (- (last-position) 1))
            ;; Put on main thread.
            (un-highlight-all-tests)))

      ;; Keyboard event handler.
      (define/override (on-focus on?)
                       (super on-focus on?)
                       (check-range 0 (- (last-position) 1)))

      ;; Mouse event handler.
      (define/override (on-event mouse-evt)
        (super on-event mouse-evt)
        (thread (thunk (mouseover-test-handler (send mouse-evt get-y)))))

      ;; Gui language selector event handler.
      (define/augment (after-set-next-settings lang-settings)
        (get-gui-language))

      ;; Editor event handlers.
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

      (define/augment (after-load-file loaded?)
        (get-gui-language)
        (first-highlight-refresh))

      ;; ========================================================
      ;; Class-private helpers.
      ;; ========================================================

      (define/private (mouseover-test-handler y-coord)
        (define cursor-expr (find-test-y-range y-coord))
        (if (not cursor-expr)
          (set-statusbar-label (get-default-statusbar-message))
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

      ;; Get the language selected from the GUI.
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


      (define/private (first-highlight-refresh)
        (when (highlight?)
          (check-range 0 (- (last-position) 1))))

      (define/private (set-statusbar-label message)
        (define frame (send (get-tab) get-frame))
        (send frame set-rktr-status-message message))

      ;; Traverse all the expressions and evaluate appropriately.
      (define/private (check-range start stop)
        ;; Kill any threads that are still running.
        (when (thread-is-running? running-thread)
          (kill-thread running-thread))
        (when (thread-is-running? statusbar-thread)
          (kill-thread statusbar-thread))
        (when (thread-is-running? clear-highlighting-thread)
          (kill-thread clear-highlighting-thread))
        (when (thread-is-running? highlighting-thread)
          (kill-thread highlighting-thread))

        ;; Make each new thread wait until the last one has finished.
        (set! running-thread (thread (thunk (check-range-helper start stop))))
        (set! statusbar-thread (thread (thunk (set-statusbar-label (get-default-statusbar-message)))))
        (set! clear-highlighting-thread (thread (thunk (clear-highlighting))))
        (thread-wait clear-highlighting-thread)
        (set! highlighting-thread (thread (thunk (highlight-all-tests-helper))))
        (thread-wait highlighting-thread)
        ) ;; define

      (define/private (thread-is-running? th)
                      (and (not (boolean? th)) (thread? th) (thread-running? th)))

      (define/private (check-range-helper start stop)
        (when (not (highlight?))
          (send (send (get-tab) get-frame) set-rktr-status-message ""))
        (define eval-interval
          (if (zero? (last-position))
            1
            (order-of-magnitude (last-position))))
        (when (>= eval-interval 2) ;; >= than 100 lines
          (set! eval-interval (* 2 eval-interval)))
        (when (and (highlight?) (= (modulo (current-seconds) eval-interval) 0))
          (set! checking-range #t)
          ;; Set language if not yet initialized.
          (when (and (symbol? CURRENT-LIBRARY) (symbol=? CURRENT-LIBRARY UNINITIALIZED))
            (get-gui-language))
          (let/ec k
            ; Ignore events that trigger for the entire file.
            (when (not (and (= 0 start) (= (last-position) stop)))
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
                  (set! test-in-port (wxme-port->port test-in-port)))

                  ; If anything is written to the error port while creating the ievaluator,
                  ; write it to a string port
                  ; TODO: Write the contents of the string port to the status bar.
                  (define test-error-output (open-output-string))
                  (define test-output (open-output-string))
                  (define evaluator (parameterize [(sandbox-eval-limits '(10 20))]
                                      (make-module-evaluator
                                        (remove-tests eval-in-port filename wxme-flag))))

                  (when evaluator
                    (set-eval-limits evaluator EVAL_LIMIT_SECONDS EVAL_LIMIT_MB)
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
                      (define-values (ignore1 y-top) (values 0 (line-location linenum)))
                      (define-values (ignore2 y-bottom) (values 0 (line-location linenum #f)))

                      (define y-locns (make-y-locations y-top y-bottom))

                      ;; New hash table entry.
                      (hash-set! test-table y-locns test-rc)

                      ;; Set the first syntax error object.
                      ;; TODO: Optimization point?
                      (when (and (not first-error-test-status) (not (passed-test? test-rc)))
                          (set! first-error-test-status test-rc))

                      ) ;; for
                    ) ;; when
                ) ;; with-handlers
              ) ;; when
            ) ;; let
          (set! checking-range #f)
          ) ;; when
        ) ;; define

      (define (clear-highlighting)
        (begin-edit-sequence #f #f)
        (change-style normal-delta 0 (last-position))
        (end-edit-sequence)
        )

      (define (highlight-all-tests-helper)
        (begin-edit-sequence #f #f)
        (define (hilite key-ignore test-rc)
          (define test-start (test-struct-start-posn test-rc))
          (define test-end (test-struct-end-posn test-rc))
          (when (test-struct? test-rc)
            (change-style
              (cond [(error-test? test-rc)  error-delta]
                    [(passed-test? test-rc) pass-delta]
                    [(failed-test? test-rc) fail-delta])
              test-start test-end)))
        (hash-for-each test-table hilite)
        (end-edit-sequence)
        ) ;; define


      (define/private (un-highlight-all-tests)
        (set! test-table (make-hash))
        (when (thread? clear-highlighting-thread)
          (kill-thread clear-highlighting-thread))
        (set! clear-highlighting-thread (thread (thunk (clear-highlighting))))
        ) ;; define

      (super-new))))

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


(define (get-default-statusbar-message)
  (set! default-statusbar-message (get-test-message first-error-test-status))
  default-statusbar-message)


(define test-statements (list 'test 'test/exn 'test/pred 'check-error 'check-expect))
(define (synreader src-port)
  (parameterize [(read-accept-reader  #t)
                 (read-accept-lang    #t)]
    (if (is-wxme-stream? src-port)
      (wxme-read-syntax 'program src-port)
;      (read-syntax 'program (wxme-port->port src-port))
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
  (test-remover (synreader src-port) filename wxme-port-flag))

;; Remove test expressions and process syntax object for evaluation.
(define (test-remover syn filename wxme-port-flag)
  ;; If the current language is not a GUI-selected language, get the lang def (declaration).
  (when (or (boolean? CURRENT-LIBRARY) (symbol? CURRENT-LIBRARY))
    (set! CURRENT-LIBRARY (third (syntax->list syn))))
  (define inner-syn (list '#%module-begin (syntax->datum syn)))

  (when wxme-port-flag
    ;; Strip out the first 'begin included by wxme-read-syntax.
    (set! inner-syn
      (foldr cons empty (cons '#%module-begin (rest (syntax->datum syn))))))

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
    (with-handlers [ ;TODO: Need a good syntax matcher
                    (exn:fail? (lambda (e) (error-test (exn-message e))))]
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
      (cond [(and (test-struct? actual-val)
                  (error-test? actual-val)) actual-val]
            [(and (test-struct? expected-val)
                  (error-test? expected-val)) expected-val]
            ;; Needs to be equal? instead of eq? for object comparison.
            [else (if (equal? actual-val expected-val)
                      passd-test
                      (faild-test actual-prime expected-prime))])))

  (local [(define (test-passes?-helper)
            (match test-exp
              [(list (or 'check-expect 'test) actual expected)
               (test-eq actual expected)]
              [(list 'test/exn actual str)
               (if (not (string? str))
                   (error-test "second expression must be a string")
                   (local [(define actual-val (try-eval actual))]
                     (if (and (test-struct? actual-val)
                              (error-test? actual-val))
                         passd-test
                         (faild-test actual-val "an exception"))))]
              [(list 'check-error actual)
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
              [else (error-test "unrecognized test variant")]))]
    (test-passes?-helper))
  ) ;; define

