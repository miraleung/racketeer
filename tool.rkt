#lang racket/unit

;; Racketeer (final version)
;; Continuous Testing in DrRacket
(require drracket/tool
         framework
         racket/class
         racket/gui
         unstable/function
         "private/testing-mixin.rkt")

(import drracket:tool^)
(export drracket:tool-exports^)

;; Statusbar ==============
(define racketeer-frame-mixin
  (mixin (drracket:unit:frame<%>) ()

    (inherit get-definitions-text)

    ;; Definitions for the status bar.
    ;; Based on panel for drracket-vim-tool (github.com/takikawa/drracket-vim-tool/blob/master/tool.rkt)
    (define rktr-status-parent-panel  'uninitialized)
    (define rktr-status-panel         'uninitialized)
    (define rktr-status-message       'uninit)

    ;; TODO: Re-enable on/off highlighting when study is done.
    #;
    (define/override (file-menu:between-open-and-revert file-menu)
      (super file-menu:between-open-and-revert file-menu)
      (new checkable-menu-item%
           [label "Racketeer Test Highlighting"]
           [parent file-menu]
           [callback
             (lambda (i e) (send (get-definitions-text) toggle-highlight!))]
           [checked (send (get-definitions-text) highlight?)]))

    ;; Construct the frame.
    (define/override (make-root-area-container cls parent)
      (set! rktr-status-parent-panel
            (super make-root-area-container vertical-panel% parent))
      (define root (new cls [parent rktr-status-parent-panel]))
      (set! rktr-status-panel
            (new horizontal-panel%
                 [style '(border)]
                 [stretchable-height #f]
                 [parent rktr-status-parent-panel]))
      (set! rktr-status-message
            (new message%
                 [parent rktr-status-panel]
                 [auto-resize #t]
                 [label "Racketeer Status Bar"]))
      root)

    ;; Message handler.
    (define/public (set-rktr-status-message str)
                   (send rktr-status-message set-label str))

    (super-new)))


(define (phase1) (void))
(define (phase2) (void))

(preferences:set-default 'drracket:racketeer-highlight-tests? #t boolean?)
(drracket:get/extend:extend-definitions-text racketeer-testing-mixin)
(drracket:get/extend:extend-unit-frame racketeer-frame-mixin)
