#lang racket/base
(provide load

         ;; user config vars
         init
         enhance-body
         clean)

(define config-vars #f)

(define (load top)
  (define frog.rkt (build-path top "frog.rkt"))
  (set! config-vars
        (for/hash ([sym '(init enhance-body clean)])
          (let ([fn (with-handlers ([exn:fail:filesystem? cannot-find-frog.rkt])
                      (dynamic-require frog.rkt sym))])
            (values sym fn)))))

(define ((make-config-fun sym) . args)
  (apply (or (hash-ref config-vars sym #f)
             (error sym "not yet dynamic-required from frog.rkt"))
         args))

(define (cannot-find-frog.rkt . _)
  (eprintf "Cannot open frog.rkt.\nMaybe you need to `raco frog --init` ?\n")
  (exit 1))

(define init (make-config-fun 'init))
(define enhance-body (make-config-fun 'enhance-body))
(define clean (make-config-fun 'clean))

;; ----------------------------------------
(module+ test
  (require rackunit
           racket/runtime-path)
  (test-case "before loading example/frog.rkt"
    (check-exn #rx"init: not yet dynamic-required from frog.rkt"
               (λ () (init)))
    (check-exn #rx"enhance-body: not yet dynamic-required from frog.rkt"
               (λ () (enhance-body '((p () "hi")))))
    (check-exn #rx"clean: not yet dynamic-required from frog.rkt"
               (λ () (clean))))
  (define-runtime-path example "../../../example/")
  (test-case "after loading example/frog.rkt"
    (load example)
    (check-not-exn (λ () (init)))
    (check-not-exn (λ () (enhance-body '((p () "hi")))))
    (check-not-exn (λ () (clean)))))
