#lang racket/base

(require
 racket/match
 (for-syntax racket/base
             syntax/parse))

(provide var)

(module+ test
  (require rackunit)

  (define (macro-expand code)
    (syntax->datum
     (expand-once code))))

(define-syntax (var stx)
  (syntax-parse stx
    [(_ bad-form:id) (raise-syntax-error 'var "missing expression to bind variable" stx #'bad-form)]
    [(_ bad-form:expr) (raise-syntax-error 'var "missing expression to bind pattern" stx #'bad-form)]
    [(_ pat rhs:expr) #'(match-define pat rhs)]
    [(_ pat rhs:expr rem* ...+) #'(begin (match-define pat rhs) (var rem* ...))]))

(module+ test
  (check-equal? (macro-expand #'(var a 1)) '(match-define a 1))
  (check-equal? (macro-expand #'(var a 1 b 2)) '(begin (match-define a 1) (var b 2))))

