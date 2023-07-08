#lang racket
(require racket/generator)

(provide (all-defined-out))


(define (ordered-keys h fun)
  (sort (hash-keys h) fun))

(define (pairs-in-order h #:sort-function [sort-function symbol<?])
  (define ok (ordered-keys h sort-function))
  (in-list (for/list ([k ok])
             (cons k (hash-ref h k)))))

(define (symbol>? a b)
  (string>? (symbol->string a)
            (symbol->string b)))

(define (sorted-key o) (car o))
(define (sorted-value o) (cdr o))

#|
(define h (hash 'a 3 'b 2 'c 1))
(for ([p (pairs-in-order h)])
  (printf "~a~n" (sorted-key p)))
|#
