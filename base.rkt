#lang racket

(provide (all-defined-out))

(struct kv (key value) #:transparent)

(define (hash->kvs h)
  (for/list ([(k v) h])
    (kv k v)))


(define (lookup ls value #:key [key (λ (o) o)])
  (pretty-print value)
  (pretty-print ls)
  (cond
    [(empty? ls) false]
    [(equal? value (key (first ls)))
     (first ls)]
    [else
     (lookup (rest ls) value #:key key)]))
