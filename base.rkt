#lang racket
(require racket/generator)

(provide (all-defined-out))


(struct Endpoint (name description fields)
  #:transparent)
(struct Field (name description type format max_length)
  #:transparent)
(struct empty-string ())


(define (endpoints-in-order loe)
  (sort loe (lambda (a b) (string<?
                           (Endpoint-name a)
                           (Endpoint-name b)))))

(define (fieldnames-in-order lof)
  (sort lof (lambda (a b) (string<?
                           (Field-name a)
                           (Field-name b)))))

;;; Contract: (-> hash? (and/c hash? immutable?))
(define (hash->immutable-hash table)
  (if (immutable? table) 
      table ;; If hash is already immutable, just return it
      (for/hash ([(k v) (in-mutable-hash table)]) (values k v))))