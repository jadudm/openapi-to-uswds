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