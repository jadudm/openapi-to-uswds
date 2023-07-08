#lang racket
(require "base.rkt"
         gregor)
(provide render)

(provide (all-defined-out))

(define (render #:endpoints [endpoints '()]
                  #:definitions [definitions (hash)]
                  #:title title
                  #:version version
                  )
  ` (div ([class "usa-section"])
         (div ([class "grid-container"])
              (div ([class "grid-row grid-gap"])
                   (main ([class "usa-layout-docs__main desktop:grid-col-9 usa-prose usa-layout-docs"]
                          [id "main-content"])
                         (h1 ,(format title version))
                         (h2 ([id "api-endpoints"])
                             "API endpoints")
                         ,(endpoints->index endpoints)
                         ,(render-endpoints endpoints definitions)
                         ))
              (div ([class "grid-row grid-gap"])
                   (p (em
                       ,(format "Last renedered ~a" (~t (today #:tz "America/New_York")
                                                        "E, MMMM d, y"))
                       )))
              ))
  )

;;;;;

(define (deep-ref table #:default [default ""] . keys )
  (define result table)
  (for ([k keys])
    (set! result (hash-ref result k default)))
  result)

(define (endpoints->index endpoints)
  `(ul
    ,@(for/list ([endpoint endpoints])
        `(li (a ([href ,(format "#endpoint-~a" endpoint)])
                ,(~a endpoint))))))


(define (render-endpoints endpoints definitions)
  (define pieces
    (for/list ([endpoint endpoints]
               [ndx (length endpoints)])
      (define toggle-target (format "accordion-endpoint-~a" ndx))
      `(div
        (a ([name ,(format "endpoint-~a" endpoint)]))
        ;; Endpoint name
        (h3 ,(~a endpoint))
        ;; Collapse the parameters
        (h4 ([class "usa-accordion__heading"]
             )
            (button ([type "button"]
                     [class "usa-accordion__button"]
                     [aria-expanded "false"]
                     [aria-controls ,toggle-target]
                     )
                    ,(~a (format "Query params on the ~a endpoint"
                                 endpoint
                                 ))))
        (div ([id ,toggle-target]
              [class "usa-accordion__content usa-prose"])
             (p ,(~a (deep-ref definitions 'description)))
             (h3 "Fields")
             ,@(for/list ([ffp (pairs-in-order (deep-ref definitions endpoint 'properties))])
                 `(div ([class "grid-row"])
                       (div ([class "grid-col-4"])
                            (h4 ,(~a (sorted-key ffp))))
                       (div ([class "grid-col-2"])
                            (br)
                            (span ([class "usa-tag"])
                                  ,(~a (hash-ref (sorted-value ffp) 'type ""))))
                       (div ([class "grid-col-6"])
                            (p ,(~a (hash-ref (sorted-value ffp) 'description "")))))
                 )))))

  `(div ([class "usa-accordion"])
        ,@pieces
        ))
