#lang racket
(require "base.rkt")
(provide template)

(provide (all-defined-out))

(define (template #:title [title "FAC API version ~a documentation"]
                  #:version [version "1.0.0"]
                  #:endpoints [endpoints '()]
                  #:definitions [definitions (hash)]
                  )
  `(html
    (head
     (meta ([charset "UTF-8"]))
     (meta ([http-equiv "X-UA-Compatible"]
            [content "IE=edge"]))
              
     (script ([src "assets/uswds/dist/js/uswds-init.min.js"]))
     (link ([rel "stylesheet"]
            [href "assets/uswds/dist/css/uswds.css"]
            ))
     (meta ([name "viewport"]
            [content "width=device-width, initial-scale=1.0"]))
     (title ,(~a (format title version)))
     )
    (body
     (script ([src "assets/uswds/dist/js/uswds.min.js"]))
     (div ([class "usa-section"])
          (div ([class "grid-container"])
               (div ([class "grid-row grid-gap"])
                    (main ([class "usa-layout-docs__main desktop:grid-col-9 usa-prose usa-layout-docs"]
                           [id "main-content"])
                          (h1 ,(format title version))
                          (h2 ([id "api-endpoints"])
                              "API endpoints")
                          ,(endpoints->index endpoints)
                          ,(render-endpoints endpoints definitions)
                          ))))
     )))

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
          (h4 ([class "usa-accordion__heading"])
              (button ([type "button"]
                       [class "usa-accordion__button"]
                       [aria-expanded "false"]
                       [aria-controls ,toggle-target])
                      ,(~a endpoint)))
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
