#lang racket
(require "base.rkt"
         "template.rkt")
(require
  data/order
  json
  net/http-easy
  racket/pretty
  xml)


(define (process-openapi #:url url
                         #:destination destination)
  (define res (get url))
  (cond
    [(= 200 (response-status-code res))
     'continue]
    [(= 400 (response-status-code res))
     (printf "Bad response: 400~n")
     (exit 1)])
  (define body (response-body res))
  (response-close! res)
  (define json
    (with-input-from-string (bytes->string/utf-8 body)
      (λ () (read-json))))
  (define assembled-document
    (template #:endpoints (map sorted-key
                               (stream->list (pairs-in-order (hash-ref json 'definitions))))
              #:definitions (hash-ref json 'definitions)                              
              ))

  (define outp (open-output-file destination #:mode 'text #:exists 'replace))
  (display "<!DOCTYPE html>" outp)
  (display-xml/content (xexpr->xml assembled-document)
                       outp
                       #:indentation 'classic)
                       
  )

(provide process-openapi)

(module* main cli
  (require (submod ".."))

  
  (define default-url "https://fac-dev-postgrest.app.cloud.gov/")
  (define default-destination "fac-api-documentation.html")

  
  (flag (openapi-url url)
        ("-u" "--url" "OpenAPI URL")
        (openapi-url url))

  (flag (destination-file fname)
        ("-o" "--outfile" "Output filename (.html)")
        (destination-file fname))
  
  (program (openapi-to-uswds)
           (process-openapi
            #:url (or (openapi-url) default-url)
            #:destination (or (destination-file) default-destination)
            ))
  
  (run openapi-to-uswds))
