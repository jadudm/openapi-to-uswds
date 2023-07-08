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
                         #:destination-filename destination
                         #:template-filename template-filename
                         #:title [title "FAC API version ~a documentation"]
                         #:version [version "1.0.0"]
                         )
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
    (render #:endpoints (map sorted-key
                               (stream->list (pairs-in-order (hash-ref json 'definitions))))
              #:definitions (hash-ref json 'definitions)
              #:title title
              #:version version
              ))
  (define templatep (open-input-file template-filename))

  (define outfp (open-output-file destination #:mode 'text #:exists 'replace))

  (display "<!DOCTYPE html>" outfp)
  (for ([line (port->lines templatep)])
    (cond
      [(regexp-match "<!-- TITLE -->" line)
       (displayln (format title version) outfp)
       ]
      [(regexp-match "<!-- MAIN -->" line)
       (display-xml/content (xexpr->xml assembled-document)
                       outfp
                       #:indentation 'classic)
       ]
      [else
       (displayln line outfp)
       ]))
  (close-output-port outfp)
  )

(provide process-openapi)

(module* main cli
  (require (submod ".."))

  
  (define default-url "https://fac-dev-postgrest.app.cloud.gov/")
  (define default-destination "fac-api-documentation.html")
  (define default-template "template.html")

  
  (flag (openapi-url url)
        ("-u" "--url" "OpenAPI URL")
        (openapi-url url))

  (flag (destination-file fname)
        ("-o" "--outfile" "Output filename (.html)")
        (destination-file fname))

  (flag (template-file tfname)
        ("-t" "--template" "Template filename")
        (template-file tfname))
  
  (program (openapi-to-uswds)
           (process-openapi
            #:url (or (openapi-url) default-url)
            #:destination-filename (or (destination-file) default-destination)
            #:template-filename (or (template-file) default-template)
            ))
  
  (run openapi-to-uswds))
