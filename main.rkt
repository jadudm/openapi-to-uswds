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
                         #:output-directory outdir
                         #:input-directory indir
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
  (define templatep (open-input-file (build-path
                                      indir
                                      template-filename)))

  (define outfp (open-output-file (build-path
                                   outdir
                                   destination) #:mode 'text #:exists 'replace))

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

  (define default-destination "api-documentation.html")
  (define default-template "template.html")
  (define default-output-directory ".")
  (define default-input-directory ".")

  
  (flag (openapi-url url)
        ("-u" "--url" "OpenAPI URL")
        (openapi-url url))

  (flag (destination-file fname)
        ("-o" "--outfile" "Output filename (.html)")
        (destination-file fname))

  (flag (template-file tfname)
        ("-t" "--template" "Template filename")
        (template-file tfname))

  (flag (output-directory od)
        ("-d" "--output-directory" "Output directory")
        (output-directory od))
  (flag (input-directory id)
        ("-i" "--input-directory" "Input directory")
        (input-directory id))
  
  (program (openapi-to-uswds)
           (cond
             [(openapi-url)
              (process-openapi
               #:url (openapi-url)
               #:destination-filename (or (destination-file) default-destination)
               #:template-filename (or (template-file) default-template)
               #:output-directory (or (output-directory) default-output-directory)
               #:input-directory (or (input-directory) default-input-directory)
               )]
             [else
              (printf "You must provide a URL leading to an OpenAPI specification. Exiting.~n")])
           )
  
  (run openapi-to-uswds))
