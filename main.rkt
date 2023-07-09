#lang racket
(require "base.rkt"
         "template.rkt")
(require
  data/order
  json
  net/http-easy
  racket/pretty
  xml)

;;
;; json->tree :: JSON object (as native Racket data typeS)
;;
;; Returns a list of Endpoint structures.
(define (json->tree json)
  (for/list ([(endpoint properties) (hash-ref json 'definitions)])
    (Endpoint (symbol->string endpoint)
              (hash-ref properties 'description (empty-string))
              (filter Field?
                      (for/list ([(field-name field-properties) (hash-ref properties 'properties)])
                        (when (hash? field-properties)
                          (Field (symbol->string field-name)
                                 (hash-ref field-properties 'description (empty-string))
                                 (hash-ref field-properties 'type (empty-string))
                                 (hash-ref field-properties 'format (empty-string))
                                 (hash-ref field-properties 'max-length (empty-string)))))))))

;;
;; process-file-or-url :: url filename
;;
;; Consumes a JSON OpenAPI spec (either as a URL or a file in the filesystem)
;; and returns a representation of that JSON object as native Racket data structures
;;
(define (process-file-or-url url json-file)
  (cond
      [json-file
       (define fp (open-input-file json-file))
       (read-json fp)]
      [url

       (define res (get url))
       (cond
         [(= 200 (response-status-code res))
          'continue]
         [(= 400 (response-status-code res))
          (printf "Bad response: 400~n")
          (exit 1)])
     
       (define body (response-body res))
       (response-close! res)
       (with-input-from-string (bytes->string/utf-8 body)
         (λ () (read-json)))]))

(define (write-documentation-to-html #:title title
                                     #:version version
                                     #:document assembled-document
                                     #:input-directory input-directory
                                     #:output-directory output-directory
                                     #:destination-filename destination-filename
                                     #:template-filename template-filename)

  (define templatep (open-input-file (build-path
                                      input-directory
                                      template-filename)))

  
  (define outfp (open-output-file (build-path
                                   output-directory
                                   destination-filename) #:mode 'text #:exists 'replace))

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

;;
;; process-openapi
;;
;; 
(define (process-openapi #:url url
                         #:file json-file
                         #:title [title "FAC API version ~a documentation"]
                         #:version [version "1.0.0"]
                         #:destination-filename destination-filename
                         #:template-filename template-filename
                         #:input-directory input-directory
                         #:output-directory output-directory
                         )

  ;; Takes a file or URL for an OpenAPI JSON spec
  ;; and turns it into a list of Endpoint objects.
  (define json (process-file-or-url url
                                    (build-path
                                      input-directory
                                      json-file)))
  (define tree (json->tree json))

  ;; Assembles the tree of documentation that will
  ;; then be embedded into the template.
  (define assembled-document
    (render
     #:endpoints tree
     #:title title
     #:version version
     ))

  ;; Embeds content into the template and writes it out to a file.
  (write-documentation-to-html #:document assembled-document
                               #:title title
                               #:version version
                               #:destination-filename destination-filename
                               #:template-filename template-filename
                               #:input-directory input-directory
                               #:output-directory output-directory)
  )

;; Need to provide this for the submod below.
(provide process-openapi)

;; The CLI module makes composing the
;; command-line interface straight-forward.
(module* main cli
  (require (submod ".."))

  ;; If they user doesn't provide these, we will.
  (define default-destination "api-documentation.html")
  (define default-template "template.html")
  (define default-output-directory ".")
  (define default-input-directory ".")

  
  (flag (openapi-url url)
        ("-u" "--url" "OpenAPI URL")
        (openapi-url url))

  (flag (openapi-file fname)
        ("-f" "--file" "OpenAPI file (JSON)")
        (openapi-file fname))

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
             [(or (openapi-file) (openapi-url))
              (process-openapi
               #:url (openapi-url)
               #:file (openapi-file)
               #:destination-filename (or (destination-file) default-destination)
               #:template-filename (or (template-file) default-template)
               #:output-directory (or (output-directory) default-output-directory)
               #:input-directory (or (input-directory) default-input-directory)
               )]
             [else
              (printf "You must provide an OpenAPI spec, either as a filename or URL. Exiting.~n")])
           )
  
  (run openapi-to-uswds))
