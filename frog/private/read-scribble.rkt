#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/file
         racket/format
         racket/match
         racket/string
         racket/pretty
         threading
         (only-in xml xexpr?)
         "html.rkt"
         "util.rkt"
         "xexpr2text.rkt"
         "xexpr-map.rkt")

(provide read-scribble-file)

(module+ test
  (require rackunit))

(define/contract (read-scribble-file path
                                     #:img-local-path img-dir
                                     #:img-uri-prefix img-uri)
  (-> path? #:img-local-path path? #:img-uri-prefix string?
      (values (listof xexpr?) hash?))
  ;; This way of running Scribble is cribbed from Ryan Culpepper's
  ;; Scriblogify:
  (define dir (path->string (make-temporary-file "frog~a" 'directory)))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-command-line-arguments
                  (vector "--quiet"
                          "--html"
                          "--dest" dir
                          "--dest-name" "frog.html"
                          "--redirect" "https://docs.racket-lang.org/local-redirect/"
                          "--redirect-main" "https://docs.racket-lang.org"
                          "++xref-in" "setup/xref" "load-collections-xref"
                          (path->string path))])
    (dynamic-require 'scribble/run #f))
  ;; Move any .PNG or .GIF or .SVG files from dir to img-dir
  (define (image-file? p)
    (match (path->string p)
      [(pregexp "(?i:\\.((png)|(gif)|(svg)))$") #t]
      [_ #f]))
  (for ([from (in-list (find-files image-file? dir))])
    (define-values (base name _) (split-path from))
    (define to (build-path img-dir name))
    (copy-file* from to #t))
  ;; Extract the part we care about -- the elements in the "main" div
  ;; after the "versionbox" div.  (The `match` might be too fragile
  ;; way to do this.)
  (match (~> (build-path dir "frog.html")
             (with-input-from-file read-html-as-xexprs)
             cadr)
    [`(html
       ()
       (head . ,_)
       ,(list-no-order
         `(div ([class "maincolumn"])
               (div ([class "main"])
                    . ,xs))
         _ ...))
     (adjust-scribble-html xs img-uri)]
    [x
     (displayln "Bad Scribble post:")
     (pretty-print x)
     (values '() #hash())]))

(define (adjust-scribble-html xs img-uri)
  ;; (-> (listof xexpr?) string? hash? (values (listof xexpr?) hash?))
  (define meta-h #hash()) ;; mutated
  (define xs*
    (xexpr-map*
     (lambda (x _)
       (match x
         ;; Delete version
         [`(div ([class "versionbox"]) . ,_) '()]
         ;; Override author if @author was used and delete element
         [`(div ([class "SAuthorListBox"]) . ,_)
          (match x
            [`(div ([class "SAuthorListBox"])
               (span ([class "SAuthorList"]) . ,_)
               ,@(list `(p ([class "author"]) ,(? string? authors))
                       ...))
             (set! meta-h (hash-set meta-h "Authors" (string-join authors ", ")))]
            [_ (void)])
          '()]
         ;; Override title if @title was used and delete element
         [`(h2 . ,_)
          (match x
            [`(h2 () (a . ,_) ,@title-xs)
             (set! meta-h (hash-set meta-h "Title" (xexpr->markdown `(span () ,@title-xs))))]
            [_ (void)])
          '()]
         ;; Convert blockquotes (???)
         [`(blockquote ([class "SCodeFlow"]) . ,xs)
          `[(div ([class "SCodeFlow"]) ,@xs)]]
         ;; Adjust image source URLs
         [`(img ,(list-no-order `[src ,src] x ...))
          `[(img ([src ,(~a img-uri "/" src)] ,@x))]]
         ;; Adjust headers:
         ;; Scribble @title is rendered as <h2>, @section as <h3>,
         ;; and @subsection as <h4>, and so on. Hoist the headings up
         ;; to be consistent with the Markdown format sources.
         ;; (that is, @section as <h2>, @subsection as <h3>, etc).
         [`(h3 . ,x) `[(h2 ,@x)]]
         [`(h4 . ,x) `[(h3 ,@x)]]
         [`(h5 . ,x) `[(h4 ,@x)]]
         [`(h6 . ,x) `[(h5 ,@x)]]
         [`(p () "<" "!" ndash " more " ndash ">") `[(!HTML-COMMENT () "more")]]
         [x (list x)]))
     xs))
  (values xs* meta-h))

(module+ test
  (let ([path (make-temporary-file)]
        [s #<<EOF
#lang scribble/manual
@title{The Post's Title}
@section{Section 1}
Here is some text.

<!-- more -->

Below the fold.
EOF
])
    (with-output-to-file path #:exists 'replace (λ () (display s)))
    (check-equal?
     (read-scribble-file path
                         #:img-local-path (find-system-path 'temp-dir)
                         #:img-uri-prefix "/")
     '((h1 () (a ((name "(part._.The_.Post_s_.Title)"))) "The Post" rsquo "s Title")
      (h1 () "1" (tt () nbsp) (a ((name "(part._.Section_1)"))) "Section 1")
      (p () "Here is some text.")
      (!HTML-COMMENT () "more")
      (p () "Below the fold.")))
    (delete-file path))
  ;; regression test for https://github.com/greghendershott/frog/issues/75
  (let ([path (make-temporary-file)]
        [s #<<EOF
#lang scribble/manual
@hyperlink["https://aur.archlinux.org/packages/?SeB=m&K=bluephoenix47" "Aur"]
EOF
])
    (with-output-to-file path #:exists 'replace (λ () (display s)))
    (check-equal?
     (read-scribble-file path
                         #:img-local-path (find-system-path 'temp-dir)
                         #:img-uri-prefix "/")
     '((p ()
          (a ((href "https://aur.archlinux.org/packages/?SeB=m&K=bluephoenix47")) "Aur"))))
    (delete-file path)))
