#lang racket/base

(require racket/string
         xml
         data/collection
         "var.rkt")

(provide svg svg-cells svg-show)

(module+ test
  (require rackunit))

(define (svg-show xexpr)
  (display-xml/content (xexpr->xml xexpr) #:indentation 'scan))

(define (svg width height body)
  (var w (format "~a" width)
       h (format "~a" height))
  `(svg ((version "1.1")
         (width ,w) (height ,h)
         (xmlns "http://www.w3.org/2000/svg"))
        ,@body))

(module+ test
  (check-equal? (svg 200 "200" '())
                '(svg ((version "1.1")
                       (width "200") (height "200")
                       (xmlns "http://www.w3.org/2000/svg")))))

(define background '(rect ((x "0") (y "0") (width "10") (height "10") (fill "lightblue") (stroke "transparent"))))
(define top-left-corner "M 0 1 h 1 v -1")
(define top-right-corner "M 10 1 h -1 v -1")
(define bottom-left-corner "M 0 9 h 1 v 1")
(define bottom-right-corner "M 10 9 h -1 v 1")
(define top-line "M 0 1 h 10")
(define left-line "M 1 0 v 10")
(define bottom-line "M 0 9 h 10")
(define right-line "M 9 0 v 10")

(define (path-combine . paths)
  `(path ((d ,(string-join paths)))))

(define svg-cells (vector
               (path-combine top-left-corner top-right-corner bottom-left-corner bottom-right-corner)
               (path-combine top-line bottom-left-corner bottom-right-corner)
               (path-combine left-line top-right-corner bottom-right-corner)
               (path-combine "M 1 10 v -9 h 9" bottom-right-corner)
               (path-combine bottom-line top-left-corner top-right-corner)
               (path-combine bottom-line top-line)
               (path-combine "M 1 0 v 9 h 9" top-right-corner)
               (path-combine "M 10 1 h -9 v 8 h 9")
               (path-combine right-line top-left-corner bottom-left-corner)
               (path-combine "M 0 1 h 9 v 9" bottom-left-corner)
               (path-combine left-line right-line)
               (path-combine "M 1 10 v -9 h 8 v 9")
               (path-combine "M 0 9 h 9 v -9" top-left-corner)
               (path-combine "M 0 1 h 9 v 8 h -9")
               (path-combine "M 1 0 v 9 h 8 v -9")
               (path-combine "M 1 1 h 8 v 8 h -8 z")))
 
(define cell-catalog
  (svg 40 (* 48 16)
       `((style "path {fill: none; stroke: green; stroke-width: 1; stroke-linejoin: round}")
         (g ((transform "scale(4, 4)")) 
            ,@(for/list ([i (naturals)]
                         [shape svg-cells])
                (var t (format "translate(0, ~a)" (* 12 i))
                     number `(text ((x "5")
                                    (y "5.5")
                                    (dominant-baseline "middle")
                                    (text-anchor "middle")
                                    (font-size "5")
                                    (font-family "Monospace"))
                                   ,(number->string i)))
                `(g ((transform ,t)) ,background ,shape ,number))))))

; (with-output-to-file "maze-cell-catalog.svg" (λ () (svg-show cell-catalog)) #:exists 'replace)



