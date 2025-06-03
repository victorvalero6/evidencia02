#lang racket 
(require "unidades.rkt")
(require "parser.rkt")


;(displayln (fahrenheit 89)) ; prueba F to C
;(displayln (celcius 33)) ; prueba C to F
;(displayln listaValorItems) 
;(displayln (hash-ref listaValorItems "fresh rosemary")) 
;(displayln listaValorItems) 

(define secciones (parse-recipe "data/Chimichurri Sauce.txt"))
(for-each displayln secciones)


(displayln(parsear-cantidad "2 3/4 cups sugar"))
(displayln(parsear-unidad "2 3/4 cups sugar"))
