#lang racket 
(require "unidades.rkt")
(require "parser.rkt")


;(displayln (fahrenheit 89)) ; prueba F to C
;(displayln (celcius 33)) ; prueba C to F
;(displayln listaValorItems) 
;(displayln (hash-ref listaValorItems "fresh rosemary")) 
;(displayln listaValorItems) 

(define secciones (parse-recipe "evidencia02/data/Best Homemade Brownies-1.txt"))
(for-each displayln secciones)


(parsear-linea "1/2 teaspoon sea salt")
