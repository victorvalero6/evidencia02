#lang racket
(require "unidades.rkt")
(require "parser.rkt")


;(displayln (fahrenheit 89)) ; prueba F to C
;(displayln (celcius 33)) ; prueba C to F
;(displayln listaValorItems)
;(displayln (hash-ref listaValorItems "fresh rosemary"))
;(displayln listaValorItems)

(define receta (parse-recipe "/Users/victorvalero/Desktop/code📂/evidencia02/data/Best Homemade Brownies-1.txt"))
(for-each displayln receta)

(displayln "-----")

(for-each
 (lambda (linea)
   (define cantidad (first linea))
   (define unidad (second linea))
   (define ingrediente (third linea))

   (define conversion (gr->t cantidad unidad ingrediente))

   (displayln (list conversion 'cups ingrediente)))
 receta)

(displayln "-----")

(for-each
 (lambda (linea)
   (define cantidad (first linea))
   (define unidad (second linea))
   (define ingrediente (third linea))

   (define conversion (t->gr cantidad unidad ingrediente))

   (displayln (list conversion 'gr ingrediente)))
 receta)

(displayln "-----")

(define instrucciones (parse-recipe-instructions "/Users/victorvalero/Desktop/code📂/evidencia02/data/Best Homemade Brownies-1.txt"))
(for-each
 (lambda (linea)
   (displayln (convertir-temperaturas linea)))
 instrucciones)



;(displayln(parsear-cantidad "2 3/4 cups sugar"))
;(displayln(parsear-unidad "2 3/4 cups sugar"))
;(displayln(parsear-ingrediente "2 3/4 cups granulated sugar"))
;(displayln(linea-completa "2 3/4 cups granulated sugar"))

;; Define solo-instrucciones or require it from another file
;; Example definition (replace with actual data as needed):
