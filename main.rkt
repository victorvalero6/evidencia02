#lang racket
(require "unidades.rkt")
(require "parser.rkt")

;se define "receta"
(define receta (parse-recipe "/Users/victorvalero/Desktop/code📂/evidencia02/data/Best Homemade Brownies-1.txt"))
(for-each displayln receta)

(newline)
(displayln "-----")
(newline)

;funciones gr a t y t a gr
(for-each
 (lambda (linea)
   (define cantidad (first linea))
   (define unidad (second linea))
   (define ingrediente (third linea))

   (define conversion (gr->t cantidad unidad ingrediente))

   (displayln (list conversion 'cups ingrediente)))
 receta)

(newline)
(displayln "-----")
(newline)

(for-each
 (lambda (linea)
   (define cantidad (first linea))
   (define unidad (second linea))
   (define ingrediente (third linea))

   (define conversion (t->gr cantidad unidad ingrediente))

   (displayln (list conversion 'gr ingrediente)))
 receta)

(newline)
(displayln "-----")
(newline)
;temperatura
(define instrucciones (parse-recipe-instructions "/Users/victorvalero/Desktop/code📂/evidencia02/data/Best Homemade Brownies-1.txt"))
(for-each
 (lambda (linea)
   (displayln (convertir-temperaturas linea)))
 instrucciones)

(newline)
(displayln "-----")
(newline)
;calorias totales
(define porciones (parse-recipe-porciones "/Users/victorvalero/Desktop/code📂/evidencia02/data/Lemon Cake-1.txt"))
(displayln porciones)

;(displayln (calorias-totales receta))
;(displayln (calorias-por-porcion receta))

;; Mostrar calorías totales
(define total-calorias (calorias-totales receta))
(displayln (string-append "Calorías totales: " (number->string total-calorias)))

;; Mostrar calorías por porción
(define calorias-porcion (calorias-por-porcion receta porciones))
(displayln (string-append "Calorías por porción (" (number->string porciones) "): " (number->string calorias-porcion)))


(newline)
(displayln "-----")
(newline)

(newline)
(displayln "Calorías por ingrediente:")
(for-each
 (lambda (linea)
   (define-values (cantidad unidad ingrediente) (apply values linea))
   (define nombre (string-join ingrediente " "))
   (define gramos (t->gr cantidad unidad ingrediente))
   (define calorias100g (hash-ref listaCaloriasItem nombre 0))
   (define calorias (CaloriasIngrediente calorias100g gramos))
   (displayln (list nombre calorias "Cal")))
 receta)

(newline)
(displayln "-----")
(newline)


(define receta-escalada (map (lambda (linea) (escalar-linea 2 linea)) receta))

(for-each displayln receta-escalada)
(newline)
(displayln "-----")
(newline)

(define nueva-receta (filtrar-receta receta "sugar" #t))
(for-each displayln nueva-receta)

(newline)
(displayln "-----")
(newline)

(define options(parser-optionstxt "/Users/victorvalero/Desktop/code📂/evidencia02/options.txt"))
(display options)