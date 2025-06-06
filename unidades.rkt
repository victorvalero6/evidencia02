#lang racket

(provide fahrenheit)
(provide celcius)
(provide listaValorItems)
(provide gr->t)
(provide t->gr)
(provide calorias-totales)
(provide listaCaloriasItem)
(provide CaloriasIngrediente)
(provide calorias-por-porcion)
(provide escalar-linea)
(provide escalar-receta)
(provide filtrar-receta)
;; definicion de constantes F a C
(define F->C 0.55555)
(define C->F 1.8)
(define tempint 32)

;; Funciones F a C & Viceversa
(define (fahrenheit f)
  (* (- f tempint) F->C))

(define (celcius c)
  (+ (* c C->F) 32))

;;Conversiones de gramos a tazas
;;Debemos bucar una estructura de datos
;;Primero para definir por cada ingrediente cuantos gramos son por taza
;;Con eso podriamos empezar la transformacion de gramos a taza o de taza a gramos

;;Lista doble nombre-valor
(define listaValorItems
  (hash
   "granulated sugar" 200
   "flour" 120
   "cocoa powder, sifted" 89
   "powdered sugar, sifted" 120
   "dark chocolate chips" 170
   "sea salt" 288
   "large egg" 50
   "eggs" 200
   "canola oil" 220
   "water" 240
   "vanilla" 13
   "extra-virgin olive oil" 215
   "white wine vinegar" 240
   "minced garlic" 130
   "dried oregano" 16
   "red pepper flakes" 48
   "smoked paprika" 92
   "fresh parsley" 60
   "dry fettuccine pasta" 100
   "butter" 227
   "heavy cream" 240
   "salt" 288
   "black pepper" 96
   "garlic salt" 288
   "grated romano cheese" 80
   "grated parmesan cheese" 100
   "all-purpose flour" 120
   "almond flour" 96
   "baking powder" 192
   "kosher salt" 288
   "extra-virgin olive oil" 215
   "grabulated sugar" 200
   "fresh lemon juice" 240
   "lemon zest" 2
   "new york strip steak" 454
   "vegetable oil" 220
   "garlic clove" 5
   "fresh rosemary" 1))

;;Funcion Gramos a Tazas
;;dividir el total de gramos por taza entre los gramos que se presentan
;;(/ gramos-total gramos-por-taza)

(define (gr->t cantidad unidad ingrediente)
  (define nombre (string-join ingrediente " "))
  (define gramos-por-taza (hash-ref listaValorItems nombre #f))
  (cond
    ;cup cups

    [(and gramos-por-taza (or (string-ci=? unidad "cup")(string-ci=? unidad "cups")))
     (/ cantidad gramos-por-taza)]

    ;1 taza= 16 tablespoon
    [(and gramos-por-taza(or (string-ci=? unidad "tablespoon")(string-ci=? unidad "tablespoons")))
     (/ cantidad (/ gramos-por-taza 16))]

    ;1 taza = 48 teaspoon
    [(and gramos-por-taza(or(string-ci=? unidad "teaspoon")(string-ci=? unidad "teaspoons")))
     (/ cantidad (/ gramos-por-taza 48))]

    [else
     cantidad]))

;;Funcion de Tazas a Gramos
;;Si el archivo dice dos tazas, * valor numeroDeTazas
;;(* número-de-tazas gramos-por-taza)

(define (t->gr cantidad unidad ingrediente)
  (define nombre (string-join ingrediente " "))
  (define gramos-por-taza (hash-ref listaValorItems nombre #f))
  (cond
    ;cup cups
    [(and gramos-por-taza (or (string-ci=? unidad "cup")(string-ci=? unidad "cups")))
     (* cantidad gramos-por-taza)]


    ;1 taza= 16 tablespoon
    [(and gramos-por-taza(or (string-ci=? unidad "tablespoon")(string-ci=? unidad "tablespoons")))
     (/ (* cantidad gramos-por-taza)16)]

    ;1 taza = 48 teaspoon
    [(and gramos-por-taza(or(string-ci=? unidad "teaspoon")(string-ci=? unidad "teaspoons")))
     (/ (* cantidad gramos-por-taza)48)]

    [else
     cantidad]))

;; CALORIAS POR PORCION - POR 100G -> CALORIAS

(define listaCaloriasItem
  (hash
   "granulated sugar" 387
   "flour" 364
   "cocoa powder, sifted" 228
   "powdered sugar, sifted" 389
   "dark chocolate chips" 546
   "sea salt" 0
   "large egg" 143
   "canola oil" 884
   "water" 0
   "vanilla" 228
   "extra-virgin olive oil" 884
   "white wine vinegar" 17
   "minced garlic" 149
   "dried oregano" 265
   "red pepper flakes" 318
   "smoked paprika" 282
   "fresh parsley" 36
   "dry fettuccine pasta" 371
   "butter" 717
   "heavy cream" 340
   "salt" 0
   "black pepper" 251
   "garlic salt" 0
   "grated romano cheese" 387
   "grated parmesan cheese" 431
   "all-purpose flour" 364
   "almond flour" 571
   "baking powder" 53
   "kosher salt" 0
   "extra-virgin olive oil" 884
   "grabulated sugar" 387
   "fresh lemon juice" 22
   "lemon zest" 47
   "new york strip steak" 454
   "vegetable oil" 884
   "garlic clove" 149
   "fresh rosemary" 131))

;formula calorias poe ingrediente
(define (CaloriasIngrediente calorias100g gramos)
  (/ (* calorias100g gramos)100))

;funcion calorias totales
(define(calorias-totales lista-receta)
  (foldl
   (lambda (linea suma)
     (define-values(cantidad unidad ingrediente) (apply values linea))
     (define nombre(string-join ingrediente " "))
     (define gramos(t->gr cantidad unidad ingrediente))
     (define cantidad100g(hash-ref listaCaloriasItem nombre 0))
     (define calorias(CaloriasIngrediente cantidad100g gramos))
     (define sumaRound(+ suma calorias))
     (/ (round (* sumaRound 100.0)) 100.0))
   0
   lista-receta))

;calorias por porcion

(define (calorias-por-porcion receta porcion)
  (define total(calorias-totales receta))
  (/ total porcion))


;ESCALAR PORCION "FACTOR"

(define (escalar-linea factor linea)
  (define cantidad(first linea))
  (define unidad(second linea))
  (define ingrediente(third linea))
  (list(* factor cantidad)unidad ingrediente))

(define(escalar-receta receta factor)
  (map(lambda(linea)
        (escalar-linea(factor linea)))
      receta))

;FILTRAR INGREDIENTES

(define (filtrar-receta receta palabra incluir?)
  (filter
   (lambda (linea)
     (define ingrediente (third linea))
     (define nombre (string-join ingrediente " "))
     (define contiene? (string-contains? (string-downcase nombre)
                                         (string-downcase palabra))) ; match parcial
     (if incluir? contiene? (not contiene?)))
   receta))



