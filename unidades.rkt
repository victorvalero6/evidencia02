#lang racket

(provide fahrenheit)
(provide celcius)
(provide listaValorItems)
(provide gr->t)
(provide t->gr)


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