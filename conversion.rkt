#lang racket

(provide fahrenheit celcius)
(provide listaValorItems)
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
        "cocoa powder" 89
        "powdered sugar" 120 
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
        "fresh rosemary"))