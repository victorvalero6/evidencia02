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
        "large eggs" 50
        "canola oil" 220
        "water" 240
        "vanilla" 13
        "extra-virgin olive oil" 215
        "white wine vinegar" 240
        "minced garlic" 130
        "driedoregano" 16
        "redpepperflakes" 48
        "smoked paprika" 92
        "finely chopped fresh flat-leaf parsley" 60))