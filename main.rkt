#lang racket
(require "unidades.rkt")
(require "parser.rkt")
(require "html.rkt")

;; Rutas
;data/Best Homemade Brownies-1.txt
;data/Chimichurri Sauce.txt
;data/Fettuccine Alfredo.txt
;data/Lemon Cake-1.txt
;data/Pan-Seared Steak with Garlic Butter.txt


(define ruta-receta "data/Lemon Cake-1.txt")
(define opciones (make-hash (parser-optionstxt "/Users/victorvalero/Desktop/code📂/evidencia02/options.txt")))

;; Leer opciones
(define escala (string->number (hash-ref opciones "porciones" "1")))
(define palabra-filtro (hash-ref opciones "filtra" #f))
(define sistema (hash-ref opciones "sistema" "metric")) ; metric o cup
(define temp (hash-ref opciones "temp" "C")) ; C o F
  (define mostrar-calorias (equal? (hash-ref opciones "show-calories" "false") "true"))
  
;; Cargar y filtrar receta
(define receta-original (parse-recipe ruta-receta))
(define receta-filtrada
(if (and palabra-filtro (not (equal? palabra-filtro "all")))
      (filtrar-receta receta-original palabra-filtro "t")
      receta-original))

;; Escalar receta
(define receta-escalada
 (map (lambda (linea) (escalar-linea escala linea)) receta-filtrada))

;; Mostrar receta final (unidades en gramos o tazas)
(displayln "----- Receta final -----")
(for-each
 (lambda (linea)
   (define cantidad (first linea))
   (define unidad (second linea))
   (define ingrediente (third linea))
   (define nombre (string-join ingrediente " "))
   (cond
     [(equal? sistema "metric")
      (define gramos (t->gr cantidad unidad ingrediente))
      (displayln (list gramos 'gr nombre))]
    [else
     (define tazas (gr->t cantidad unidad ingrediente))
     (displayln (list tazas 'cups nombre))]))

  
receta-escalada)

;Mostrar instrucciones con temperatura convertida si aplica
(displayln "----- Instrucciones -----")
(define instrucciones (parse-recipe-instructions ruta-receta))
(for-each
 (lambda (linea)
   (cond
     [(equal? temp "C") (displayln (convertir-temperaturas linea))]
     [else (displayln linea)]))
 instrucciones)

;; Mostrar calorías si aplica
(when mostrar-calorias
  (define total (calorias-totales receta-escalada))
  (define porciones (parse-recipe-porciones ruta-receta))
  (define por-porcion (calorias-por-porcion receta-escalada porciones))
  (newline)
  (displayln (string-append "Calorías totales: " (number->string total)))
  (displayln (string-append "Calorías por porción (" (number->string porciones) "): " (number->string por-porcion)))
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
   receta-escalada))

(generar-html ruta-receta "Receta00.html" receta-escalada instrucciones)