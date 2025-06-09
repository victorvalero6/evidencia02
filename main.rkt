#lang racket
(require "unidades.rkt")
(require "parser.rkt")

(define inicio(current-inexact-milliseconds))

(define lista-recetas
  (for/list ([i (in-range 1 101)])
    (string-append "data/Receta" (number->string i) ".txt")))

(define opciones (make-hash (parser-optionstxt "/Users/victorvalero/Desktop/code📂/evidencia02/options.txt")))
(define escala (string->number (hash-ref opciones "porciones" "1")))
(define palabra-filtro (hash-ref opciones "filtra" #f))
(define sistema (hash-ref opciones "sistema" "metric")) ; metric o cup
(define temp (hash-ref opciones "temp" "C")) ; C o F
(define mostrar-calorias (equal? (hash-ref opciones "show-calories" "false") "true"))

;; Abrimos el archivo de salida una sola vez
(define salida (open-output-file "Receta00.html" #:exists 'replace))

;; Encabezado HTML general
(fprintf salida "<!DOCTYPE html>\n<html>\n<head>\n<meta charset='UTF-8'>\n<title>Recetas</title>\n<link rel='stylesheet' href='style.css'>\n</head>\n<body>\n")

;; Procesamos cada receta
(for-each
 (lambda (ruta)
   (fprintf salida "<hr><h1>Receta: ~a</h1>\n" ruta)

   ;; Cargar y filtrar receta
   (define receta-original (parse-recipe ruta))
   (define receta-filtrada
     (if (and palabra-filtro (not (equal? palabra-filtro "all")))
         (filtrar-receta receta-original palabra-filtro "t")
         receta-original))

   ;; Escalar
   (define receta-escalada
     (map (lambda (linea) (escalar-linea escala linea)) receta-filtrada))

   ;; Receta final transformada
   (define receta-final
     (map
      (lambda (linea)
        (define cantidad (first linea))
        (define unidad (second linea))
        (define ingrediente (third linea))
        (define nombre (string-join ingrediente " "))
        (define gramos (t->gr cantidad unidad ingrediente))
        (define cal100 (hash-ref listaCaloriasItem nombre 0))
        (define cal (CaloriasIngrediente cal100 gramos))
        (if (equal? sistema "metric")
            (list gramos 'gr nombre cal "Cal")
            (let ([tazas (gr->t cantidad unidad ingrediente)])
              (list tazas 'cups nombre cal "Cal"))))
      receta-escalada))

   ;; Mostrar ingredientes
   (fprintf salida "<h2>Ingredientes</h2>\n<ul>\n")
   (for-each
    (lambda (linea)
      (fprintf salida "<li>~a</li>\n" (format "~a" linea)))
    receta-final)
   (fprintf salida "</ul>\n")

   ;; Mostrar instrucciones
   (fprintf salida "<h2>Instrucciones</h2>\n")
   (define instrucciones (parse-recipe-instructions ruta))
   (for-each
    (lambda (linea)
      (fprintf salida "<p>~a</p>\n"
               (if (equal? temp "C")
                   (convertir-temperaturas linea)
                   linea)))
    instrucciones)

   ;; Mostrar sección "Extra" si existe
   (define extra (parse-recipe-extra ruta))
   (unless (null? extra)
     (fprintf salida "<h2>Extra:</h2>\n")
     (for-each
      (lambda (linea)
        (fprintf salida "<p>~a</p>\n" linea))
      extra))

   ;; Calorías
   (when mostrar-calorias
     (define total (calorias-totales receta-escalada))
     (define porciones (parse-recipe-porciones ruta))
     (define por-porcion (calorias-por-porcion receta-escalada porciones))
     (fprintf salida "<h3>Calorías totales: ~a</h3>\n" total)
     (fprintf salida "<h3>Calorías por porción (~a): ~a</h3>\n" porciones por-porcion)))
 lista-recetas)

(define fin(current-inexact-milliseconds))
(define duracion-total-secuencial(- fin inicio))
(displayln(string-append "Duracion total secuencial (ms): " (number->string duracion-total-secuencial)))




;; Cierre del HTML
(fprintf salida "</body>\n</html>")
(close-output-port salida)
