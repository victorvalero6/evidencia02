#lang racket
(require "unidades.rkt")
(require "parser.rkt")

(define inicioSecuencial(current-inexact-milliseconds))

;Para utilizar las 5 recetas originales, utilice esta lista:
;vvv
(define lista-recetas
  (list
   "data/Best Homemade Brownies-1.txt"
   "data/Chimichurri Sauce.txt"
   "data/Fettuccine Alfredo.txt"
   "data/Lemon Cake-1.txt"
   "data/Pan-Seared Steak with Garlic Butter.txt"))

;Para hacer la lectura de las 100 recetas, favor de clonar el repositorio de github para tener las
;100 recetas, despues descomentarla.  vvvv

;(define lista-recetas
;  (for/list ([i (in-range 1 101)])
;    (string-append "data/Receta" (number->string i) ".txt")))

(define opciones (make-hash (parser-optionstxt "/Users/victorvalero/Desktop/code📂/evidencia02/options.txt")))
(define escala (string->number (hash-ref opciones "porciones" "1")))
(define palabra-filtro (hash-ref opciones "filtra" #f))
(define sistema (hash-ref opciones "sistema" "metric")) ; metric o cup
(define temp (hash-ref opciones "temp" "C")) ; C o F
(define mostrar-calorias (equal? (hash-ref opciones "show-calories" "false") "true"))

;; Procesamos cada receta
(for-each
 (lambda (ruta)
   ;; Sacar el nombre del archivo sin la ruta
   (define nombre-base
     (string-replace (last (string-split ruta "/")) ".txt" ""))
   (define salida
     (open-output-file
      (string-append "recetasSalidaSecuencial/" nombre-base ".html")
      #:exists 'replace))

   ;; Encabezado HTML
   (fprintf salida "<!DOCTYPE html>\n<html>\n<head>\n<meta charset='UTF-8'>\n<title>~a</title>\n<link rel='stylesheet' href='style.css'>\n</head>\n<body>\n" nombre-base)

   ;; Receta
   (fprintf salida "<hr><h1>Receta: ~a</h1>\n" ruta)

   (define receta-original (parse-recipe ruta))
   (define receta-filtrada
     (if (and palabra-filtro (not (equal? palabra-filtro "all")))
         (filtrar-receta receta-original palabra-filtro "t")
         receta-original))

   (define receta-escalada
     (map (lambda (linea) (escalar-linea escala linea)) receta-filtrada))

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

   (fprintf salida "<h2>Ingredientes</h2>\n<ul>\n")
   (for-each
    (lambda (linea)
      (fprintf salida "<li>~a</li>\n" (format "~a" linea)))
    receta-final)
   (fprintf salida "</ul>\n")

   (fprintf salida "<h2>Instrucciones</h2>\n")
   (define instrucciones (parse-recipe-instructions ruta))
   (for-each
    (lambda (linea)
      (fprintf salida "<p>~a</p>\n"
               (if (equal? temp "C")
                   (convertir-temperaturas linea)
                   linea)))
    instrucciones)

   (define extra (parse-recipe-extra ruta))
   (unless (null? extra)
     (fprintf salida "<h2>Extra:</h2>\n")
     (for-each
      (lambda (linea)
        (fprintf salida "<p>~a</p>\n" linea))
      extra))

   (when mostrar-calorias
     (define total (calorias-totales receta-escalada))
     (define porciones (parse-recipe-porciones ruta))
     (define por-porcion (calorias-por-porcion receta-escalada porciones))
     (fprintf salida "<h3>Calorías totales: ~a</h3>\n" total)
     (fprintf salida "<h3>Calorías por porción (~a): ~a</h3>\n" porciones por-porcion))

   (fprintf salida "</body>\n</html>")
   (close-output-port salida))
 lista-recetas)

(define finSecuencial (current-inexact-milliseconds))




;------INICIO PARALELIZACION------------


(define inicioParalelo (current-inexact-milliseconds))

(define (procesar-receta ruta)
  (define nombre-base
    (string-replace (last (string-split ruta "/")) ".txt" ""))
  (define salida
    (open-output-file
     (string-append "recetasSalidaParalelo/" nombre-base ".html")
     #:exists 'replace))

  (fprintf salida "<!DOCTYPE html>\n<html>\n<head>\n<meta charset='UTF-8'>\n<title>~a</title>\n<link rel='stylesheet' href='style1.css'>\n</head>\n<body>\n" nombre-base)
  (fprintf salida "<hr><h1>Receta: ~a</h1>\n" ruta)

  (define receta-original (parse-recipe ruta))
  (define receta-filtrada
    (if (and palabra-filtro (not (equal? palabra-filtro "all")))
        (filtrar-receta receta-original palabra-filtro "t")
        receta-original))
  (define receta-escalada
    (map (lambda (linea) (escalar-linea escala linea)) receta-filtrada))
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

  (fprintf salida "<h2>Ingredientes</h2>\n<ul>\n")
  (for-each
   (lambda (linea)
     (fprintf salida "<li>~a</li>\n" (format "~a" linea)))
   receta-final)
  (fprintf salida "</ul>\n")

  (fprintf salida "<h2>Instrucciones</h2>\n")
  (define instrucciones (parse-recipe-instructions ruta))
  (for-each
   (lambda (linea)
     (fprintf salida "<p>~a</p>\n"
              (if (equal? temp "C")
                  (convertir-temperaturas linea)
                  linea)))
   instrucciones)

  (define extra (parse-recipe-extra ruta))
  (unless (null? extra)
    (fprintf salida "<h2>Extra:</h2>\n")
    (for-each
     (lambda (linea)
       (fprintf salida "<p>~a</p>\n" linea))
     extra))

  (when mostrar-calorias
    (define total (calorias-totales receta-escalada))
    (define porciones (parse-recipe-porciones ruta))
    (define por-porcion (calorias-por-porcion receta-escalada porciones))
    (fprintf salida "<h3>Calorías totales: ~a</h3>\n" total)
    (fprintf salida "<h3>Calorías por porción (~a): ~a</h3>\n" porciones por-porcion))

  (fprintf salida "</body>\n</html>")
  (close-output-port salida))

;; Crear un thread por receta
(define hilos
  (map (lambda (ruta)
         (thread (lambda () (procesar-receta ruta))))
       lista-recetas))

;; Esperar a que todos los hilos terminen
(for-each thread-wait hilos)

(define finParalelo (current-inexact-milliseconds))

;; Medición de tiempo
(define duracion-total-paralelo (- finParalelo inicioParalelo))

(define duracion-total-secuencial(- finSecuencial inicioSecuencial))
;; Speedup y eficiencia
(define num-hilos (length lista-recetas)) ; En este caso, 100
(define speedup (/ duracion-total-secuencial duracion-total-paralelo))
(define eficiencia (/ speedup num-hilos))


(define resultados (open-output-file "resultados.txt" #:exists 'replace))
(fprintf resultados "Duración secuencial (ms): ~a\n" duracion-total-secuencial)
(fprintf resultados "Duración paralela (ms): ~a\n" duracion-total-paralelo)
(fprintf resultados "Speedup: ~a\n" speedup)
(fprintf resultados "Eficiencia: ~a\n" eficiencia)
(close-output-port resultados)






