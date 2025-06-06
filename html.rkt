#lang racket
(provide generar-html)

;; Leer el contenido original de la receta
(define (leer-archivo-como-texto ruta-receta)
  (define in (open-input-file ruta-receta))
  (define contenido (port->string in))
  (close-input-port in)
  contenido)

;; Mostrar línea de ingredientes sin paréntesis
(define (imprimir-ingrediente-html salida linea)
  (define cantidad (first linea))
  (define unidad (second linea))
  (define nombre (third linea))  ; Changed from ingrediente to nombre
  (fprintf salida "<li>~a ~a ~a</li>\n" cantidad unidad nombre))

;; Generar HTML final
(define (generar-html ruta-receta nombre-html receta-final instrucciones)
  (define original (leer-archivo-como-texto ruta-receta))
  (define salida (open-output-file nombre-html #:exists 'replace))

  ;; Encabezado del HTML
  (fprintf salida "<!DOCTYPE html>\n<html>\n<head>\n<meta charset='UTF-8'>\n")
  (fprintf salida "<title>~a</title>\n" (last (string-split ruta-receta "/")))
  (fprintf salida "<link rel='stylesheet' href='style.css'>\n")
  (fprintf salida "</head>\n<body>\n")

  ;; Nombre de la receta
  (fprintf salida "<h1>~a</h1>\n"
           (string-replace (last (string-split ruta-receta "/")) ".txt" ""))

  ;; Ingredientes transformados
  (fprintf salida "<h2>Ingredientes</h2>\n<ul>\n")
  (for-each
   (lambda (linea)
     (fprintf salida "<li>~a ~a ~a ~a ~a</li>\n"
              (first linea)   ; cantidad
              (second linea)  ; unidad (gr/cups)
              (third linea)   ; nombre
              (fourth linea)  ; calorias
              (fifth linea))) ; "Cal"
   receta-final)
  (fprintf salida "</ul>\n")

  ;; Instrucciones
  (fprintf salida "<h2>Instrucciones</h2>\n")
  (for-each
   (lambda (linea)
     (fprintf salida "<p>~a</p>\n" linea))
   instrucciones)

  ;; Cierre
  (fprintf salida "</body>\n</html>")
  (close-output-port salida))
