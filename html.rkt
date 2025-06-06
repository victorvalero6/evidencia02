#lang racket
(provide generar-html)

;; Leer el contenido original de la receta
(define (leer-archivo-como-texto ruta)
  (define in (open-input-file ruta))
  (define contenido (port->string in))
  (close-input-port in)
  contenido)

;; Mostrar línea de ingredientes sin paréntesis
(define (imprimir-ingrediente-html salida linea)
  (define cantidad (first linea))
  (define unidad (second linea))
  (define ingrediente (string-join (third linea) " "))
  (fprintf salida "<li>~a ~a ~a</li>\n" cantidad unidad ingrediente))

;; Generar HTML final
(define (generar-html ruta-receta nombre-html lista-transformada instrucciones)
  (define original (leer-archivo-como-texto ruta-receta))
  (define salida (open-output-file nombre-html #:exists 'replace))

  ;; Encabezado del HTML
  (fprintf salida "<!DOCTYPE html>\n<html>\n<head>\n<meta charset='UTF-8'>\n")
  (fprintf salida "<title>Receta</title>\n")
  (fprintf salida "<link rel='stylesheet' href='style.css'>\n")
  (fprintf salida "</head>\n<body>\n")


  ;; Ingredientes transformados
  (fprintf salida "<h2>Ingredientes Transformados</h2>\n<ul>\n")
  (for-each
   (lambda (linea)
     (imprimir-ingrediente-html salida linea)) lista-transformada)
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
