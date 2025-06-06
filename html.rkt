#lang racket
(provide generar-html)

(define (leer-archivo-como-texto ruta)
  (define in (open-input-file ruta))
  (define contenido (port->string in))
  (close-input-port in)
  contenido)

(define (generar-html ruta-receta nombre-html lista-transformada instrucciones)
  (define original (leer-archivo-como-texto ruta-receta))
  (define salida (open-output-file nombre-html #:exists 'replace))

  (fprintf salida "<!DOCTYPE html>\n<html>\n<head>\n<meta charset='UTF-8'>\n<title>Receta</title>\n<link rel='stylesheet' href='style.css'>\n</head>\n<body>\n")

  ;receta original
  (fprintf salida "<h2>Ingredientes Transdormados: <h2>\n<ul>\n")
  (for-each
   (lambda (linea)
     (fprintf salida "<li>~a</li>\n" (format "~a" linea)))
   lista-transformada)
  (fprintf salida "</ul>\n")

  (fprintf salida "<h2>Instrucciones: <h2>")
  (for-each
   (lambda (linea)
     (fprintf salida "<h3>~a</h3>\n" (format "~a" linea)))
   instrucciones)
  (fprintf salida "<h3>\n")

  (fprintf salida "<h2>Instrucciones: <h2>")


  ;salida
  (close-output-port salida))