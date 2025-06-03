#lang racket
(require racket/list)
(provide parse-recipe)
(provide parsear-cantidad)
(provide parsear-unidad)
(provide parsear-ingrediente)
(provide linea-completa)


(define (dropf pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (dropf pred (cdr lst))]
    [else lst]))

(define (takef pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (takef pred (cdr lst)))]
    [else '()]))


(define (parse-recipe archivo)
  (define in (open-input-file archivo))
  (define lineas (sequence->list (in-lines in)))
  (close-input-port in)

  (define ingredientes
    (dropf (lambda (l) (not (string=? l "Ingredients:" ))) lineas))

  (define solo-instrucciones
    (takef (lambda (l) (not (string=? l "Instructions:" ))) (cdr ingredientes)))

  solo-instrucciones)

;;----CANTIDAD
;;funcion para transformar los casos "1/2" "1" "1 1/2" y 
(define (parsear-cantidad linea)
  (define tokens (string-split linea))
  (cond
    ;; caso "1 1/2"
    [(and (>= (length tokens) 2)
          (regexp-match? #rx"^[0-9]+$" (first tokens))
          (regexp-match? #rx"^[0-9]+/[0-9]+$" (second tokens)))
     (let* ([entero (string->number (first tokens))]
            [frac (regexp-split #rx"/" (second tokens))]
            [num (string->number (first frac))]
            [den (string->number (second frac))])
       (*(+ entero (/ num den))1.0))]

    ;; caso solo fraccion "1/2"
    [(and (>= (length tokens) 1)
          (regexp-match? #rx"^[0-9]+/[0-9]+$" (first tokens)))
     (let* ([frac (regexp-split #rx"/" (first tokens))]
            [num (string->number (first frac))]
            [den (string->number (second frac))])
       (*(/ num den)1.0))]

    ;; caso solo entero "2"
    [(and (>= (length tokens) 1)
          (regexp-match? #rx"^[0-9]+$" (first tokens)))
     (string->number (first tokens))]

    [else
     (error "Formato no reconocido en la línea" linea)]))

;;---------UNIDAD
(define (parsear-unidad linea)
  (define tokens (string-split linea))
  (cond
    ;; caso "1 1/2 unidad"
    [(and (>= (length tokens) 3)
          (regexp-match? #rx"^[0-9]+$" (first tokens))
          (regexp-match? #rx"^[0-9]+/[0-9]+$" (second tokens)))
     (list-ref tokens 2)]

    ;; caso solo fraccion "1/2 unidad"
    [(and (>= (length tokens) 2)
          (regexp-match? #rx"^[0-9]+/[0-9]+$" (first tokens)))
     (list-ref tokens 1)]

    ;; caso solo entero "2 unidad"
    [(and (>= (length tokens) 2)
          (regexp-match? #rx"^[0-9]+$" (first tokens)))
     (list-ref tokens 1)]

    [else
     (error "No hay suficientes elementos o formato no reconocido en la línea" linea)]))

;;---------INGREDIENTE
(define (parsear-ingrediente linea)
  (define tokens (string-split linea))
  (cond
    ;; caso "1 1/2 unidad"
    [(and (>= (length tokens) 4)
          (regexp-match? #rx"^[0-9]+$" (first tokens))
          (regexp-match? #rx"^[0-9]+/[0-9]+$" (second tokens)))
     (list-tail tokens 3)]

    ;; caso solo fraccion "1/2 unidad"
    [(and (>= (length tokens) 3)
          (regexp-match? #rx"^[0-9]+/[0-9]+$" (first tokens)))
     (list-tail tokens 2)]

    ;; caso solo entero "2 unidad"
    [(and (>= (length tokens) 3)
          (regexp-match? #rx"^[0-9]+$" (first tokens)))
     (list-tail tokens 2)]

    [else
     (error "No hay suficientes elementos o formato no reconocido en la línea" linea)]))

;;------[LINEA COMPLETA]
(define(linea-completa linea)
    (define tokens(string-split linea))
    (define cantidad(parsear-cantidad linea))
    (define unidad(parsear-unidad linea))
    (define ingredientes(parsear-ingrediente linea))
    (list cantidad unidad ingredientes))