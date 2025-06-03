#lang racket
(require racket/list)
(provide parse-recipe)
(provide parsear-cantidad)
(provide parsear-unidad)


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

  ;;String split - aqui separamos por espacio las palabras
(define (string-split str)
    (regexp-split #rx" " str))

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

(define(parsear-unidad linea)
  (define tokens(string->number linea))
  (cons
    [(and(=(length tokens)1)
        (regexp-match? #rx"^a-z+$" (first tokens)))]
  
  ;;Me gustaria que la funcion encuentre todos los elementos strings de la linea, que el car lo tome 
  ;;y lo defina como el de unidad y el resto cdr que sea definido como el ingrediente
  
  
  
  
  
  
  
  ))
