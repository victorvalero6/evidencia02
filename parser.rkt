#lang racket
(require racket/list)
(provide parse-recipe)

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