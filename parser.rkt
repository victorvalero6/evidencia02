#lang racket
(require racket/list)
(require racket/string)
(provide parse-recipe)
(provide parse-recipe-instructions)
(require "unidades.rkt")

(define (string-blank? s)
  (string=? (string-trim s) ""))
(provide parsear-cantidad)
(provide parsear-unidad)
(provide parsear-ingrediente)
(provide linea-completa)
(provide convertir-temperaturas)
(provide parse-recipe-porciones)
(provide parser-optionstxt)
(provide second-to-last)
(provide parse-recipe-extra)




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

(define (parse-recipe archivo0)
  (define in (open-input-file archivo0))
  (define lineas (sequence->list (in-lines in)))
  (close-input-port in)

  (define ingredientes
    (dropf (lambda (l) (not (string=? l "Ingredients:" ))) lineas))

  (define solo-instrucciones
    (takef (lambda (l) (not (string=? l "Instructions:" ))) (cdr ingredientes)))

  (define solo-lineas-validas
    (filter (lambda (l) (not (string-blank? l))) solo-instrucciones))

  (define lineas-procesadas
    (map linea-completa solo-lineas-validas))

  lineas-procesadas)


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
     linea]))

;Casos especiales Grated zest of 1 lemon

(define (second-to-last lst)
  (if (< (length lst) 2)
      (error "List too short for second-to-last")
      (list-ref lst (- (length lst) 2))))

(define (parsear-linea linea)
  (define tokens (string-split linea))
  (cond

    [(and (= (length tokens) 2)
          (regexp-match? #rx"^[0-9]+$" (first tokens)))
     (list (string->number (first tokens))
           "unit"
           (list (second tokens)))]

    ;Powdered sugar, for dusting
    [(and (>= (length tokens) 3)
          (string=? (last tokens) "dusting")
          (string=? (second-to-last tokens) "for"))
     (list 1 "unit" (list (string-join (drop-right tokens 2) " ") "for dusting"))]

    [(and (>= (length tokens) 4)
          (string=? (first tokens) "Grated")
          (string=? (second tokens) "zest")
          (string=? (third tokens) "of"))
     (list 1 "unit" (list "lemon zest"))]

    [(and (>= (length tokens) 2)
          (string=? (string-join (take-right tokens 2)) "to taste"))
     (list 1 "unit" (list (string-join (drop-right tokens 2) " ") "to taste"))]

    [else
     (list (parsear-cantidad linea)
           (parsear-unidad linea)
           (parsear-ingrediente linea))]))


;;------[LINEA COMPLETA]
(define (linea-completa linea)
  (cond
    [(string-blank? linea) '()]

    [else (parsear-linea linea)]))

;----INSTRUCCIONES READ

(define (parse-recipe-instructions archivo1)
  (define in (open-input-file archivo1))
  (define lineas (sequence->list (in-lines in)))
  (close-input-port in)

  (define instrucciones
    (cdr(dropf (lambda (l) (not (string=? l "Instructions:"))) lineas)))

  instrucciones)

(define (convertir-temperaturas texto)
  ;; Definimos 350°F
  (define regex #px"([0-9]+)°F")
  (if (regexp-match regex texto)
      (let* (
             [match (regexp-match regex texto)]
             [f (string->number (second match))]
             [c (fahrenheit f)]
             [c-redondeado (round (* c 10))]
             [c-final (/ c-redondeado 10.0)]
             [nuevo-texto (regexp-replace regex texto
                                          (format "~a°C" c-final))])
        ;; Retornamos el texto con la temperatura convertida
        nuevo-texto)

      ;; Si no hay coincidencia, simplemente regresamos el texto original
      texto))

;;------Porciones READ
(define (parse-recipe-porciones archivo2)
  (define in (open-input-file archivo2))
  (define lineas (sequence->list (in-lines in)))
  (close-input-port in)

  (define posibles-lineas
    (dropf (lambda (l)
             (not (regexp-match? #px"(?i:^serv(e|ing)s?[[:space:]]*-[[:space:]]*[0-9]+)" l)))
           lineas))

  (if (null? posibles-lineas)
      (error "No se encontró la línea de porciones")
      (let* ([linea (first posibles-lineas)]
             [partes (string-split linea "-")]
             [num-str (string-trim (second partes))]
             [numPorciones (string->number num-str)])
        (if numPorciones
            numPorciones
            (error "No se pudo convertir el número de porciones")))))

;LEER OPTIONS.TXT

(define (parser-optionstxt path)
  (define in (open-input-file path))
  (define lines (sequence->list (in-lines in)))
  (close-input-port in)
  (map (lambda (line)
         (define parts (string-split line ":"))
         (cons (string-trim (string-downcase (first parts)))
               (string-trim (second parts))))
       lines))

;;TODO ANTES DE INGREDIENTES

;----INSTRUCCIONES READ

(define (parse-recipe-extra archivo4)
  (define in (open-input-file archivo4))
  (define lineas (sequence->list (in-lines in)))
  (close-input-port in)

  (define info-extra
    (cdr(takef (lambda (l) (not (or(string=? l "Ingredients:")))) lineas)))

  info-extra)


