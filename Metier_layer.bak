#lang scheme
(provide get-operateur premier-operande deuxieme-operande constructeur est-constante? est-identificateur? est-surparenthese?)

; Une expression arithmétique a trois forme
;      1) Forme Préfixé   (+ x 1)
;      2) Forme Infixé    (x + 1)
;      3) Forme Suffixé   (x 1 +)


(define get-operateur (lambda (exp)                        ;fonction operateur : expression ------> operateur
                    (car (cdr exp))                        ;return l'operateur principale d'une expression arithmétique 
                 ))

(define premier-operande (lambda (exp)                     ;fonction opde1 : expression ------> expression
                    (car exp)                              ;renvoie la première opérande d'une expression arithmétique
                 ))

(define deuxieme-operande (lambda (exp)                    ;fonction opde2 : expression ------> expression
                    (car (cdr (cdr exp)))                  ;renvoie la deuxième opérande d'une expression arithmétique
                 ))

;(define deuxieme-operande (lambda (exp)                   ;fonction opde2 : expression ------> expression
;                    (caddr exp)                           ;renvoie la deuxième opérande d'une expression arithmétique
;                 ))

(define est-constante? (lambda (exp)                       ;fonction const? : expression ------> booléen
                    (number? exp)                          ;renvoie vrai si l'expression est une constante et faux sinon
                 ))

(define est-identificateur? (lambda (exp)                  ;fonction id? : expression ------> booléen
                    (symbol? exp)                          ;renvoie vrai si l'expression est un identificateur (symbol) et faux sinon
                 ))

(define est-surparenthese? (lambda (exp)
                    (equal? (length exp) 1)                ;fonction : expression -----> booléen
                 ))

(define constructeur (lambda (exp1 operateur exp2)
                    (list exp1 operateur exp2)             ;fontion qui construit 
                 ))

