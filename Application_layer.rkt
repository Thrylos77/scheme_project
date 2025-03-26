#lang scheme
(require "Metier_layer.rkt")
(provide simplification)

(define simplifier+ (lambda (exp)
              (cond
                ((eq? (simplification (premier-operande exp)) 0)                       (simplification (deuxieme-operande exp)))             ;operande1 = 0
                ((eq? (simplification (deuxieme-operande exp)) 0)                      (simplification (premier-operande exp)))              ;operande2 = 0
                
                ((and (est-constante? (simplification (premier-operande exp)))
                      (est-constante? (simplification (deuxieme-operande exp))))   (+  (simplification (premier-operande exp))
                                                                                       (simplification (deuxieme-operande exp))))            ;les deux operandes sont des constants
                
                (else                                                                  (list (simplification (premier-operande exp))         ;au moins un des operandes est une liste
                                                                                             (get-operateur exp)
                                                                                           (simplification (deuxieme-operande exp))))
                )))

(define simplifier- (lambda (exp)
              (cond
                ((eq? (simplification (premier-operande exp)) 0)                       (simplification (deuxieme-operande exp)))             ;operande1 = 0
                ((eq? (simplification (deuxieme-operande exp)) 0)                      (simplification (premier-operande exp)))              ;operande2 = 0
                
                ((and (est-constante? (simplification (deuxieme-operande exp)))
                      (est-constante? (simplification (premier-operande exp))))   (-   (simplification (premier-operande exp))
                                                                                       (simplification (deuxieme-operande exp))))            ;les deux operandes sont des constants
                
                (else                                                                  (list (simplification (premier-operande exp))         ;au moins un des operandes est une liste
                                                                                             (get-operateur exp)
                                                                                             (simplification (deuxieme-operande exp))))
                )))

(define simplifier* (lambda (exp)
            (cond
              ((eq? (simplification (premier-operande exp)) 1)                         (simplification (deuxieme-operande exp)))             ;operande1 = 1                                                                             
              ((eq? (simplification (deuxieme-operande exp)) 1)                        (simplification (premier-operande exp)))              ;operande2 = 1

              ((or (eq? (simplification (premier-operande exp))  0)                                                                          ;l'un des operandes est null
                   (eq? (simplification (deuxieme-operande exp)) 0))       0)
        
             ((and (est-constante? (simplification (premier-operande exp)))
                   (est-constante? (simplification (deuxieme-operande exp))))   (*     (simplification (premier-operande exp))
                                                                                       (simplification (deuxieme-operande exp))))            ;les deux operandes sont des constants

             (else                                                                     (list (simplification (premier-operande exp))         ;au moins un des operandes est une liste
                                                                                             (get-operateur exp)
                                                                                             (simplification (deuxieme-operande exp))))
             )))


;; Fonction pour reformater une expressino en (opérande1 operateur opérande2)
(define (reformat exp)
       (cond
         ((not (list? exp)) exp)                                                          ; Si ce n'est pas une liste, on ne modifie pas.

         ((and (= (length exp) 3)                                                         ; Cas classique : (+ 5 x) => (5 + x)
               (or (equal? (car exp) '+)
                   (equal? (car exp) '-)
                   (equal? (car exp) '*)))
                                                                            (list (reformat (cadr exp)) (car exp) (reformat (caddr exp))))
         
         ((and (= (length exp) 3)                                                         ; Cas inversé : (5 x +) => (5 + x)
               (or (eq? (caddr exp) '+)
                   (eq? (caddr exp) '-)
                   (eq? (caddr exp) '*)))
                                                                             (list (reformat (car exp)) (caddr exp) (reformat (cadr exp))))
      
         (else (map reformat exp))))                                                      ; Appliquer récursivement à tous les éléments



(define simplification (lambda (exp)                                                              ; Applique le reformat en premier
                   (cond
                       ((or (est-identificateur? exp)
                            (est-constante? exp))                        exp)                                ; Identificateur et constante inchangés                

                       ((est-surparenthese? exp)                        (simplification (car exp)))         ; Suppression des parenthèses inutiles

                       ((eq? '+ (get-operateur (reformat exp)))         (simplifier+ (reformat exp)))                  ;

                       ((eq? '- (get-operateur (reformat exp)))         (simplifier- (reformat exp)))                        

                       ((eq? '* (get-operateur (reformat exp)))         (simplifier* (reformat exp)))                  ;
                    )))






(simplification '(+ 5 13))
(simplification '(- 10 b))
(simplification '(+ (* 2 5) (- 3 1)))
(simplification '((4 + 0) (x + 2) *))           ; => (4 * (x + 2))
(simplification '((x + (1 * y)) + (x * 0)))     ; => (x + y)
(simplification '(+ (3 * 0) (1 - x)))           ; => (1 - x)
(simplification '((x + 0 ) (3 x *) +))          ; => ((x + 2) * (3 * x))
(simplification '(((((x * 1)) + (y * 0)) + (z))))