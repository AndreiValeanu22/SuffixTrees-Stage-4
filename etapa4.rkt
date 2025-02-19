#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))
(define (list->stream L)
  (if (null? L)
      empty-stream
      (collection-cons (car L) (list->stream (cdr L)))))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).
;functie etapa1
(define (longest-common-prefix w1 w2)
  (common-prefix-h w1 w2 null)
  )

(define (common-prefix-h w1 w2 L)
  (cond ((null? w1) (if (null? w2)
                        (cons L (cons '() (cons '() '())))
                        (cons L (cons '() (cons w2 '())))))
        (else (if (null? w2)
                  (append (list L) (list w1) (list '()))
                  (if (equal? (car w1) (car w2))
                      (common-prefix-h (cdr w1) (cdr w2) (append L (list (car w1))))
                      (cons L (cons w1 (cons w2 '()))))))))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
;functie etapa1
(define (longest-common-prefix-of-collection words)
  (let loop ([cms-prefix (collection-car words)]
             [words words])
    (if (collection-empty? words)
        cms-prefix
        (let* ([prefix (car (longest-common-prefix cms-prefix (collection-car words)))])
           (if (> (length cms-prefix) (length prefix))
               (loop prefix (collection-cdr words))
               (loop cms-prefix (collection-cdr words)))))))
             




;functie etapa1
(define (match-pattern-with-label st pattern)
  (if (collection-empty? st)
      (append (list #f) (list '()))
     (let* (
            [ramura (first-branch st)]
            [eticheta (get-branch-label ramura)]
            [subarbore_eticheta (get-branch-subtree ramura)]
            [rest_arbore (other-branches st)]
            )
       (if (equal? eticheta pattern)
           #t
           (if (equal? (egalitate_eticheta_sablon eticheta pattern null) #t)
               #t
               (if (null? (egalitate_eticheta_sablon eticheta pattern null))
                   (match-pattern-with-label rest_arbore pattern)
                   (if (equal? (car (egalitate_eticheta_sablon eticheta pattern null)) '(5))
                       (append (list eticheta) (list (car (cdr (cdr (egalitate_eticheta_sablon eticheta pattern null))))) (list subarbore_eticheta))
                       (if  (not (equal? (car (egalitate_eticheta_sablon eticheta pattern null)) '(5)))
                               
                          (append (list #f) (list (egalitate_eticheta_sablon eticheta pattern null)))
                           
                           (match-pattern-with-label rest_arbore pattern)))))))))


;functie etapa1
(define (egalitate_eticheta_sablon eticheta sablon copie)
  (if (and (null? eticheta) (not (null? sablon))) ;daca sunt pe cazul in care trebuie eticheta e mai mica decat sablonul
      (append (list '(5)) (list copie) (list sablon))
     
      (if (and (null? sablon) (not (null? eticheta)))  ;daca sablonul este cuprin total in prefixul etichetei
          #t
          (if (equal? (car eticheta) (car sablon))
              (egalitate_eticheta_sablon (cdr eticheta) (cdr sablon) (append copie (list (car eticheta))))
              copie))))


(define (st-has-pattern? st pattern)
  (let* ([rezultat (match-pattern-with-label st pattern)])
    (if (equal? rezultat #t)
        #t
        (if (equal? (car rezultat) #f)
            #f
          (st-has-pattern? (cdr (cdr rezultat)) (car (cdr rezultat)))))))

;functie etapa2
(define (get-suffixes text)
  (if (collection-empty? text)
      empty-collection
      (collection-cons text
                       (get-suffixes (collection-cdr text)))))


(define (get-ch-words words ch)
  (collection-filter (λ (word) (and (not (null? word)) (eq? (car word) ch)))
                     words))


 (define (ast-func suffixes)
    (let* ([sufix    (collection-car suffixes)]
           [litera (car sufix)])
  (cons (list litera)
        (collection-map (λ (sufix) (cdr sufix))
                        suffixes))))


(define (cst-func suffixes)
 (let* ([prefix (longest-common-prefix-of-collection suffixes)])
   (cons prefix
         (collection-map (λ (sufix) (car (cdr (longest-common-prefix sufix prefix))))
                         suffixes))))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
(collection-map (λ (arr)
                  (if (null? (cdr arr))
                      arr
                      (cons  (car arr) (suffixes->st labeling-func (cdr arr) alphabet ))))
                (collection-map  labeling-func
                                 (collection-filter (λ (list)
                                                      (not (collection-empty? list)))
                                                    (collection-map (λ (litera)
                                                                      (get-ch-words suffixes litera))
                                                                      alphabet)))))



(define (text->st functie_etichete)
  (λ (text)
    (let* ([text_$ (append text '(#\$))]
           [alp_stream (list->stream (sort (remove-duplicates text_$) char<?))])
    (suffixes->st functie_etichete (get-suffixes text_$) alp_stream))))

; b) Din funcția text->st derivați funcția text->ast care
; primește un text (listă de caractere) și întoarce AST-ul
; asociat textului.
(define text->ast 
   (text->st ast-func))

; c) Din funcția text->st derivați funcția text->cst care
; primește un text (listă de caractere) și întoarce CST-ul
; asociat textului.
(define text->cst 
  (text->st  cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.

(define (substring? text pattern)
  (let loop ([text text] [pattern1 pattern] [afisare '()])
    (let* ([egalitate (equal? (reverse afisare) pattern)]
           [null_pat (null?  pattern1)]
           [null_text (null? text)])
    (cond ((and null_text egalitate)  #t)
          ((and null_text (not egalitate)) #f)
          ((and null_pat egalitate)  #t)
          ((and null_pat (not egalitate)) #f)
          ((equal? (car text) (car pattern1)) (loop (cdr text) (cdr pattern1) (cons (car text) afisare)))
          (else (loop (cdr text)  pattern null))))))
             
 

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  'your-code-here)