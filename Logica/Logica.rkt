#lang racket
;; =========================================================
;Referencias
;https://chatgpt.com/
;https://www.youtube.com/watch?v=NwZPlLTK-UQ&list=PLTd5ehIj0goMswKV4Glhr4uixrhCmihIt&index=2&ab_channel=makigas
; =========================================================
; Buscaminas como grafo, programación funcional)
; - Matriz NxM (H filas x W columnas), lista de adyacencia y tabla de 8 vecinos
; =========================================================
;(x,y) = (3,2) ⇒ i = 3 + 2·5 = 13
;Y si i = 11, entonces x = 11 mod(%) 5 = 1, y = ⌊11//5⌋ = 2 ⇒ (1,2)
; ---------- Indexado 2D<->1D ----------
(define (idx x y W) (+ x (* y W)))      ; (x,y) -> i
(define (x-of i W)  (remainder i W))    ; i -> x
(define (y-of i W)  (quotient  i W))    ; i -> y
;Como un plano cartesiano
(define (dentro? x y W H)
  (and (<= 0 x) (< x W) (<= 0 y) (< y H)))

; ---------- Vecinos ----------
; Si el vecino existe devuelve su índice; si no, #f
(define (vecino i dx dy W H)
  (if (dentro? (+ (x-of i W) dx) (+ (y-of i W) dy) W H)
      (idx (+ (x-of i W) dx) (+ (y-of i W) dy) W)
      #f))

; cons solo si x no es #f (helper puro)
(define (cons-si x xs) (if x (cons x xs) xs))

; Lista de vecinos válidos (3/5/8 respectivamente esquina/borde/interior)
(define (vecinos-de i W H)
  (cons-si (vecino i -1 -1 W H)
    (cons-si (vecino i  0 -1 W H)
      (cons-si (vecino i  1 -1 W H)
        (cons-si (vecino i -1  0 W H)
          (cons-si (vecino i  1  0 W H)
            (cons-si (vecino i -1  1 W H)
              (cons-si (vecino i  0  1 W H)
                (cons-si (vecino i  1  1 W H)
                         '())))))))))

; Tabla fija de 8 vecinos (con #f en los que no existen)
(define (vecinos8 i W H)
  (cons (vecino i -1 -1 W H)
    (cons (vecino i  0 -1 W H)
      (cons (vecino i  1 -1 W H)
        (cons (vecino i -1  0 W H)
          (cons (vecino i  1  0 W H)
            (cons (vecino i -1  1 W H)
              (cons (vecino i  0  1 W H)
                    (cons (vecino i  1  1 W H) '())))))))))

; ---------- Constructores (listas puras) ----------
; rango [a, b)
(define (rango a b)
  (if (>= a b) '()
      (cons a (rango (+ a 1) b))))

; fila y -> (idx 0 y W) ... (idx (W-1) y W)
(define (fila W y x)
  (if (= x W) '()
      (cons (idx x y W) (fila W y (+ x 1)))))

; matriz HxW (lista de filas)
(define (matriz-nodos W H y)
  (if (= y H) '()
      (cons (fila W y 0) (matriz-nodos W H (+ y 1)))))

; lista de adyacencia: en posición i, (vecinos-de i W H)
(define (alist-ady W H i N)
  (if (= i N) '()
      (cons (vecinos-de i W H) (alist-ady W H (+ i 1) N))))

; tabla de 8 vecinos por nodo (listas de 8 elementos o #f)
(define (tabla-vecinos W H i N)
  (if (= i N) '()
      (cons (vecinos8 i W H) (tabla-vecinos W H (+ i 1) N))))

; ---------- Estructura de grafo (inmutable por defecto) ----------
(struct grafo (W H N nodos matriz adjacencia vecinos8) #:transparent)
; W, H: dimensiones
;; N: número total de nodos (= W*H)
; nodos: '(0 1 2 ... N-1)
; matriz: lista de H filas; cada fila lista de W índices (NxM)
; adjacencia: lista donde (list-ref adjacencia i) => vecinos válidos de i
; vecinos8: lista donde (list-ref vecinos8 i) => 8 vecinos (o #f)

(define (grafo-grid W H)
  (grafo W H
         (* W H)
         (rango 0 (* W H))
         (matriz-nodos W H 0)
         (alist-ady W H 0 (* W H))
         (tabla-vecinos W H 0 (* W H))))

; ---------- Utilidades puras ----------
(define (grado i g)
  (length (list-ref (grafo-adjacencia g) i)))

; ----------- Ejemplos (descomentar para probar) --------------
;(define G (grafo-grid 9 9))
;(grafo-matriz G)                   ; matriz 9x9 con índices 0..80
;(list-ref (grafo-adjacencia G) 0)  ; => vecinos de 0 (3 elementos)
;(list-ref (grafo-vecinos8 G) 0)    ; => (#f #f #f #f 1 #f 9 10)
;(grado 0 G)                        ; => 3

; ---- Exports ----
(provide
  idx x-of y-of dentro?
  vecino cons-si vecinos-de vecinos8
  rango fila matriz-nodos alist-ady tabla-vecinos
  grafo grafo? grafo-W grafo-H grafo-N grafo-nodos grafo-matriz grafo-adjacencia grafo-vecinos8
  grafo-grid)

