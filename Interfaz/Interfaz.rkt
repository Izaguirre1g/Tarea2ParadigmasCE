#lang racket
(require 2htdp/universe 2htdp/image
         "../Logica/Logica.rkt")
;Referencias
;https://share.google/6mKGLEJ16MMdItcFi
; ================== UI Buscaminas (solo interfaz) ==================

; ---- Constantes de UI ----
(define CELL        24)
(define TOP-H       52)
(define MARGIN      10)
(define DD-W        92)
(define DD-H        28)
(define DD-ITEM-H   26)
(define TOOL-W      560)
;(define TOOL-W 800)  ; ancho mínimo de ventana
;(define TOOL-H 500)  ; alto mínimo de ventana (nuevo)


(define COLOR-top     (make-color 80 120 40))
(define COLOR-top-txt "white")
(define COLOR-dd-bg   "white")
(define COLOR-dd-bor  (make-color 200 200 200))
(define COLOR-board-a (make-color 176 214 102))
(define COLOR-board-b (make-color 164 204 94))

; ---- Dificultades ----
(define (config d)
  (case d
    [(facil)   (values 9  9  10)]
    [(medio)   (values 16 16 40)]
    [(dificil) (values 30 16 99)]
    [else (error 'config (format "dificultad desconocida: ~a" d))]))

; ---- Estado (ahora incluye el grafo g) ----
(struct world (cols rows mines diff menu-open? g last-cell t) #:transparent)
; g: grafo del tablero (cada nodo = celda)
; last-cell: (cons x y) o #f
; t: estructura tablero (lista de celdas)

(define (mk-world diff)
  (define-values (W H M) (config diff))
  (world W H M diff #f
         (grafo-grid W H)                                ; grafo
         #f
         (crear-tablero-estructura H W diff              ; tablero vacío
                                   (crear-lista-celdas-vacias-WxH W H))))
; ---- Dropdown ----
(define (dd-button label)
  (overlay/align "left" "middle"
    (beside (text label 16 COLOR-top-txt)
            (rectangle 8 1 "solid" "transparent")
            (triangle 8 "solid" "white"))
    (rectangle DD-W DD-H "solid" COLOR-top)))

(define (dd-menu current)
  (define (row txt checked?)
    (overlay/align "left" "middle"
      (beside (rectangle 8 1 "solid" "transparent")
              (text txt 14 "black")
              (rectangle 8 1 "solid" "transparent")
              (if checked? (text "✓" 14 "forestgreen")
                           (rectangle 1 1 "solid" "transparent")))
      (rectangle DD-W DD-ITEM-H "solid" COLOR-dd-bg)))
  (overlay/align "left" "top"
    (above (row "Facil"   (eq? current 'facil))
           (row "Medio"   (eq? current 'medio))
           (row "Dificil" (eq? current 'dificil)))
    (rectangle DD-W (* 3 DD-ITEM-H) "outline" COLOR-dd-bor)))

; ---- Tablero ajedrezado ----
(define (tile alt?) (rectangle CELL CELL "solid" (if alt? COLOR-board-a COLOR-board-b)))

(define (make-row cols start-alt)
  (define (go n alt acc)
    (if (= n 0) acc
        (go (sub1 n) (not alt) (beside acc (tile alt)))))
  (if (= cols 0)
      (rectangle 0 0 "solid" "transparent")
      (go (sub1 cols) (not start-alt) (tile start-alt))))

(define (checkerboard-cols-rows cols rows)
  (define (go r alt acc)
    (if (= r 0) acc
        (go (sub1 r) (not alt) (above acc (make-row cols alt)))))
  (if (= rows 0)
      (rectangle 0 0 "solid" "transparent")
      (go (sub1 rows) #f (make-row cols #t))))

; ---- Barra superior (solo dropdown + título centrado) ----
(define (draw-top w SCN-W)
  (define dd (dd-button (case (world-diff w)
                          [(facil) "Facil"]
                          [(medio) "Medio"]
                          [else     "Dificil"])))
  (define base (rectangle SCN-W TOP-H "solid" COLOR-top))
  (define with-dd
    (place-image dd
                 (+ MARGIN (/ (image-width dd) 2))
                 (/ TOP-H 2)
                 base))
  (place-image (text "Buscaminas" 18 COLOR-top-txt)
               (/ SCN-W 2) (/ TOP-H 2)
               with-dd))

; ---- Conversión pantalla → celda (x,y) ----
; Devuelve (cons cx cy) si el click cae en el tablero; #f si no.
(define (screen->cell w scn-w x y)
  (define BW (* (world-cols w) CELL))
  (define BH (* (world-rows w) CELL))
  (define left (- (/ scn-w 2) (/ BW 2)))
  (define top  (+ TOP-H MARGIN))
  (define cx (inexact->exact (floor (/ (- x left) CELL))))
  (define cy (inexact->exact (floor (/ (- y top)  CELL))))
  (if (and (<= left x) (< x (+ left BW)) (<= top y) (< y (+ top BH)))
      (cons cx cy)
      #f))

; ---- Dibujo del mundo ----
(define (draw-world w)
  (define BOARD-W (* (world-cols w) CELL))
  (define BOARD-H (* (world-rows w) CELL))
  (define SCN-W (max (+ (* 2 MARGIN) BOARD-W) TOOL-W))
  (define SCN-H (+ TOP-H MARGIN BOARD-H MARGIN))
  (define board (checkerboard-cols-rows (world-cols w) (world-rows w)))
  (define scene (empty-scene SCN-W SCN-H))
  (define with-top (place-image (draw-top w SCN-W) (/ SCN-W 2) (/ TOP-H 2) scene))
  (define with-board
    (place-image board (/ SCN-W 2) (+ TOP-H MARGIN (/ BOARD-H 2)) with-top))
  (define lc (world-last-cell w))
  (define msg-img
    (if lc
        (text (string-append "click: (" (number->string (car lc))
                             "," (number->string (cdr lc)) ")")
              14 "black")
        (rectangle 1 1 "solid" "transparent")))
  (place-image msg-img
               (/ SCN-W 2)
               (+ TOP-H -8) ; justo debajo de la barra
               (if (world-menu-open? w)
                   (place-image (dd-menu (world-diff w))
                                (+ MARGIN (/ DD-W 2))
                                (+ (/ TOP-H 2) (/ DD-H 2) (/ (* 3 DD-ITEM-H) 2) 6)
                                with-board)
                   with-board)))

; ---- Hit-tests ----
(define (pt-in-rect? x y rx ry rw rh)
  (and (<= rx x) (< x (+ rx rw)) (<= ry y) (< y (+ ry rh))))

(define (hit-dd? x y)
  (pt-in-rect? x y MARGIN 12 DD-W DD-H))

(define (hit-dd-item x y)
  (define rx MARGIN)
  (define ry (+ 12 DD-H 4))
  (cond [(pt-in-rect? x y rx ry DD-W DD-ITEM-H) 'facil]
        [(pt-in-rect? x y rx (+ ry DD-ITEM-H) DD-W DD-ITEM-H) 'medio]
        [(pt-in-rect? x y rx (+ ry (* 2 DD-ITEM-H)) DD-W DD-ITEM-H) 'dificil]
        [else #f]))

; ---- Cambio de dificultad: reconstruye grafo y tablero ----
(define (with-diff w d)
  (define-values (W H M) (config d))
  (world W H M d #f
         (grafo-grid W H)
         #f
         (crear-tablero-estructura H W d
                                   (crear-lista-celdas-vacias-WxH W H))))

; ---- Eventos (mouse) ----
(define (handle-mouse w x y me)
  (define BOARD-W (* (world-cols w) CELL))
  (define SCN-W (max (+ (* 2 MARGIN) BOARD-W) TOOL-W))
  (define p (screen->cell w SCN-W x y))
  (cond
    ; Abrir/cerrar menú
    [(and (string=? me "button-down") (hit-dd? x y))
     (struct-copy world w [menu-open? (not (world-menu-open? w))])]
    ; Elegir dificultad
    [(and (string=? me "button-down") (world-menu-open? w))
     (define d (hit-dd-item x y))
     (if d (with-diff w d) (struct-copy world w [menu-open? #f]))]
    ; Click en tablero: reporta celda (x,y) y su índice de nodo
    [(and (string=? me "button-down") p)
     (define cx (car p))
     (define cy (cdr p))
     (define i  (idx cx cy (world-cols w))) ; nodo del grafo / índice en tablero
     (printf "Click en celda (~a,~a)  -> nodo i=~a\n" cx cy i)
     (struct-copy world w [last-cell p])]
    [else w]))

; ---- Main ----
(define (main [inicio 'facil])
  (big-bang (mk-world inicio)
    (to-draw  draw-world)
    (on-mouse handle-mouse)
    (name     "BusCEminas")))

;(main 'dificil)
(void (main 'dificil));Evita que se imprima todo del programa