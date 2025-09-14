#lang racket
(require 2htdp/universe 2htdp/image
         "../Logica/Logica.rkt")

;==================== CONSTANTES DE UI ====================
(define CELL        28)   ; Tamaño de celda aumentado para mejor visibilidad
(define TOP-H       52)
(define MARGIN      10)
(define DD-W        92)
(define DD-H        28)
(define DD-ITEM-H   26)
(define TOOL-W      560)
(define MIN-WIDTH 600)
(define MIN-HEIGHT 500)
(define BASE-WIDTH 800)
(define BASE-HEIGHT 600)

;Colores
(define COLOR-top       (make-color 80 120 40))
(define COLOR-top-txt   "white")
(define COLOR-dd-bg     "white")
(define COLOR-dd-bor    (make-color 200 200 200))
(define COLOR-board-a   (make-color 176 214 102))
(define COLOR-board-b   (make-color 164 204 94))
(define COLOR-cell-open (make-color 220 220 220))
(define COLOR-mine      "red")
(define COLOR-flag      "orange")

;Colores para números
(define (color-numero n)
  (case n
    [(1) "blue"]
    [(2) "green"]
    [(3) "red"]
    [(4) "darkblue"]
    [(5) "brown"]
    [(6) "cyan"]
    [(7) "black"]
    [(8) "gray"]
    [else "black"]))

;-----------------CONFIGURACIÓN DE DIFICULTADES------------------
(define (config d)
  (case d
    [(facil)   (values 9  9  10)]  ; columnas, filas, minas
    [(medio)   (values 16 16 40)]
    [(dificil) (values 30 16 99)]
    [else (error 'config (format "dificultad desconocida: ~a" d))]))

;-----------------ESTADO DEL MUNDO------------------------------
(struct world (cols rows mines diff menu-open? tablero estado mensaje) #:transparent)
;estado: 'jugando, 'victoria, 'derrota
;mensaje: string para mostrar al usuario

(define (mk-world diff)
  (define-values (cols rows mines) (config diff))
  (define tablero-juego (crear-tablero rows cols diff))  ; filas, columnas, nivel
  (world cols rows mines diff #f tablero-juego 'jugando (format "Minas: ~a" mines)))

;-----------------DROPDOWN MENU-----------------------
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
              (if checked? 
                  (text "✓" 14 "forestgreen")
                  (rectangle 1 1 "solid" "transparent")))
      (rectangle DD-W DD-ITEM-H "solid" COLOR-dd-bg)))
  (overlay/align "left" "top"
    (above (row "Facil"   (eq? current 'facil))
           (row "Medio"   (eq? current 'medio))
           (row "Dificil" (eq? current 'dificil)))
    (rectangle DD-W (* 3 DD-ITEM-H) "outline" COLOR-dd-bor)))

;--------------------DIBUJO DE CELDAS---------------
(define (dibujar-celda celda x y alt?)
  (define base-color (if alt? COLOR-board-a COLOR-board-b))
  (cond
    ;Celda descubierta
    [(celda-descubierta? celda)
     (cond
       ;Es mina - mostrar bomba
       [(celda-mina? celda)
        (overlay (circle 10 "solid" COLOR-mine)
                 (rectangle CELL CELL "solid" COLOR-cell-open))]
       ;Sin minas alrededor - celda vacía
       [(= (celda-vecinos celda) 0)
        (rectangle CELL CELL "solid" COLOR-cell-open)]
       ;Con minas alrededor - mostrar número
       [else
        (overlay (text (number->string (celda-vecinos celda)) 
                      16 
                      (color-numero (celda-vecinos celda)))
                 (rectangle CELL CELL "solid" COLOR-cell-open))])]
    ;Celda marcada con bandera
    [(celda-marcada? celda)
     (overlay (text "🚩" 16 COLOR-flag)
              (rectangle CELL CELL "solid" base-color))]
    ;Celda oculta
    [else
     (rectangle CELL CELL "solid" base-color)]))

;-------------------DIBUJO DEL TABLERO------------------------
(define (dibujar-tablero-completo w)
  (define tablero (world-tablero w))
  (define filas (tablero-filas tablero))
  (define columnas (tablero-columnas tablero))
  (define celdas (tablero-celdas tablero))
  (dibujar-filas celdas columnas filas))

(define (dibujar-filas celdas columnas filas)
  (define (dibujar-fila y)
    (define (dibujar-celda-en-x x)
      (define idx-actual (idx x y columnas))
      (dibujar-celda (list-ref celdas idx-actual) x y (even? (+ x y))))
    (apply beside (map dibujar-celda-en-x (range columnas))))
  (apply above (map dibujar-fila (range filas))))

;-----------------------BARRA SUPERIOR--------------------------
(define (draw-top w SCN-W)
  (define dd (dd-button (case (world-diff w)
                          [(facil) "Facil"]
                          [(medio) "Medio"]
                          [else "Dificil"])))
  (define base (rectangle SCN-W TOP-H "solid" COLOR-top))
  (define with-dd
    (place-image dd
                 (+ MARGIN (/ (image-width dd) 2))
                 (/ TOP-H 2)
                 base))
  ;Agregar información del juego
  (define with-title
    (place-image (text "BusCEMinas" 18 COLOR-top-txt)
                 (/ SCN-W 2) (/ TOP-H 2)
                 with-dd))
  ;Mostrar contadores
  (define minas-restantes 
    (- (world-mines w)
       (tablero-contar-marcadas (world-tablero w))))
  (place-image (text (format "Minas: ~a" minas-restantes) 14 COLOR-top-txt)
               (- SCN-W 60) (/ TOP-H 2)
               with-title))

;------------------CONVERSIÓN PANTALLA → CELDA---------------------
(define (screen->cell w scn-w x y)
  (define cols (world-cols w))
  (define rows (world-rows w))
  (define BW (* cols CELL))
  (define BH (* rows CELL))
  
  ; Calcular la posición del tablero centrado
  (define left (- (/ scn-w 2) (/ BW 2)))
  (define top (+ TOP-H MARGIN))
  
  (define cx (inexact->exact (floor (/ (- x left) CELL))))
  (define cy (inexact->exact (floor (/ (- y top) CELL))))
  
  (if (and (<= left x) (< x (+ left BW)) 
           (<= top y) (< y (+ top BH))
           (>= cx 0) (< cx cols)
           (>= cy 0) (< cy rows))
      (cons cx cy)
      #f))

;------------------DIBUJO DEL MUNDO----------------------------
(define (draw-world w)
  (define cols (world-cols w))
  (define rows (world-rows w))
  (define BOARD-W (* cols CELL))
  (define BOARD-H (* rows CELL))
  
  ; Calcular dimensiones de la ventana basado en el tamaño del tablero
  (define SCN-W (max MIN-WIDTH (+ (* 2 MARGIN) BOARD-W)))
  (define SCN-H (max MIN-HEIGHT (+ TOP-H MARGIN BOARD-H MARGIN 30)))
  
  (define board (dibujar-tablero-completo w))
  (define scene (empty-scene SCN-W SCN-H))
  (define with-top (place-image (draw-top w SCN-W) (/ SCN-W 2) (/ TOP-H 2) scene))
  
  ; Calcular posición central del tablero
  (define board-x (/ SCN-W 2))
  (define board-y (+ TOP-H MARGIN (/ BOARD-H 2)))
  
  (define with-board (place-image board board-x board-y with-top))
  
  ; Mostrar mensaje de estado
  (define mensaje
    (cond
      [(eq? (world-estado w) 'victoria)
       (text "¡VICTORIA! Presiona R para reiniciar" 16 "green")]
      [(eq? (world-estado w) 'derrota)
       (text "¡DERROTA! Presiona R para reiniciar" 16 "red")]
      [else
       (text (world-mensaje w) 14 "black")]))
  
  (define with-message
    (place-image mensaje
                 (/ SCN-W 2)
                 (- SCN-H 15)
                 with-board))
  
  ; Si el menú está abierto, mostrarlo
  (if (world-menu-open? w)
      (place-image (dd-menu (world-diff w))
                   (+ MARGIN (/ DD-W 2))
                   (+ (/ TOP-H 2) (/ DD-H 2) (/ (* 3 DD-ITEM-H) 2) 6)
                   with-message)
      with-message))

;--------------------HIT TESTS-------------------------
(define (pt-in-rect? x y rx ry rw rh)
  (and (<= rx x) (< x (+ rx rw)) 
       (<= ry y) (< y (+ ry rh))))

(define (hit-dd? x y)
  (pt-in-rect? x y MARGIN 12 DD-W DD-H))

(define (hit-dd-item x y)
  (define rx MARGIN)
  (define ry (+ 12 DD-H 4))
  (cond [(pt-in-rect? x y rx ry DD-W DD-ITEM-H) 'facil]
        [(pt-in-rect? x y rx (+ ry DD-ITEM-H) DD-W DD-ITEM-H) 'medio]
        [(pt-in-rect? x y rx (+ ry (* 2 DD-ITEM-H)) DD-W DD-ITEM-H) 'dificil]
        [else #f]))

;---------------------CAMBIO DE DIFICULTAD-------------------------
(define (with-diff w d)
  (mk-world d))

;---------------------MANEJO DE EVENTOS-------------------------
(define (handle-mouse w x y me)
  ;No permitir clicks si el juego terminó
  (if (member (world-estado w) '(victoria derrota))
      w  ;No hacer nada si el juego terminó
      (handle-mouse-activo w x y me)))

(define (handle-mouse-activo w x y me)
  (define cols (world-cols w))
  (define SCN-W (max MIN-WIDTH (+ (* 2 MARGIN) (* cols CELL))))
  (define p (screen->cell w SCN-W x y))
  
  ; Resto del código permanece igual...
  (cond
    ; Abrir/cerrar menú
    [(and (string=? me "button-down") (hit-dd? x y))
     (struct-copy world w [menu-open? (not (world-menu-open? w))])]
    
    ; Elegir dificultad del menú
    [(and (string=? me "button-down") (world-menu-open? w))
     (define d (hit-dd-item x y))
     (if d (with-diff w d) 
         (struct-copy world w [menu-open? #f]))]
    
    ; Click izquierdo en tablero - descubrir celda
    [(and (string=? me "button-down") p)
     (handle-left-click w (car p) (cdr p))]
    
    ; Click derecho en tablero - marcar/desmarcar
    [(and (string=? me "right-button-down") p)
     (handle-right-click w (car p) (cdr p))]
    
    [else w]))

;Manejar click izquierdo (descubrir celda)
(define (handle-left-click w cx cy)
  (define tb-actual (world-tablero w))
  (define celda-actual (tablero-obtener-celda-xy tb-actual cx cy))
  
  (cond
    [(not celda-actual) w]  ;Coordenadas inválidas
    [(celda-marcada? celda-actual) w]  ;No descubrir si está marcada
    [(celda-descubierta? celda-actual) w]  ;Ya descubierta
    [else
     (define tb-nuevo (descubrir-celda tb-actual cx cy))
     (define nuevo-mundo (struct-copy world w [tablero tb-nuevo]))
     
     ;Verificar estado del juego
     (cond
       [(verificar-derrota? tb-nuevo)
        (struct-copy world nuevo-mundo 
                    [estado 'derrota]
                    [mensaje "¡Has perdido! Presiona R para reiniciar"])]
       [(verificar-victoria? tb-nuevo)
        (struct-copy world nuevo-mundo 
                    [estado 'victoria]
                    [mensaje "¡Has ganado! Presiona R para reiniciar"])]
       [else
        (struct-copy world nuevo-mundo
                    [mensaje (format "Minas restantes: ~a" 
                                   (- (world-mines w)
                                      (tablero-contar-marcadas tb-nuevo)))])])]))

;Manejar click derecho (marcar/desmarcar)
(define (handle-right-click w cx cy)
  (define tb-actual (world-tablero w))
  (define celda-actual (tablero-obtener-celda-xy tb-actual cx cy))
  
  (cond
    [(not celda-actual) w]
    [(celda-descubierta? celda-actual) w]  ;No marcar si ya está descubierta
    [else
     (define tb-nuevo (marcar-celda tb-actual cx cy))
     (struct-copy world w 
                 [tablero tb-nuevo]
                 [mensaje (format "Minas restantes: ~a" 
                               (- (world-mines w)
                                  (tablero-contar-marcadas tb-nuevo)))])]))

;---------------------MANEJO DE TECLADO------------------------
(define (handle-key w key)
  (cond
    ;Reiniciar juego con R
    [(key=? key "r")
     (mk-world (world-diff w))]
    
    ;Cambiar dificultad con teclas numéricas
    [(key=? key "1") (mk-world 'facil)]
    [(key=? key "2") (mk-world 'medio)]
    [(key=? key "3") (mk-world 'dificil)]
    
    [else w]))

;---------------------FUNCIÓN PRINCIPAL---------------------
(define (main [inicio 'facil])
  (big-bang (mk-world inicio)
    (to-draw  draw-world)
    (on-mouse handle-mouse)
    (on-key   handle-key)
    (name     "BusCEMinas")))

;Iniciar el juego
(main 'facil)