#lang racket
(require 2htdp/universe 2htdp/image
         "../Logica/Logica.rkt")

; ================== UI Buscaminas Escalable y Configurable ==================

; ---- Constantes de UI Dinámicas ----
(define BASE-CELL-SIZE 32)
(define MIN-CELL-SIZE 20)
(define MAX-CELL-SIZE 50)
(define TOP-H 80)  ; Aumentado para mejor visualización
(define MARGIN 20)
(define DD-W 140)
(define DD-H 35)
(define DD-ITEM-H 32)

; Límites de ventana más amplios para tableros grandes
(define MAX-WINDOW-WIDTH 1800)  ; Ajustado para mejor compatibilidad
(define MAX-WINDOW-HEIGHT 1400) ; Ajustado para mejor compatibilidad
(define MIN-WINDOW-WIDTH 600)   ; Aumentado para tableros pequeños
(define MIN-WINDOW-HEIGHT 550)  ; Aumentado para tableros pequeños
(define UI-EXTRA-HEIGHT (+ TOP-H (* 2 MARGIN) 120)) ; Más espacio para controles

; Colores mejorados
(define COLOR-top       (make-color 60 100 30))
(define COLOR-top-txt   "white")
(define COLOR-dd-bg     "white")
(define COLOR-dd-bor    (make-color 180 180 180))
(define COLOR-board-a   (make-color 176 214 102))
(define COLOR-board-b   (make-color 164 204 94))
(define COLOR-cell-open (make-color 230 230 230))
(define COLOR-mine      "red")
(define COLOR-flag      "orange")
(define COLOR-bg        (make-color 245 245 245))

(define (color-numero n)
  (case n
    [(1) "blue"] [(2) "green"] [(3) "red"] [(4) "darkblue"]
    [(5) "brown"] [(6) "cyan"] [(7) "black"] [(8) "gray"]
    [else "black"]))

;----------------- CONFIGURACIONES DE TAMAÑO COMPLETAS ------------------
; Rangos permitidos: 8x8 a 15x15
(define MIN-BOARD-SIZE 8)
(define MAX-BOARD-SIZE 15)

; Mapeo de teclas a tamaños (8x8 a 15x15) - Actualizado según especificaciones
(define TAMAÑOS-TECLADO
  '(("1" . (8  8))   ; 8x8
    ("2" . (9  9))   ; 9x9
    ("3" . (10 10))  ; 10x10
    ("4" . (11 11))  ; 11x11
    ("5" . (12 12))  ; 12x12
    ("6" . (13 13))  ; 13x13
    ("7" . (14 14))  ; 14x14
    ("8" . (15 15)))) ; 15x15

; Niveles de dificultad
(define DIFICULTADES
  '((facil   . 0.10)   ; 10% de minas
    (medio   . 0.15)   ; 15% de minas
    (dificil . 0.20))) ; 20% de minas

(define (get-porcentaje-dificultad nivel)
  (cdr (assoc nivel DIFICULTADES)))

(define (get-tamaño-desde-tecla key)
  (define entrada (assoc key TAMAÑOS-TECLADO))
  (if entrada
      (values (first (cdr entrada)) (second (cdr entrada)))
      (values #f #f)))

;----------------- CÁLCULO DINÁMICO DE CELDA MEJORADO ------------------
(define (calcular-tamaño-celda cols rows)
  ; Calcular espacio disponible para el tablero
  (define max-board-width (- MAX-WINDOW-WIDTH (* 2 MARGIN)))
  (define max-board-height (- MAX-WINDOW-HEIGHT UI-EXTRA-HEIGHT))
  
  ; Calcular tamaño de celda basado en restricciones
  (define cell-by-width (floor (/ max-board-width cols)))
  (define cell-by-height (floor (/ max-board-height rows)))
  (define optimal-size (min cell-by-width cell-by-height))
  
  ; Aplicar límites mín/máx con ajuste mejorado para diferentes tamaños
  (define adjusted-size 
    (cond 
      [(and (>= cols 14) (>= rows 14)) (max 22 (min 30 optimal-size))] ; Tableros muy grandes
      [(and (>= cols 12) (>= rows 12)) (max 25 (min 35 optimal-size))] ; Tableros grandes
      [(and (>= cols 10) (>= rows 10)) (max 28 (min 40 optimal-size))] ; Tableros medianos
      [else (max MIN-CELL-SIZE (min MAX-CELL-SIZE optimal-size))])) ; Tableros pequeños
  
  adjusted-size)

(define (calcular-dimensiones-ventana cols rows cell-size)
  (define board-width (* cols cell-size))
  (define board-height (* rows cell-size))
  
  ; Añadir márgenes y espacio UI
  (define needed-width (+ board-width (* 2 MARGIN)))
  (define needed-height (+ board-height UI-EXTRA-HEIGHT))
  
  ; Asegurar que la ventana sea lo suficientemente grande para el contenido
  (define final-width (max MIN-WINDOW-WIDTH needed-width))
  (define final-height (max MIN-WINDOW-HEIGHT needed-height))
  
  ; Si excede los límites máximos, usar los máximos
  (define clamped-width (min final-width MAX-WINDOW-WIDTH))
  (define clamped-height (min final-height MAX-WINDOW-HEIGHT))
  
  (values clamped-width clamped-height))

;----------------- ESTADO DEL MUNDO SIMPLIFICADO ------------------
(struct world (cols rows mines dificultad tablero estado mensaje 
               cell-size window-width window-height) #:transparent)

(define (mk-world-desde-tamaño cols rows dificultad)
  (unless (and (>= cols MIN-BOARD-SIZE) (<= cols MAX-BOARD-SIZE)
               (>= rows MIN-BOARD-SIZE) (<= rows MAX-BOARD-SIZE))
    (error 'mk-world-desde-tamaño 
           "Las dimensiones deben estar entre ~a y ~a" MIN-BOARD-SIZE MAX-BOARD-SIZE))
  
  (define porcentaje-minas (get-porcentaje-dificultad dificultad))
  (define total-celdas (* cols rows))
  (define mines (max 1 (inexact->exact (floor (* total-celdas porcentaje-minas)))))
  
  (define tablero-juego (crear-tablero-personalizado rows cols porcentaje-minas))
  (define cell-size (calcular-tamaño-celda cols rows))
  (define-values (win-w win-h) (calcular-dimensiones-ventana cols rows cell-size))
  
  (world cols rows mines dificultad tablero-juego 'jugando 
         (format "~ax~a | ~a | Minas: ~a" cols rows 
                 (case dificultad
                   [(facil) "FÁCIL"]
                   [(medio) "MEDIO"] 
                   [(dificil) "DIFÍCIL"])
                 mines)
         cell-size win-w win-h))

; Mundo inicial por defecto (9x9 medio)
(define (mk-world-inicial)
  (mk-world-desde-tamaño 9 9 'medio))

;----------------- DROPDOWN MENU PARA DIFICULTAD ------------------
(define (dd-button-dificultad dificultad)
  (define label (case dificultad
                  [(facil) "FÁCIL (10%)"]
                  [(medio) "MEDIO (15%)"]
                  [(dificil) "DIFÍCIL (20%)"]))
  (overlay/align "left" "middle"
    (beside (text label 14 COLOR-top-txt)
            (rectangle 8 1 "solid" "transparent")
            (triangle 7 "solid" "white"))
    (rectangle 160 DD-H "solid" COLOR-top)))

(define (dd-menu-dificultad current)
  (define (row txt dificultad-key checked?)
    (overlay/align "left" "middle"
      (beside (rectangle 8 1 "solid" "transparent")
              (text txt 12 "black")
              (rectangle 6 1 "solid" "transparent")
              (if checked? 
                  (text "✓" 13 "forestgreen")
                  (rectangle 1 1 "solid" "transparent")))
      (rectangle 160 DD-ITEM-H "solid" COLOR-dd-bg)))
  
  (define menu-items
    (list (row "FÁCIL (10%)"   'facil   (eq? current 'facil))
          (row "MEDIO (15%)"   'medio   (eq? current 'medio))
          (row "DIFÍCIL (20%)" 'dificil (eq? current 'dificil))))
  
  (overlay/align "left" "top"
    (apply above menu-items)
    (rectangle 160 (* (length menu-items) DD-ITEM-H) "outline" COLOR-dd-bor)))

;-------------------- DIBUJO DE CELDAS OPTIMIZADO ---------------
(define (dibujar-celda celda x y alt? cell-size)
  (define base-color (if alt? COLOR-board-a COLOR-board-b))
  (define font-size (max 10 (min 24 (inexact->exact (floor (* cell-size 0.65))))))
  (define mine-radius (max 4 (min 15 (inexact->exact (floor (* cell-size 0.4))))))
  (define flag-size (max 10 (min 20 (inexact->exact (floor (* cell-size 0.6))))))
  
  (cond
    [(celda-descubierta? celda)
     (cond
       [(celda-mina? celda)
        (overlay (circle mine-radius "solid" COLOR-mine)
                 (rectangle cell-size cell-size "solid" COLOR-cell-open))]
       [(= (celda-vecinos celda) 0)
        (rectangle cell-size cell-size "solid" COLOR-cell-open)]
       [else
        (overlay (text (number->string (celda-vecinos celda)) 
                      font-size
                      (color-numero (celda-vecinos celda)))
                 (rectangle cell-size cell-size "solid" COLOR-cell-open))])]
    [(celda-marcada? celda)
     (overlay (text "🚩" flag-size COLOR-flag)
              (rectangle cell-size cell-size "solid" base-color))]
    [else
     (rectangle cell-size cell-size "solid" base-color)]))

;------------------- DIBUJO DEL TABLERO -------------------
(define (dibujar-tablero-completo w)
  (define tablero (world-tablero w))
  (define filas (tablero-filas tablero))
  (define columnas (tablero-columnas tablero))
  (define celdas (tablero-celdas tablero))
  (define cell-size (world-cell-size w))
  (dibujar-filas celdas columnas filas cell-size))

(define (dibujar-filas celdas columnas filas cell-size)
  (define (dibujar-fila y)
    (define (dibujar-celda-en-x x)
      (define idx-actual (idx x y columnas))
      (dibujar-celda (list-ref celdas idx-actual) x y (even? (+ x y)) cell-size))
    (apply beside (map dibujar-celda-en-x (range columnas))))
  (apply above (map dibujar-fila (range filas))))

;------------------- INTERFAZ SUPERIOR MEJORADA -------------------
(define (draw-top w)
  (define SCN-W (world-window-width w))
  (define dd (dd-button-dificultad (world-dificultad w)))
  (define base (rectangle SCN-W TOP-H "solid" COLOR-top))
  
  ; Colocar dropdown de dificultad
  (define with-dd
    (place-image dd
                 (+ MARGIN (/ (image-width dd) 2))
                 (/ TOP-H 2)
                 base))
  
  ; Título centrado con mejor posicionamiento
  (define with-title
    (place-image (text "BusCAMinas Pro" 20 COLOR-top-txt)
                 (/ SCN-W 2) (- (/ TOP-H 2) 12)
                 with-dd))
  
  ; Información del juego
  (define minas-restantes 
    (- (world-mines w) (tablero-contar-marcadas (world-tablero w))))
  (define info-text (format "~ax~a | ~a | Restantes: ~a" 
                            (world-cols w) (world-rows w) 
                            (case (world-dificultad w)
                              [(facil) "FÁCIL"]
                              [(medio) "MEDIO"] 
                              [(dificil) "DIFÍCIL"])
                            minas-restantes))
  
  (define with-info
    (place-image (text info-text 13 COLOR-top-txt)
                 (/ SCN-W 2) (+ (/ TOP-H 2) 8)
                 with-title))
  
  ; Controles en la esquina superior derecha
  (place-image (text "R: Reiniciar" 10 COLOR-top-txt)
               (- SCN-W 80) (- (/ TOP-H 2) 8)
               with-info))

;------------------- CONVERSIÓN DE COORDENADAS -------------------
(define (screen->cell w x y)
  (define cols (world-cols w))
  (define rows (world-rows w))
  (define cell-size (world-cell-size w))
  (define SCN-W (world-window-width w))
  
  (define BW (* cols cell-size))
  (define BH (* rows cell-size))
  (define left (- (/ SCN-W 2) (/ BW 2)))
  (define top (+ TOP-H MARGIN))
  
  (define cx (inexact->exact (floor (/ (- x left) cell-size))))
  (define cy (inexact->exact (floor (/ (- y top) cell-size))))
  
  (if (and (<= left x) (< x (+ left BW)) 
           (<= top y) (< y (+ top BH))
           (>= cx 0) (< cx cols)
           (>= cy 0) (< cy rows))
      (cons cx cy)
      #f))

;------------------- DIBUJO PRINCIPAL -------------------
(define (draw-world w)
  (define SCN-W (world-window-width w))
  (define SCN-H (world-window-height w))
  (define cell-size (world-cell-size w))
  (define cols (world-cols w))
  (define rows (world-rows w))
  
  (define BOARD-W (* cols cell-size))
  (define BOARD-H (* rows cell-size))
  
  (define board (dibujar-tablero-completo w))
  (define scene (rectangle SCN-W SCN-H "solid" COLOR-bg))
  (define with-top (place-image (draw-top w) (/ SCN-W 2) (/ TOP-H 2) scene))
  
  ; Centrar tablero
  (define board-x (/ SCN-W 2))
  (define board-y (+ TOP-H MARGIN (/ BOARD-H 2)))
  (define with-board (place-image board board-x board-y with-top))
  
  ; Mensajes adaptativos con mejor tamaño
  (define font-size (min 18 (max 12 (inexact->exact (floor (* cell-size 0.5))))))
  (define mensaje
    (cond
      [(eq? (world-estado w) 'victoria)
       (text "¡VICTORIA! Presiona R para reiniciar" font-size "green")]
      [(eq? (world-estado w) 'derrota)
       (text "¡DERROTA! Presiona R para reiniciar" font-size "red")]
      [else
       (text (world-mensaje w) (max 12 (- font-size 2)) "black")]))
  
  (define with-message
    (place-image mensaje (/ SCN-W 2) (- SCN-H 60) with-board))
  
  ; Controles mejorados
  (define controles1 (text "TAMAÑOS: 1(8x8) 2(9x9) 3(10x10) 4(11x11) 5(12x12) 6(13x13) 7(14x14) 8(15x15)" 11 "black"))
  (define controles2 (text "R: Reiniciar | Click Der: Descubrir | Click Izq: Bandera | Click menú: Dificultad" 11 "black"))
  
  (define with-controls1
    (place-image controles1 (/ SCN-W 2) (- SCN-H 35) with-message))
  
  (define with-controls2
    (place-image controles2 (/ SCN-W 2) (- SCN-H 20) with-controls1))
  
  ; Menú dropdown si está abierto
  with-controls2)

;------------------- DETECCIÓN DE CLICS PARA DIFICULTAD -------------------
(define (hit-dd-dificultad? x y)
  (pt-in-rect? x y MARGIN 12 160 DD-H))

(define (hit-dd-dificultad-item x y)
  (define rx MARGIN)
  (define ry (+ 12 DD-H 8))
  (define items '(facil medio dificil))
  (define (check-item idx)
    (if (< idx (length items))
        (if (pt-in-rect? x y rx (+ ry (* idx DD-ITEM-H)) 160 DD-ITEM-H)
            (list-ref items idx)
            (check-item (+ idx 1)))
        #f))
  (check-item 0))

(define (pt-in-rect? x y rx ry rw rh)
  (and (<= rx x) (< x (+ rx rw)) 
       (<= ry y) (< y (+ ry rh))))

;; === NEW: detector genérico de botón derecho (si el backend lo reporta) ===
(define (right-click? me)
  (and (string? me) (string-contains? me "right")))

;------------------- MANEJO DE EVENTOS -------------------
(define (handle-mouse w x y me)
  (if (member (world-estado w) '(victoria derrota))
      w
      (handle-mouse-activo w x y me)))

(define (handle-mouse-activo w x y me)
  (define p (screen->cell w x y))
  (cond
    ; Cambiar dificultad y reiniciar con el mismo tamaño
    [(and (string=? me "button-down") (hit-dd-dificultad? x y))
     (define nueva-dificultad 
       (case (world-dificultad w)
         [(facil) 'medio]
         [(medio) 'dificil]
         [(dificil) 'facil]))
     (mk-world-desde-tamaño (world-cols w) (world-rows w) nueva-dificultad)]
    
    ; CLICK DERECHO => DESCUBRIR (si el entorno entrega "right-...")
    [(and p (right-click? me))
     (handle-left-click w (car p) (cdr p))]
    
    ; CLICK IZQUIERDO => BANDERA
    [(and (string=? me "button-down") p)
     (handle-right-click w (car p) (cdr p))]
    
    [else w]))

; Manejo de clics izquierdo y derecho
(define (handle-left-click w cx cy)
  (define tb-actual (world-tablero w))
  (define celda-actual (tablero-obtener-celda-xy tb-actual cx cy))
  
  (cond
    [(not celda-actual) w]
    [(celda-marcada? celda-actual) w]
    [(celda-descubierta? celda-actual) w]
    [else
     (define tb-nuevo (descubrir-celda tb-actual cx cy))
     (define nuevo-mundo (struct-copy world w [tablero tb-nuevo]))
     
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
                    [mensaje (format "~ax~a | ~a | Restantes: ~a" 
                                   (world-cols w) (world-rows w)
                                   (case (world-dificultad w)
                                     [(facil) "FÁCIL"]
                                     [(medio) "MEDIO"] 
                                     [(dificil) "DIFÍCIL"])
                                   (- (world-mines w)
                                      (tablero-contar-marcadas tb-nuevo)))])])]))

(define (handle-right-click w cx cy)
  (define tb-actual (world-tablero w))
  (define celda-actual (tablero-obtener-celda-xy tb-actual cx cy))
  
  (cond
    [(not celda-actual) w]
    [(celda-descubierta? celda-actual) w]
    [else
     (define tb-nuevo (marcar-celda tb-actual cx cy))
     (struct-copy world w 
                 [tablero tb-nuevo]
                 [mensaje (format "~ax~a | ~a | Restantes: ~a" 
                               (world-cols w) (world-rows w)
                               (case (world-dificultad w)
                                 [(facil) "FÁCIL"]
                                 [(medio) "MEDIO"] 
                                 [(dificil) "DIFÍCIL"])
                               (- (world-mines w)
                                  (tablero-contar-marcadas tb-nuevo)))])]))

;------------------- MANEJO DE TECLADO MEJORADO -------------------
(define (handle-key w key)
  (cond
    ; Reiniciar juego con R
    [(key=? key "r") (mk-world-desde-tamaño (world-cols w) (world-rows w) (world-dificultad w))]
    
    ; Tamaños configurables (1-8)
    [else
     (define-values (cols rows) (get-tamaño-desde-tecla key))
     (if (and cols rows)
         (mk-world-desde-tamaño cols rows (world-dificultad w))
         w)]))

;------------------- FUNCIÓN PRINCIPAL -------------------
(define (main)
  (define initial-world (mk-world-inicial))
  (big-bang initial-world
    (to-draw  draw-world 
              (world-window-width initial-world) 
              (world-window-height initial-world))
    (on-mouse handle-mouse)
    (on-key   handle-key)
    (name     "BusCAMinas Pro - Escalable y Configurable")))

;------------------- FUNCIONES DE UTILIDAD -------------------
; Mostrar información sobre tamaños y controles
(define (mostrar-controles)
  (printf "=== CONTROLES BUSCAMINAS PRO ===~n")
  (printf "TAMAÑOS DE TABLERO:~n")
  (printf "  1 → 8x8   | 2 → 9x9   | 3 → 10x10 | 4 → 11x11~n")
  (printf "  5 → 12x12 | 6 → 13x13 | 7 → 14x14 | 8 → 15x15~n")
  (printf "~n")
  (printf "DIFICULTADES:~n")
  (printf "  FÁCIL: 10%% de minas~n")
  (printf "  MEDIO: 15%% de minas~n")
  (printf "  DIFÍCIL: 20%% de minas~n")
  (printf "~n")
  (printf "CONTROLES:~n")
  (printf "  Click Izquierdo: Marcar/desmarcar bandera~n")
  (printf "  Click Derecho: Descubrir celda~n")
  (printf "  R: Reiniciar juego~n")
  (printf "  Click en menú: Cambiar dificultad~n"))

; Mostrar información de mundo actual
(define (debug-world-info w)
  (printf "=== ESTADO ACTUAL ===~n")
  (printf "Tamaño: ~ax~a~n" (world-cols w) (world-rows w))
  (printf "Dificultad: ~a~n" (world-dificultad w))
  (printf "Tamaño celda: ~a~n" (world-cell-size w))
  (printf "Ventana: ~ax~a~n" (world-window-width w) (world-window-height w))
  (printf "Minas totales: ~a~n" (world-mines w))
  (printf "Estado: ~a~n" (world-estado w)))

;------------------- INICIALIZACIÓN -------------------
; Mostrar controles disponibles
(mostrar-controles)

; Iniciar el juego
(main)
