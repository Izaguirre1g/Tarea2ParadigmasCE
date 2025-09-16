#lang racket
(require 2htdp/universe 2htdp/image
         "../Logica/Logica.rkt")

; ================== UI Buscaminas Escalable y Configurable ==================
; Este archivo implementa ÚNICAMENTE la interfaz (renderizado y manejo de eventos).
; Toda la lógica de juego (estructuras, descubrir, marcar, victoria/derrota, etc.)
; se encuentra en "../Logica/Logica.rkt" y aquí solo la invocamos.

; ---- Constantes de UI Dinámicas ----
(define BASE-CELL-SIZE 32)     ; Tamaño base de celda (referencia para escalado)
(define MIN-CELL-SIZE 20)      ; Tamaño mínimo de celda permitido
(define MAX-CELL-SIZE 50)      ; Tamaño máximo de celda permitido
(define TOP-H 80)              ; Altura de la barra superior (título/controles)
(define MARGIN 20)             ; Margen exterior alrededor del tablero
(define DD-W 140)              ; (reservado) Ancho de botón "dropdown" (visual)
(define DD-H 35)               ; (reservado) Alto de botón "dropdown"
(define DD-ITEM-H 32)          ; (reservado) Alto de cada item de dropdown

; Límites de ventana (para acomodar tableros grandes sin salirse de pantalla)
(define MAX-WINDOW-WIDTH 1800)
(define MAX-WINDOW-HEIGHT 1400)
(define MIN-WINDOW-WIDTH 600)
(define MIN-WINDOW-HEIGHT 550)
(define UI-EXTRA-HEIGHT (+ TOP-H (* 2 MARGIN) 120)) ; Espacio extra (mensajes/controles)

; Colores (paleta de la interfaz)
(define COLOR-top       (make-color 60 100 30)) ; Barra superior
(define COLOR-top-txt   "white")                ; Texto de barra superior
(define COLOR-dd-bg     "white")                ; Fondo hipotético de dropdown
(define COLOR-dd-bor    (make-color 180 180 180)) ; Borde hipotético de dropdown
(define COLOR-board-a   (make-color 176 214 102)) ; Color casilla (patrón ajedrez A)
(define COLOR-board-b   (make-color 164 204 94))  ; Color casilla (patrón ajedrez B)
(define COLOR-cell-open (make-color 230 230 230)) ; Fondo de celda descubierta
(define COLOR-mine      "black")                 ; Color de la mina dibujada
(define COLOR-flag      "red")                   ; Color/emoji de la bandera
(define COLOR-bg        (make-color 245 245 245)) ; Fondo de la escena

; Color para los números de vecinos (1..8) al estilo clásico de Buscaminas
(define (color-numero n)
  (case n
    [(1) "blue"] [(2) "green"] [(3) "red"] [(4) "darkblue"]
    [(5) "brown"] [(6) "cyan"] [(7) "black"] [(8) "gray"]
    [else "black"]))

;----------------- CONFIGURACIONES DE TAMAÑO ------------------
(define MIN-BOARD-SIZE 8)   ; Número mínimo de filas/columnas permitido
(define MAX-BOARD-SIZE 15)  ; Número máximo de filas/columnas permitido

; Mapa: teclas rápidas → (columnas filas) para cambiar tamaño de tablero
(define TAMAÑOS-TECLADO
  '(("1" . (8  8))
    ("2" . (9  9))
    ("3" . (10 10))
    ("4" . (11 11))
    ("5" . (12 12))
    ("6" . (13 13))
    ("7" . (14 14))
    ("8" . (15 15))))

; Dificultades y su % de minas relativo al total de celdas
(define DIFICULTADES
  '((facil   . 0.10)
    (medio   . 0.15)
    (dificil . 0.20)))

; Devuelve el porcentaje de minas asociado a un símbolo de dificultad
(define (get-porcentaje-dificultad nivel)
  (cdr (assoc nivel DIFICULTADES)))

; Traduce una tecla (string) a (cols, rows) si existe en TAMAÑOS-TECLADO
(define (get-tamaño-desde-tecla key)
  (define entrada (assoc key TAMAÑOS-TECLADO))
  (if entrada
      (values (first (cdr entrada)) (second (cdr entrada)))
      (values #f #f))) ; Si no hay mapeo, devuelve #f #f

;----------------- CÁLCULO DINÁMICO DE CELDA ------------------
; Calcula el tamaño de cada celda para que el tablero quepa en la ventana
; respetando límites mínimos/máximos y márgenes.
(define (calcular-tamaño-celda cols rows)
  (define max-board-width (- MAX-WINDOW-WIDTH (* 2 MARGIN)))       ; Ancho disponible
  (define max-board-height (- MAX-WINDOW-HEIGHT UI-EXTRA-HEIGHT))  ; Alto disponible
  (define cell-by-width (floor (/ max-board-width cols)))          ; Tamaño por ancho
  (define cell-by-height (floor (/ max-board-height rows)))        ; Tamaño por alto
  (define optimal-size (min cell-by-width cell-by-height))         ; El más restrictivo
  ; Ajuste fino según tamaño del tablero para mejorar legibilidad
  (define adjusted-size 
    (cond 
      [(and (>= cols 14) (>= rows 14)) (max 22 (min 30 optimal-size))]
      [(and (>= cols 12) (>= rows 12)) (max 25 (min 35 optimal-size))]
      [(and (>= cols 10) (>= rows 10)) (max 28 (min 40 optimal-size))]
      [else (max MIN-CELL-SIZE (min MAX-CELL-SIZE optimal-size))]))
  adjusted-size)

; Calcula el tamaño de la ventana recomendado a partir del tablero y la celda
(define (calcular-dimensiones-ventana cols rows cell-size)
  (define board-width (* cols cell-size))    ; Ancho del tablero en px
  (define board-height (* rows cell-size))   ; Alto del tablero en px
  (define needed-width (+ board-width (* 2 MARGIN)))     ; Ancho necesario con márgenes
  (define needed-height (+ board-height UI-EXTRA-HEIGHT)) ; Alto necesario con UI
  ; Forzamos mínimos de ventana
  (define final-width (max MIN-WINDOW-WIDTH needed-width))
  (define final-height (max MIN-WINDOW-HEIGHT needed-height))
  ; Limitamos a máximos de ventana
  (define clamped-width (min final-width MAX-WINDOW-WIDTH))
  (define clamped-height (min final-height MAX-WINDOW-HEIGHT))
  (values clamped-width clamped-height))

;----------------- ESTADO DEL MUNDO (interfaz) ------------------
; flag-left? controla QUÉ hace el clic IZQUIERDO:
;   #t → IZQ coloca bandera (por defecto ideal si no hay botón derecho real)
;   #f → IZQ descubre (útil si quieres usar el izquierdo para descubrir)
(struct world (cols rows mines dificultad tablero estado mensaje 
               cell-size window-width window-height flag-left?) #:transparent)

; Construye un mundo nuevo con dimensiones y dificultad dadas
(define (mk-world-desde-tamaño cols rows dificultad)
  ; Validación de dimensiones soportadas por la UI
  (unless (and (>= cols MIN-BOARD-SIZE) (<= cols MAX-BOARD-SIZE)
               (>= rows MIN-BOARD-SIZE) (<= rows MAX-BOARD-SIZE))
    (error 'mk-world-desde-tamaño 
           "Las dimensiones deben estar entre ~a y ~a" MIN-BOARD-SIZE MAX-BOARD-SIZE))
  ; Cálculo de minas según dificultad
  (define porcentaje-minas (get-porcentaje-dificultad dificultad))
  (define total-celdas (* cols rows))
  (define mines (max 1 (inexact->exact (floor (* total-celdas porcentaje-minas)))))
  ; Pedimos a la LÓGICA que cree un tablero ya inicializado (minas + conteos)
  (define tablero-juego (crear-tablero-personalizado rows cols porcentaje-minas))
  ; Calculamos tamaño de celda y ventana apropiados
  (define cell-size (calcular-tamaño-celda cols rows))
  (define-values (win-w win-h) (calcular-dimensiones-ventana cols rows cell-size))
  ; Construimos y devolvemos el estado (mundo)
  (world cols rows mines dificultad tablero-juego 'jugando 
         (format "~ax~a | ~a | Minas: ~a" cols rows 
                 (case dificultad
                   [(facil) "FÁCIL"]
                   [(medio) "MEDIO"] 
                   [(dificil) "DIFÍCIL"])
                 mines)
         cell-size win-w win-h
         #t)) ; flag-left? por defecto: IZQ coloca bandera

; Mundo inicial por omisión (9x9, dificultad media)
(define (mk-world-inicial)
  (mk-world-desde-tamaño 9 9 'medio))

;----------------- UI: Botón visual de “dropdown” de dificultad ------------------
; Dibuja un rectángulo/título con un triángulo, solo representativo.
; El cambio real de dificultad se activa con un hit-test en el área del botón.
(define (dd-button-dificultad dificultad)
  (define label (case dificultad
                  [(facil) "FÁCIL (10%)"]
                  [(medio) "MEDIO (15%)"]
                  [(dificil) "DIFÍCIL (20%)"]))
  (overlay/align "left" "middle"
    (beside (text label 14 COLOR-top-txt)
            (rectangle 8 1 "solid" "transparent")
            (triangle 7 "solid" "white")) ; Triángulo como indicador de lista
    (rectangle 160 DD-H "solid" COLOR-top)))

;-------------------- DIBUJO DE CELDAS ------------------------
; Dibuja una celda: fondo (según paridad A/B), y:
;  - si está descubierta:
;      * mina → círculo negro
;      * 0 vecinos → celda gris clara
;      * >0 vecinos → número coloreado
;  - si está marcada → bandera roja (emoji)
;  - si está oculta → color A/B
(define (dibujar-celda celda x y alt? cell-size)
  (define base-color (if alt? COLOR-board-a COLOR-board-b)) ; Patrón ajedrez
  (define font-size (max 10 (min 24 (inexact->exact (floor (* cell-size 0.65))))))
  (define mine-radius (max 4 (min 15 (inexact->exact (floor (* cell-size 0.4))))))
  (define flag-size (max 10 (min 20 (inexact->exact (floor (* cell-size 0.6))))))
  (cond
    [(celda-descubierta? celda)
     (cond
       [(celda-mina? celda)
        ; Mina: círculo sólido sobre fondo de celda descubierta
        (overlay (circle mine-radius "solid" COLOR-mine)
                 (rectangle cell-size cell-size "solid" COLOR-cell-open))]
       [(= (celda-vecinos celda) 0)
        ; Sin minas vecinas: solo fondo descubierto
        (rectangle cell-size cell-size "solid" COLOR-cell-open)]
       [else
        ; Con minas vecinas: número con color tradicional
        (overlay (text (number->string (celda-vecinos celda)) 
                      font-size
                      (color-numero (celda-vecinos celda)))
                 (rectangle cell-size cell-size "solid" COLOR-cell-open))])]
    [(celda-marcada? celda)
     ; Bandera en celdas ocultas marcadas
     (overlay (text "🚩" flag-size COLOR-flag)
              (rectangle cell-size cell-size "solid" base-color))]
    [else
     ; Celda oculta sin marcar
     (rectangle cell-size cell-size "solid" base-color)]))

; Dibuja el tablero completo componiendo filas y celdas
(define (dibujar-tablero-completo w)
  (define tablero (world-tablero w))
  (define filas (tablero-filas tablero))
  (define columnas (tablero-columnas tablero))
  (define celdas (tablero-celdas tablero))
  (define cell-size (world-cell-size w))
  (define (dibujar-fila y)
    (define (dibujar-celda-en-x x)
      (define idx-actual (idx x y columnas)) ; (x,y) → índice lineal
      (dibujar-celda (list-ref celdas idx-actual) x y (even? (+ x y)) cell-size))
    (apply beside (map dibujar-celda-en-x (range columnas))))
  (apply above (map dibujar-fila (range filas))))

;------------------- INTERFAZ SUPERIOR -------------------
; Dibuja la barra superior con:
;   - botón visual de dificultad
;   - título
;   - info de minas restantes (minas totales - marcadas)
;   - atajo de reinicio “R”
(define (draw-top w)
  (define SCN-W (world-window-width w))
  (define dd (dd-button-dificultad (world-dificultad w)))
  (define base (rectangle SCN-W TOP-H "solid" COLOR-top))
  (define with-dd
    (place-image dd (+ MARGIN (/ (image-width dd) 2)) (/ TOP-H 2) base))
  (define with-title
    (place-image (text "BusCEMinas" 20 COLOR-top-txt)
                 (/ SCN-W 2) (- (/ TOP-H 2) 12) with-dd))
  ; Minas restantes = minas totales - celdas marcadas
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
                 (/ SCN-W 2) (+ (/ TOP-H 2) 8) with-title))
  (place-image (text "R: Reiniciar" 10 COLOR-top-txt)
               (- SCN-W 80) (- (/ TOP-H 2) 8) with-info))

;------------------- CONVERSIÓN DE COORDENADAS -------------------
; Convierte coordenadas de pantalla (x,y) a coordenadas de celda (cx,cy).
; Devuelve (cons cx cy) si (x,y) cae dentro del tablero, y #f en caso contrario.
(define (screen->cell w x y)
  (define cols (world-cols w))
  (define rows (world-rows w))
  (define cell-size (world-cell-size w))
  (define SCN-W (world-window-width w))
  (define BW (* cols cell-size)) ; ancho del tablero en px
  (define BH (* rows cell-size)) ; alto del tablero en px
  (define left (- (/ SCN-W 2) (/ BW 2))) ; borde izquierdo del tablero
  (define top (+ TOP-H MARGIN))           ; borde superior del tablero
  (define cx (inexact->exact (floor (/ (- x left) cell-size))))
  (define cy (inexact->exact (floor (/ (- y top) cell-size))))
  (if (and (<= left x) (< x (+ left BW)) 
           (<= top y) (< y (+ top BH))
           (>= cx 0) (< cx cols)
           (>= cy 0) (< cy rows))
      (cons cx cy)
      #f))

;------------------- DIBUJO PRINCIPAL -------------------
; Compone la escena completa: fondo, barra superior, tablero, mensajes y ayudas.
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
  ; Centramos el tablero en la parte inferior de la barra superior
  (define board-x (/ SCN-W 2))
  (define board-y (+ TOP-H MARGIN (/ BOARD-H 2)))
  (define with-board (place-image board board-x board-y with-top))
  ; Mensajes de estado adaptados al tamaño de celda
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
  ; Ayudas/atajos en la parte inferior
  (define controles1
    (text "TAMAÑOS: 1(8x8) 2(9x9) 3(10x10) 4(11x11) 5(12x12) 6(13x13) 7(14x14) 8(15x15)" 11 "black"))
  (define controles2
    (text (string-append
           "R: Reiniciar | Click Der: Descubrir | Click Izq: Bandera"
           " | F: Alternar IZQ (Bandera/Descubrir) | Menú: Dificultad")
          11 "black"))
  (define with-controls1
    (place-image controles1 (/ SCN-W 2) (- SCN-H 35) with-message))
  (define with-controls2
    (place-image controles2 (/ SCN-W 2) (- SCN-H 20) with-controls1))
  with-controls2)

;------------------- HIT-TESTS -------------------
; Utilidad geométrica: ¿un punto (x,y) cae dentro del rectángulo (rx,ry,rw,rh)?
(define (pt-in-rect? x y rx ry rw rh)
  (and (<= rx x) (< x (+ rx rw)) 
       (<= ry y) (< y (+ ry rh))))

; Área clickeable del “botón” de dificultad (arriba a la izquierda)
(define (hit-dd-dificultad? x y)
  (pt-in-rect? x y MARGIN 12 160 DD-H))

; ----- Detección genérica de botón derecho -----
; 2htdp/universe en muchos entornos NO reporta botón derecho;
; si 'me' contiene "right" asumimos que fue un click derecho real.
(define (right-click? me)
  (and (string? me) (string-contains? me "right")))

;------------------- MANEJO DE EVENTOS (MOUSE) -------------------
; Desvía a handle-mouse-activo a menos que el juego ya este terminado
(define (handle-mouse w x y me)
  (if (member (world-estado w) '(victoria derrota))
      w
      (handle-mouse-activo w x y me)))

; Lógica de interacción con el mouse:
;  - Click sobre el botón de dificultad: cicla dificultad y reinicia manteniendo tamaño
;  - Click derecho real (si se detecta): DESCUBRIR celda
;  - Click izquierdo:
;       * si flag-left? = #t → PONER/QUITAR BANDERA
;       * si flag-left? = #f → DESCUBRIR
(define (handle-mouse-activo w x y me)
  (define p (screen->cell w x y)) ; Traducimos (x,y) a celda (cx,cy) o #f
  (cond
    ;; Menú de dificultad cíclico
    [(and (string=? me "button-down") (hit-dd-dificultad? x y))
     (define nueva-dificultad 
       (case (world-dificultad w)
         [(facil) 'medio] [(medio) 'dificil] [(dificil) 'facil]))
     (mk-world-desde-tamaño (world-cols w) (world-rows w) nueva-dificultad)]

    ;; CLICK DERECHO real => descubrir (si el backend lo reporta)
    [(and p (right-click? me))
     (handle-left-click w (car p) (cdr p))]

    ;; CLICK IZQUIERDO => según flag-left? (por defecto, bandera)
    [(and (string=? me "button-down") p)
     (if (world-flag-left? w)
         (handle-right-click w (car p) (cdr p)) ; bandera
         (handle-left-click  w (car p) (cdr p)))] ; descubrir

    [else w]))

; --- Acciones que llaman a la LÓGICA de juego ---

; Descubrir celda (respeta reglas en la lógica: no descubre marcadas/ya abiertas)
(define (handle-left-click w cx cy)
  (define tb-actual (world-tablero w))
  (define celda-actual (tablero-obtener-celda-xy tb-actual cx cy))
  (cond
    [(not celda-actual) w]                 ; Click fuera o índice inválido
    [(celda-marcada? celda-actual) w]      ; No se descubre si está marcada
    [(celda-descubierta? celda-actual) w]  ; Ni si ya está descubierta
    [else
     (define tb-nuevo (descubrir-celda tb-actual cx cy))       ; Lógica: descubrir
     (define nuevo-mundo (struct-copy world w [tablero tb-nuevo]))
     (cond
       [(verificar-derrota? tb-nuevo) ; Si se descubrió una mina → derrota
        (struct-copy world nuevo-mundo 
                     [estado 'derrota]
                     [mensaje "¡Has perdido! Presiona R para reiniciar"])]
       [(verificar-victoria? tb-nuevo) ; Si todas las no-minas están abiertas → victoria
        (struct-copy world nuevo-mundo 
                     [estado 'victoria]
                     [mensaje "¡Has ganado! Presiona R para reiniciar"])]
       [else
        ; Continuamos jugando: actualizamos mensaje con minas restantes
        (struct-copy world nuevo-mundo
                     [mensaje (format "~ax~a | ~a | Restantes: ~a" 
                                      (world-cols w) (world-rows w)
                                      (case (world-dificultad w)
                                        [(facil) "FÁCIL"]
                                        [(medio) "MEDIO"] 
                                        [(dificil) "DIFÍCIL"])
                                      (- (world-mines w)
                                         (tablero-contar-marcadas tb-nuevo)))])])]))

; Marcar/Desmarcar bandera (la lógica impide marcar si ya está descubierta)
(define (handle-right-click w cx cy)
  (define tb-actual (world-tablero w))
  (define celda-actual (tablero-obtener-celda-xy tb-actual cx cy))
  (cond
    [(not celda-actual) w]                ; Click fuera o índice inválido
    [(celda-descubierta? celda-actual) w] ; No marcar si ya está descubierta
    [else
     (define tb-nuevo (marcar-celda tb-actual cx cy)) ; Lógica: alternar marcado
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

;------------------- TECLADO -------------------
; Atajos:
;   - "r": reiniciar con mismo tamaño y dificultad
;   - "f": alterna el comportamiento del CLIC IZQUIERDO (bandera ↔ descubrir)
;   - "1".."8": cambian el tamaño del tablero con la dificultad actual
(define (handle-key w key)
  (cond
    [(key=? key "r")
     (mk-world-desde-tamaño (world-cols w) (world-rows w) (world-dificultad w))]

    ;; Alternar mapeo del clic izquierdo (fallback universal)
    [(key=? key "f")
     (struct-copy world w
                  [flag-left? (not (world-flag-left? w))]
                  [mensaje (if (world-flag-left? w)
                               "IZQ ahora DESCUBRE (F para volver a Bandera)"
                               "IZQ ahora BANDERA (F para descubrir)")])]

    [else
     (define-values (cols rows) (get-tamaño-desde-tecla key))
     (if (and cols rows)
         (mk-world-desde-tamaño cols rows (world-dificultad w))
         w)]))

;------------------- MAIN -------------------
; Lanza el mundo con big-bang. Se fija el tamaño de ventana en base al mundo inicial.
(define (main)
  (define initial-world (mk-world-inicial))
  (big-bang initial-world
    (to-draw  draw-world 
              (world-window-width initial-world) 
              (world-window-height initial-world))
    (on-mouse handle-mouse)
    (on-key   handle-key)
    (name     "BusCEMinas")))

;------------------- AUX: Ayuda en consola -------------------
; Imprime en la consola un resumen de controles y atajos.
(define (mostrar-controles)
  (printf "=== CONTROLES BusCEMinas ===~n")
  (printf "TAMAÑOS: 1→8x8 2→9x9 3→10x10 4→11x11 5→12x12 6→13x13 7→14x14 8→15x15~n~n")
  (printf "DIFICULTADES: FÁCIL 10%% | MEDIO 15%% | DIFÍCIL 20%%~n~n")
  (printf "CLICK IZQ: Bandera (por defecto) — presiona F para alternar a Descubrir~n")
  (printf "CLICK DER (si tu entorno lo reporta): Descubrir~n")
  (printf "R: Reiniciar | F: Alternar función del click ~n"))

; Mostrar ayuda e iniciar el juego
(mostrar-controles)
(main)


