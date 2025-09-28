#lang racket
(require 2htdp/universe 2htdp/image
         "../Logica/Logica.rkt")

; ================== UI Buscaminas Escalable y Configurable ==================
; Este archivo implementa UNICAMENTE la interfaz (renderizado y manejo de eventos).
; Toda la logica de juego (estructuras, descubrir, marcar, victoria/derrota, etc.)
; se encuentra en "../Logica/Logica.rkt" y aqui solo la invocamos.

; ---- Constantes de UI Dinamicas ----
(define BASE-CELL-SIZE 32)     ; Tamano base de celda (referencia para escalado)
(define MIN-CELL-SIZE 20)      ; Tamano minimo de celda permitido
(define MAX-CELL-SIZE 50)      ; Tamano maximo de celda permitido
(define TOP-H 80)              ; Altura de la barra superior (titulo/controles)
(define MARGIN 20)             ; Margen exterior alrededor del tablero
(define DD-W 140)              ; (reservado) Ancho de boton "dropdown" (visual)
(define DD-H 35)               ; (reservado) Alto de boton "dropdown"
(define DD-ITEM-H 32)          ; (reservado) Alto de cada item de dropdown

; Limites de ventana (para acomodar tableros grandes sin salirse de pantalla)
(define MAX-WINDOW-WIDTH 1800)
(define MAX-WINDOW-HEIGHT 1400)
(define MIN-WINDOW-WIDTH 600)
(define MIN-WINDOW-HEIGHT 550)
(define UI-EXTRA-HEIGHT (+ TOP-H (* 2 MARGIN) 120)) ; Espacio extra (mensajes/controles)

; Colores (paleta de la interfaz)
(define COLOR-top       (make-color 60 100 30)) ; Barra superior
(define COLOR-top-txt   "white")                ; Texto de barra superior
(define COLOR-dd-bg     "white")                ; Fondo hipotetico de dropdown
(define COLOR-dd-bor    (make-color 180 180 180)) ; Borde hipotetico de dropdown
(define COLOR-board-a   (make-color 176 214 102)) ; Color casilla (patron ajedrez A)
(define COLOR-board-b   (make-color 164 204 94))  ; Color casilla (patron ajedrez B)
(define COLOR-cell-open (make-color 230 230 230)) ; Fondo de celda descubierta
(define COLOR-mine      "black")                 ; Color de la mina dibujada
(define COLOR-flag      "red")                   ; Color/emoji de la bandera
(define COLOR-bg        (make-color 245 245 245)) ; Fondo de la escena

; Color para los numeros de vecinos (1..8) al estilo clasico de Buscaminas
(define (color-numero n)
  (case n
    [(1) "blue"] [(2) "green"] [(3) "red"] [(4) "darkblue"]
    [(5) "brown"] [(6) "cyan"] [(7) "black"] [(8) "gray"]
    [else "black"]))

;----------------- CONFIGURACIONES DE TAMANO ------------------
(define MIN-BOARD-SIZE 8)   ; Numero minimo de filas/columnas permitido
(define MAX-BOARD-SIZE 15)  ; Numero maximo de filas/columnas permitido

; Mapa: teclas rapidas -> (columnas filas) para cambiar tamano de tablero
(define TAMANOS-TECLADO
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

; Devuelve el porcentaje de minas asociado a un simbolo de dificultad
(define (get-porcentaje-dificultad nivel)
  (cdr (assoc nivel DIFICULTADES)))

; Traduce una tecla (string) a (cols, rows) si existe en TAMANOS-TECLADO
(define (get-tamano-desde-tecla key)
  (define entrada (assoc key TAMANOS-TECLADO))
  (if entrada
      (values (first (cdr entrada)) (second (cdr entrada)))
      (values #f #f))) ; Si no hay mapeo, devuelve #f #f

;----------------- CALCULO DINAMICO DE CELDA ------------------
; Calcula el tamano de cada celda para que el tablero quepa en la ventana
; respetando limites minimos/maximos y margenes.
(define (calcular-tamano-celda cols rows)
  (define max-board-width (- MAX-WINDOW-WIDTH (* 2 MARGIN)))       ; Ancho disponible
  (define max-board-height (- MAX-WINDOW-HEIGHT UI-EXTRA-HEIGHT))  ; Alto disponible
  (define cell-by-width (floor (/ max-board-width cols)))          ; Tamano por ancho
  (define cell-by-height (floor (/ max-board-height rows)))        ; Tamano por alto
  (define optimal-size (min cell-by-width cell-by-height))         ; El mas restrictivo
  ; Ajuste fino segun tamano del tablero para mejorar legibilidad
  (define adjusted-size 
    (cond 
      [(and (>= cols 14) (>= rows 14)) (max 22 (min 30 optimal-size))]
      [(and (>= cols 12) (>= rows 12)) (max 25 (min 35 optimal-size))]
      [(and (>= cols 10) (>= rows 10)) (max 28 (min 40 optimal-size))]
      [else (max MIN-CELL-SIZE (min MAX-CELL-SIZE optimal-size))]))
  adjusted-size)

; Calcula el tamano de la ventana recomendado a partir del tablero y la celda
(define (calcular-dimensiones-ventana cols rows cell-size)
  (define board-width (* cols cell-size))    ; Ancho del tablero en px
  (define board-height (* rows cell-size))   ; Alto del tablero en px
  (define needed-width (+ board-width (* 2 MARGIN)))     ; Ancho necesario con margenes
  (define needed-height (+ board-height UI-EXTRA-HEIGHT)) ; Alto necesario con UI
  ; Forzamos minimos de ventana
  (define final-width (max MIN-WINDOW-WIDTH needed-width))
  (define final-height (max MIN-WINDOW-HEIGHT needed-height))
  ; Limitamos a maximos de ventana
  (define clamped-width (min final-width MAX-WINDOW-WIDTH))
  (define clamped-height (min final-height MAX-WINDOW-HEIGHT))
  (values clamped-width clamped-height))

;----------------- ESTADO DEL MUNDO (interfaz) ------------------
; - inicio-tiempo: momento en que comenzo el juego (en segundos)
; - tiempo-juego: tiempo transcurrido (se actualiza solo cuando esta jugando)
; - primer-click?: indica si aun no se ha hecho el primer click
(struct world (cols rows mines dificultad tablero estado mensaje 
               cell-size window-width window-height flag-left?
               inicio-tiempo tiempo-juego primer-click?) #:transparent)

; Construye un mundo nuevo con dimensiones y dificultad dadas
(define (mk-world-desde-tamano cols rows dificultad)
  ; Validacion de dimensiones soportadas por la UI
  (unless (and (>= cols MIN-BOARD-SIZE) (<= cols MAX-BOARD-SIZE)
               (>= rows MIN-BOARD-SIZE) (<= rows MAX-BOARD-SIZE))
    (error 'mk-world-desde-tamano 
           "Las dimensiones deben estar entre ~a y ~a" MIN-BOARD-SIZE MAX-BOARD-SIZE))
  ; Calculo de minas segun dificultad
  (define porcentaje-minas (get-porcentaje-dificultad dificultad))
  (define total-celdas (* cols rows))
  (define mines (max 1 (inexact->exact (floor (* total-celdas porcentaje-minas)))))
  
  ; CAMBIO: Creamos un tablero VACIO inicialmente para el primer click seguro
  (define tablero-vacio (crear-tablero-vacio rows cols dificultad))
  
  ; Calculamos tamano de celda y ventana apropiados
  (define cell-size (calcular-tamano-celda cols rows))
  (define-values (win-w win-h) (calcular-dimensiones-ventana cols rows cell-size))
  ; Construimos y devolvemos el estado (mundo)
  (world cols rows mines dificultad tablero-vacio 'jugando 
         (format "~ax~a | ~a | Minas: ~a" cols rows 
                 (case dificultad
                   [(facil) "FACIL"]
                   [(medio) "MEDIO"] 
                   [(dificil) "DIFICIL"])
                 mines)
         cell-size win-w win-h
         #t ; flag-left? por defecto: IZQ coloca bandera
         (current-seconds) ; inicio-tiempo: momento actual
         0 ; tiempo-juego: inicia en 0
         #t)) ; primer-click?: si, aun no se ha hecho el primer click

; Mundo inicial por omision (9x9, dificultad media)
(define (mk-world-inicial)
  (mk-world-desde-tamano 9 9 'medio))

;Actualizar el tiempo de juego
(define (actualizar-tiempo-mundo w)
  (if (eq? (world-estado w) 'jugando)
      (struct-copy world w 
                   [tiempo-juego (- (current-seconds) (world-inicio-tiempo w))])
      w))

;Formatear tiempo en MM:SS
(define (formatear-tiempo segundos)
  (define minutos (quotient segundos 60))
  (define segs (remainder segundos 60))
  (format "~a:~a" 
          (if (< minutos 10) (string-append "0" (number->string minutos)) (number->string minutos))
          (if (< segs 10) (string-append "0" (number->string segs)) (number->string segs))))

;Generar tablero con minas evitando una posicion especifica
(define (crear-tablero-con-primer-click-seguro rows cols porcentaje-minas click-x click-y)
  ; Creamos tablero vacio
  (define tablero-vacio (crear-tablero-vacio rows cols 'personalizado))
  
  ; Calculamos numero de minas
  (define total-celdas (* rows cols))
  (define mines (max 1 (inexact->exact (floor (* total-celdas porcentaje-minas)))))
  
  ; Generamos posiciones para minas, excluyendo el area del primer click
  (define posiciones-prohibidas (obtener-area-segura click-x click-y cols rows))
  (define posiciones-disponibles 
    (filter (lambda (pos) (not (member pos posiciones-prohibidas)))
            (range total-celdas)))
  
  ; Seleccionamos posiciones aleatorias para las minas
  (define num-minas-final (min mines (length posiciones-disponibles)))
  (define posiciones-minas (tomar-aleatorio posiciones-disponibles num-minas-final))
  
  ; Colocamos las minas usando la funcion de la logica
  (define tablero-con-minas (colocar-minas-en-posiciones tablero-vacio posiciones-minas))
  
  ; Actualizamos conteos de vecinos
  (actualizar-conteo-vecinos tablero-con-minas))

;Obtener area segura alrededor del primer click (3x3)
(define (obtener-area-segura click-x click-y cols rows)
  (define area '())
  (for* ([dx (list -1 0 1)]
         [dy (list -1 0 1)])
    (define x (+ click-x dx))
    (define y (+ click-y dy))
    (when (and (>= x 0) (< x cols) (>= y 0) (< y rows))
      (set! area (cons (idx x y cols) area))))
  area)

;Tomar elementos aleatorios de una lista
(define (tomar-aleatorio lista n)
  (cond
    [(or (<= n 0) (null? lista)) '()]
    [(>= n (length lista)) lista]
    [else
     (define indice (random (length lista)))
     (define elemento (list-ref lista indice))
     (define resto (append (take lista indice) (drop lista (+ indice 1))))
     (cons elemento (tomar-aleatorio resto (- n 1)))]))

;----------------- UI: Boton visual de "dropdown" de dificultad ------------------
; Dibuja un rectangulo/titulo con un triangulo, solo representativo.
; El cambio real de dificultad se activa con un hit-test en el area del boton.
(define (dd-button-dificultad dificultad)
  (define label (case dificultad
                  [(facil) "FACIL (10%)"]
                  [(medio) "MEDIO (15%)"]
                  [(dificil) "DIFICIL (20%)"]))
  (overlay/align "left" "middle"
    (beside (text label 14 COLOR-top-txt)
            (rectangle 8 1 "solid" "transparent")
            (triangle 7 "solid" "white")) ; Triangulo como indicador de lista
    (rectangle 160 DD-H "solid" COLOR-top)))

;-------------------- DIBUJO DE CELDAS ------------------------
; Dibuja una celda: fondo (segun paridad A/B), y:
;  - si esta descubierta:
;      * mina -> circulo negro
;      * 0 vecinos -> celda gris clara
;      * >0 vecinos -> numero coloreado
;  - si esta marcada -> bandera roja (signo !)
;  - si esta oculta -> color A/B
(define (dibujar-celda celda x y alt? cell-size show-all-mines?)
  (define base-color (if alt? COLOR-board-a COLOR-board-b)) ; Patrón ajedrez
  (define font-size (max 10 (min 24 (inexact->exact (floor (* cell-size 0.65))))))
  (define mine-radius (max 4 (min 15 (inexact->exact (floor (* cell-size 0.4))))))
  (define flag-size  (max 10 (min 20 (inexact->exact (floor (* cell-size 0.6))))))

  ; ¿Debemos dibujar esta celda como "descubierta" aunque no lo esté?
  ; - Si ya está descubierta => sí.
  ; - Si el juego terminó en derrota y es mina => también sí (forzamos mostrar la mina).
  (define tratar-como-descubierta?
    (or (celda-descubierta? celda)
        (and show-all-mines? (celda-mina? celda))))

  (cond
    [tratar-como-descubierta?
     (cond
       [(celda-mina? celda)
        ; Mina visible (descubierta o revelada por derrota) sobre fondo abierto
        (overlay (circle mine-radius "solid" COLOR-mine)
                 (rectangle cell-size cell-size "solid" COLOR-cell-open))]

       [(= (celda-vecinos celda) 0)
        ; Sin minas vecinas: solo fondo abierto
        (rectangle cell-size cell-size "solid" COLOR-cell-open)]

       [else
        ; Con minas vecinas: número coloreado sobre fondo abierto
        (overlay (text (number->string (celda-vecinos celda))
                       font-size
                       (color-numero (celda-vecinos celda)))
                 (rectangle cell-size cell-size "solid" COLOR-cell-open))])]

    ; Si no se trata como descubierta: o bien está marcada, o sigue oculta
    [(celda-marcada? celda)
     (overlay (text "!" flag-size COLOR-flag)
              (rectangle cell-size cell-size "solid" base-color))]

    [else
     (rectangle cell-size cell-size "solid" base-color)]))

; Dibuja el tablero completo componiendo filas y celdas.
; Ahora pasa el flag `show-all-mines?` a cada celda para revelar todas las minas en derrota.
(define (dibujar-tablero-completo w)
  (define tablero (world-tablero w))
  (define filas (tablero-filas tablero))
  (define columnas (tablero-columnas tablero))
  (define celdas (tablero-celdas tablero))
  (define cell-size (world-cell-size w))

  ; Si el estado del mundo es 'derrota, revelamos todas las minas
  (define show-all-mines? (eq? (world-estado w) 'derrota))

  (define (dibujar-fila y)
    (define (dibujar-celda-en-x x)
      (define idx-actual (idx x y columnas)) ; (x,y) -> índice lineal
      (dibujar-celda (list-ref celdas idx-actual)
                     x y
                     (even? (+ x y))
                     cell-size
                     show-all-mines?))
    (apply beside (map dibujar-celda-en-x (range columnas))))
  (apply above (map dibujar-fila (range filas))))


;------------------- INTERFAZ SUPERIOR -------------------
;Ahora incluye el cronometro
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
  
  ;Informacion con cronometro
  (define tiempo-formateado (formatear-tiempo (world-tiempo-juego w)))
  (define info-text (format "~ax~a | ~a | Restantes: ~a | Tiempo: ~a" 
                            (world-cols w) (world-rows w) 
                            (case (world-dificultad w)
                              [(facil) "FACIL"]
                              [(medio) "MEDIO"] 
                              [(dificil) "DIFICIL"])
                            minas-restantes
                            tiempo-formateado))
  (define with-info
    (place-image (text info-text 13 COLOR-top-txt)
                 (/ SCN-W 2) (+ (/ TOP-H 2) 8) with-title))
  (place-image (text "R: Reiniciar" 10 COLOR-top-txt)
               (- SCN-W 80) (- (/ TOP-H 2) 8) with-info))

;------------------- CONVERSION DE COORDENADAS -------------------
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
;Ahora actualiza el tiempo antes de dibujar
(define (draw-world w)
  ; Actualizamos el tiempo si esta jugando
  (define w-con-tiempo (actualizar-tiempo-mundo w))
  
  (define SCN-W (world-window-width w-con-tiempo))
  (define SCN-H (world-window-height w-con-tiempo))
  (define cell-size (world-cell-size w-con-tiempo))
  (define cols (world-cols w-con-tiempo))
  (define rows (world-rows w-con-tiempo))
  (define BOARD-W (* cols cell-size))
  (define BOARD-H (* rows cell-size))
  (define board (dibujar-tablero-completo w-con-tiempo))
  (define scene (rectangle SCN-W SCN-H "solid" COLOR-bg))
  (define with-top (place-image (draw-top w-con-tiempo) (/ SCN-W 2) (/ TOP-H 2) scene))
  ; Centramos el tablero en la parte inferior de la barra superior
  (define board-x (/ SCN-W 2))
  (define board-y (+ TOP-H MARGIN (/ BOARD-H 2)))
  (define with-board (place-image board board-x board-y with-top))
  ; Mensajes de estado adaptados al tamano de celda
  (define font-size (min 18 (max 12 (inexact->exact (floor (* cell-size 0.5))))))
  (define mensaje
    (cond
      [(eq? (world-estado w-con-tiempo) 'victoria)
       (text "VICTORIA! Presiona R para reiniciar" font-size "green")]
      [(eq? (world-estado w-con-tiempo) 'derrota)
       (text "DERROTA! Presiona R para reiniciar" font-size "red")]
      [else
       (text (world-mensaje w-con-tiempo) (max 12 (- font-size 2)) "black")]))
  (define with-message
    (place-image mensaje (/ SCN-W 2) (- SCN-H 60) with-board))
  ; Ayudas/atajos en la parte inferior
  (define controles1
    (text "TAMANOS: 1(8x8) 2(9x9) 3(10x10) 4(11x11) 5(12x12) 6(13x13) 7(14x14) 8(15x15)" 11 "black"))
  (define controles2
    (text (string-append
           "R: Reiniciar | Click Izq: Bandera | F: Alternar IZQ (Bandera/Descubrir) | Menu: Dificultad")
          11 "black"))
  (define with-controls1
    (place-image controles1 (/ SCN-W 2) (- SCN-H 35) with-message))
  (define with-controls2
    (place-image controles2 (/ SCN-W 2) (- SCN-H 20) with-controls1))
  with-controls2)

;------------------- HIT-TESTS -------------------
; Utilidad geometrica: un punto (x,y) cae dentro del rectangulo (rx,ry,rw,rh)?
(define (pt-in-rect? x y rx ry rw rh)
  (and (<= rx x) (< x (+ rx rw)) 
       (<= ry y) (< y (+ ry rh))))

; Area clickeable del "boton" de dificultad (arriba a la izquierda)
(define (hit-dd-dificultad? x y)
  (pt-in-rect? x y MARGIN 12 160 DD-H))

; ----- Deteccion generica de boton derecho -----
; 2htdp/universe en muchos entornos NO reporta boton derecho;
; si 'me' contiene "right" asumimos que fue un click derecho real.
(define (right-click? me)
  (and (string? me) (string-contains? me "right")))

;------------------- MANEJO DE EVENTOS (MOUSE) -------------------
; Desvia a handle-mouse-activo a menos que el juego ya este terminado
(define (handle-mouse w x y me)
  (if (member (world-estado w) '(victoria derrota))
      w
      (handle-mouse-activo w x y me)))

;Logica de interaccion con el mouse que maneja el primer click
(define (handle-mouse-activo w x y me)
  (define p (screen->cell w x y)) ; Traducimos (x,y) a celda (cx,cy) o #f
  (cond
    ;; Menu de dificultad ciclico
    [(and (string=? me "button-down") (hit-dd-dificultad? x y))
     (define nueva-dificultad 
       (case (world-dificultad w)
         [(facil) 'medio] [(medio) 'dificil] [(dificil) 'facil]))
     (mk-world-desde-tamano (world-cols w) (world-rows w) nueva-dificultad)]

    ;; CLICK DERECHO real => descubrir (si el backend lo reporta)
    [(and p (right-click? me))
     (handle-left-click w (car p) (cdr p))]

    ;; CLICK IZQUIERDO => segun flag-left? (por defecto, bandera)
    [(and (string=? me "button-down") p)
     (if (world-flag-left? w)
         (handle-right-click w (car p) (cdr p)) ; bandera
         (handle-left-click  w (car p) (cdr p)))] ; descubrir

    [else w]))

; --- Acciones que llaman a la LOGICA de juego ---

;Descubrir celda con manejo del primer click seguro
(define (handle-left-click w cx cy)
  ; Si es el primer click, generamos el tablero con minas evitando esta posicion
  (define w-preparado 
    (if (world-primer-click? w)
        (let ([porcentaje (get-porcentaje-dificultad (world-dificultad w))]
              [tablero-nuevo (crear-tablero-con-primer-click-seguro 
                             (world-rows w) (world-cols w)
                             (get-porcentaje-dificultad (world-dificultad w))
                             cx cy)])
          (struct-copy world w 
                       [tablero tablero-nuevo]
                       [primer-click? #f]
                       [inicio-tiempo (current-seconds)])) ; Reiniciamos el cronometro
        w))
  
  (define tb-actual (world-tablero w-preparado))
  (define celda-actual (tablero-obtener-celda-xy tb-actual cx cy))
  (cond
    [(not celda-actual) w-preparado]                 ; Click fuera o indice invalido
    [(celda-marcada? celda-actual) w-preparado]      ; No se descubre si esta marcada
    [(celda-descubierta? celda-actual) w-preparado]  ; Ni si ya esta descubierta
    [else
     (define tb-nuevo (descubrir-celda tb-actual cx cy))       ; Logica: descubrir
     (define nuevo-mundo (struct-copy world w-preparado [tablero tb-nuevo]))
     (cond
       [(verificar-derrota? tb-nuevo) ; Si se descubrio una mina -> derrota
        (struct-copy world nuevo-mundo 
                     [estado 'derrota]
                     [mensaje "Has perdido! Presiona R para reiniciar"])]
       [(verificar-victoria? tb-nuevo) ; Si todas las no-minas estan abiertas -> victoria
        (struct-copy world nuevo-mundo 
                     [estado 'victoria]
                     [mensaje "Has ganado! Presiona R para reiniciar"])]
       [else
        ; Continuamos jugando: actualizamos mensaje con minas restantes
        (struct-copy world nuevo-mundo
                     [mensaje (format "~ax~a | ~a | Restantes: ~a" 
                                      (world-cols w-preparado) (world-rows w-preparado)
                                      (case (world-dificultad w-preparado)
                                        [(facil) "FACIL"]
                                        [(medio) "MEDIO"] 
                                        [(dificil) "DIFICIL"])
                                      (- (world-mines w-preparado)
                                         (tablero-contar-marcadas tb-nuevo)))])])]))

; Marcar/Desmarcar bandera (la logica impide marcar si ya esta descubierta)
(define (handle-right-click w cx cy)
  (define tb-actual (world-tablero w))
  (define celda-actual (tablero-obtener-celda-xy tb-actual cx cy))
  (cond
    [(not celda-actual) w]                ; Click fuera o indice invalido
    [(celda-descubierta? celda-actual) w] ; No marcar si ya esta descubierta
    [else
     (define tb-nuevo (marcar-celda tb-actual cx cy)) ; Logica: alternar marcado
     (struct-copy world w 
                  [tablero tb-nuevo]
                  [mensaje (format "~ax~a | ~a | Restantes: ~a" 
                                   (world-cols w) (world-rows w)
                                   (case (world-dificultad w)
                                     [(facil) "FACIL"]
                                     [(medio) "MEDIO"] 
                                     [(dificil) "DIFICIL"])
                                   (- (world-mines w)
                                      (tablero-contar-marcadas tb-nuevo)))])]))

;------------------- TECLADO -------------------
;Atajos que ahora tambien reinician el cronometro
(define (handle-key w key)
  (cond
    [(key=? key "r")
     (mk-world-desde-tamano (world-cols w) (world-rows w) (world-dificultad w))]

    ;; Alternar mapeo del clic izquierdo (fallback universal)
    [(key=? key "f")
     (struct-copy world w
                  [flag-left? (not (world-flag-left? w))]
                  [mensaje (if (world-flag-left? w)
                               "IZQ ahora DESCUBRE (F para volver a Bandera)"
                               "IZQ ahora BANDERA (F para descubrir)")])]

    [else
     (define-values (cols rows) (get-tamano-desde-tecla key))
     (if (and cols rows)
         (mk-world-desde-tamano cols rows (world-dificultad w))
         w)]))

;------------------- MAIN -------------------
; Lanza el mundo con big-bang. Se fija el tamano de ventana en base al mundo inicial.
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
  (printf "TAMANOS: 1->8x8 2->9x9 3->10x10 4->11x11 5->12x12 6->13x13 7->14x14 8->15x15~n~n")
  (printf "DIFICULTADES: FACIL 10%% | MEDIO 15%% | DIFICIL 20%%~n~n")
  (printf "CLICK IZQ: Bandera (por defecto) — presiona F para alternar a Descubrir~n")
  (printf "CLICK DER (si tu entorno lo reporta): Descubrir~n")
  (printf "R: Reiniciar | F: Alternar funcion del click ~n")
  (printf "CRONOMETRO: Se inicia con el primer click y se pausa al ganar/perder~n")
  (printf "PRIMER CLICK SEGURO: Siempre descubrira celdas y nunca sera una mina~n"))

; Mostrar ayuda e iniciar el juego
(mostrar-controles)
(main)