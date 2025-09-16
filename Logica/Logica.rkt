#lang racket
;=========================================================
;Referencias
;https://chatgpt.com/
;https://www.youtube.com/watch?v=NwZPlLTK-UQ&list=PLTd5ehIj0goMswKV4Glhr4uixrhCmihIt&index=2&ab_channel=makigas
;=========================================================
; Buscaminas con un grafo
; - Matriz NxM (H filas x W columnas), lista de adyacencia y tabla de 8 vecinos
; - Soporte para tableros personalizados 8x8 a 15x15
; =========================================================

;---------------------ESTRUCTURAS---------------------
(struct tablero (filas columnas nivel celdas) #:transparent)
(struct celda (mina? vecinos descubierta? marcada?) #:transparent)
(struct grafo (W H N nodos matriz adjacencia vecinos8) #:transparent)

;---------------------INDEXACIÓN Y COORDENADAS---------------------
(define (idx x y W) (+ x (* y W)))
(define (x-of i W) (remainder i W))
(define (y-of i W) (quotient i W))
(define (dentro? x y W H)
  (and (<= 0 x) (< x W) (<= 0 y) (< y H)))

;---------------------VALIDACIÓN DE DIMENSIONES---------------------
(define MIN-SIZE 8)
(define MAX-SIZE 15)

(define (validar-dimensiones filas columnas)
  (and (>= filas MIN-SIZE) (<= filas MAX-SIZE)
       (>= columnas MIN-SIZE) (<= columnas MAX-SIZE)))

;---------------------CREACIÓN DE TABLERO PRINCIPAL---------------------
; Crear tablero con dimensiones personalizadas
(define (crear-tablero-personalizado filas columnas porcentaje-minas)
  (unless (validar-dimensiones filas columnas)
    (error 'crear-tablero-personalizado 
           "Las dimensiones deben estar entre ~a y ~a" MIN-SIZE MAX-SIZE))
  (unless (and (>= porcentaje-minas 0.05) (<= porcentaje-minas 0.30))
    (error 'crear-tablero-personalizado 
           "El porcentaje de minas debe estar entre 5% y 30%"))
  
  (define total-celdas (* filas columnas))
  (define num-minas (max 1 (min (- total-celdas 1) 
                               (inexact->exact (floor (* total-celdas porcentaje-minas))))))
  (define tablero-vacio (crear-tablero-vacio filas columnas 'personalizado))
  (define tablero-con-minas (colocar-minas-aleatorias tablero-vacio num-minas))
  (actualizar-conteo-vecinos tablero-con-minas))

; Crear tablero completo con minas
(define (crear-tablero filas columnas nivel)
  (define num-minas (calcular-numero-minas filas columnas nivel))
  (define tablero-vacio (crear-tablero-vacio filas columnas nivel))
  (define tablero-con-minas (colocar-minas-aleatorias tablero-vacio num-minas))
  (actualizar-conteo-vecinos tablero-con-minas))

; Crear tablero vacío (sin minas)
(define (crear-tablero-vacio filas columnas nivel)
  (unless (validar-dimensiones filas columnas)
    (error 'crear-tablero-vacio 
           "Las dimensiones deben estar entre ~a y ~a" MIN-SIZE MAX-SIZE))
  (tablero filas columnas nivel 
           (crear-lista-celdas-vacias (* filas columnas))))

; Crear lista de celdas vacías recursivamente
(define (crear-lista-celdas-vacias n)
  (if (<= n 0)
      '()
      (cons (celda #f 0 #f #f)
            (crear-lista-celdas-vacias (- n 1)))))

; Calcular número de minas según nivel (mejorado)
(define (calcular-numero-minas filas columnas nivel)
  (define total-celdas (* filas columnas))
  (define porcentaje 
    (case nivel
      [(facil) 0.10]
      [(medio) 0.15]
      [(dificil) 0.20]
      [(personalizado) 0.15]  ; Por defecto para tableros personalizados
      [else 0.10]))
  (max 1 (min (- total-celdas 1)  ; Asegurar que quede al menos una celda sin mina
              (inexact->exact (floor (* total-celdas porcentaje))))))

;---------------------MANEJO DE VECINOS---------------------
(define (vecino i dx dy W H)
  (if (dentro? (+ (x-of i W) dx) (+ (y-of i W) dy) W H)
      (idx (+ (x-of i W) dx) (+ (y-of i W) dy) W)
      #f))

(define (cons-si x xs) 
  (if x (cons x xs) xs))

(define (vecinos-de i W H)
  (cons-si (vecino i -1 -1 W H)
    (cons-si (vecino i  0 -1 W H)
      (cons-si (vecino i  1 -1 W H)
        (cons-si (vecino i -1  0 W H)
          (cons-si (vecino i  1  0 W H)
            (cons-si (vecino i -1  1 W H)
              (cons-si (vecino i  0  1 W H)
                (cons-si (vecino i  1  1 W H) '())))))))))

(define (vecinos8 i W H)
  (list (vecino i -1 -1 W H)
        (vecino i  0 -1 W H)
        (vecino i  1 -1 W H)
        (vecino i -1  0 W H)
        (vecino i  1  0 W H)
        (vecino i -1  1 W H)
        (vecino i  0  1 W H)
        (vecino i  1  1 W H)))

;---------------------- COLOCACIÓN DE MINAS ---------------------
(define (generar-posiciones-unicas num-minas total-celdas)
  (generar-posiciones-unicas-aux num-minas total-celdas '()))

(define (generar-posiciones-unicas-aux num-necesarias total-celdas posiciones-actuales)
  (cond
    [(= (length posiciones-actuales) num-necesarias) posiciones-actuales]
    [else
     (define nueva-posicion (random total-celdas))
     (if (member nueva-posicion posiciones-actuales)
         (generar-posiciones-unicas-aux num-necesarias total-celdas posiciones-actuales)
         (generar-posiciones-unicas-aux num-necesarias total-celdas 
                                       (cons nueva-posicion posiciones-actuales)))]))

(define (colocar-minas tablero lista-indices)
  (cond
    [(null? lista-indices) tablero]
    [else
     (define indice-actual (car lista-indices))
     (define mina-nueva (celda #t 0 #f #f))
     (define tablero-actualizado 
       (tablero-reemplazar-celda-nodo tablero indice-actual mina-nueva))
     (colocar-minas tablero-actualizado (cdr lista-indices))]))

(define (colocar-minas-aleatorias tablero num-minas)
  (define total-celdas (* (tablero-filas tablero) (tablero-columnas tablero)))
  (cond
    [(> num-minas total-celdas)
     (error 'colocar-minas-aleatorias 
            "Se pidieron ~a minas pero solo hay ~a celdas"
            num-minas total-celdas)]
    [else
     (define posiciones-aleatorias (generar-posiciones-unicas num-minas total-celdas))
     (colocar-minas tablero posiciones-aleatorias)]))

;--------------------- CONTEO DE MINAS VECINAS--------------------------
(define (contar-minas-vecinas tb x y)
  (if (not (dentro? x y (tablero-columnas tb) (tablero-filas tb)))
      0
      (contar-minas-en-posiciones 
        tb 
        (vecinos-de (idx x y (tablero-columnas tb)) 
                    (tablero-columnas tb) 
                    (tablero-filas tb)))))

(define (contar-minas-en-posiciones tb lista-indices)
  (cond
    [(null? lista-indices) 0]
    [else
     (define celda-actual (tablero-obtener-celda-nodo tb (car lista-indices)))
     (if (and celda-actual (celda-mina? celda-actual))
         (+ 1 (contar-minas-en-posiciones tb (cdr lista-indices)))
         (contar-minas-en-posiciones tb (cdr lista-indices)))]))

(define (actualizar-conteo-vecinos tablero)
  (define total (* (tablero-filas tablero) (tablero-columnas tablero)))
  (actualizar-conteo-aux tablero 0 total))

(define (actualizar-conteo-aux tablero indice total)
  (cond
    [(>= indice total) tablero]
    [else
     (define celda-actual (list-ref (tablero-celdas tablero) indice))
     (if (celda-mina? celda-actual)
         (actualizar-conteo-aux tablero (+ indice 1) total)
         (actualizar-conteo-aux 
           (tablero-reemplazar-celda-nodo 
             tablero 
             indice 
             (celda-establecer-vecinos 
               celda-actual 
               (contar-minas-vecinas 
                 tablero 
                 (x-of indice (tablero-columnas tablero))
                 (y-of indice (tablero-columnas tablero)))))
           (+ indice 1) 
           total))]))

;--------------------- MODIFICACIÓN DE CELDAS ---------------------
(define (celda-marcar c nuevo-estado)
  (celda (celda-mina? c) (celda-vecinos c) (celda-descubierta? c) nuevo-estado))

(define (celda-descubrir c)
  (celda (celda-mina? c) (celda-vecinos c) #t (celda-marcada? c)))

(define (celda-establecer-vecinos c nuevo-numero)
  (celda (celda-mina? c) nuevo-numero (celda-descubierta? c) (celda-marcada? c)))

(define (celda-alternar-marcado c)
  (celda-marcar c (not (celda-marcada? c))))

; --------------------- ACCESO Y MODIFICACIÓN DE TABLERO ---------------------
(define (tablero-obtener-celda-xy tb x y)
  (if (dentro? x y (tablero-columnas tb) (tablero-filas tb))
      (tablero-obtener-celda-nodo tb (idx x y (tablero-columnas tb)))
      #f))

(define (tablero-obtener-celda-nodo tb nodo)
  (if (and (>= nodo 0) (< nodo (* (tablero-filas tb) (tablero-columnas tb))))
      (list-ref (tablero-celdas tb) nodo)
      #f))

(define (tablero-reemplazar-celda-xy tb x y nueva-celda)
  (if (dentro? x y (tablero-columnas tb) (tablero-filas tb))
      (tablero-reemplazar-celda-nodo tb (idx x y (tablero-columnas tb)) nueva-celda)
      tb))

(define (tablero-reemplazar-celda-nodo tb nodo nueva-celda)
  (if (and (>= nodo 0) (< nodo (* (tablero-filas tb) (tablero-columnas tb))))
      (tablero (tablero-filas tb)
               (tablero-columnas tb)
               (tablero-nivel tb)
               (reemplazar-elemento-lista (tablero-celdas tb) nodo nueva-celda))
      tb))

(define (reemplazar-elemento-lista lista indice nuevo-elemento)
  (cond
    [(null? lista) '()]
    [(< indice 0) lista]
    [(= indice 0) (cons nuevo-elemento (cdr lista))]
    [else (cons (car lista) 
                (reemplazar-elemento-lista (cdr lista) (- indice 1) nuevo-elemento))]))

; --------------------- LÓGICA DE JUEGO ---------------------
; Descubrir celda y manejar cascada si es 0
(define (descubrir-celda tb x y)
  (define celda (tablero-obtener-celda-xy tb x y))
  (cond
    [(not celda) tb]  ; Coordenadas inválidas
    [(celda-descubierta? celda) tb]  ; Ya descubierta
    [(celda-marcada? celda) tb]  ; Marcada con bandera
    [(celda-mina? celda)  ; Es mina - Game Over
     (tablero-reemplazar-celda-xy tb x y (celda-descubrir celda))]
    [else  ; No es mina
     (define celda-descubierta (celda-descubrir celda))
     (define tb-actualizado (tablero-reemplazar-celda-xy tb x y celda-descubierta))
     (if (= (celda-vecinos celda) 0)
         (descubrir-vecinos-recursivo tb-actualizado x y)
         tb-actualizado)]))

; Descubrir recursivamente vecinos cuando la celda tiene 0 minas alrededor
(define (descubrir-vecinos-recursivo tb x y)
  (define vecinos (vecinos-de (idx x y (tablero-columnas tb)) 
                              (tablero-columnas tb) 
                              (tablero-filas tb)))
  (descubrir-lista-vecinos tb vecinos))

(define (descubrir-lista-vecinos tb lista-indices)
  (cond
    [(null? lista-indices) tb]
    [else
     (define indice (car lista-indices))
     (define x (x-of indice (tablero-columnas tb)))
     (define y (y-of indice (tablero-columnas tb)))
     (define tb-actualizado (descubrir-celda tb x y))
     (descubrir-lista-vecinos tb-actualizado (cdr lista-indices))]))

; Marcar/desmarcar celda con bandera
(define (marcar-celda tb x y)
  (define celda (tablero-obtener-celda-xy tb x y))
  (cond
    [(not celda) tb]
    [(celda-descubierta? celda) tb]  ; No se puede marcar si ya está descubierta
    [else (tablero-reemplazar-celda-xy tb x y (celda-alternar-marcado celda))]))

;Obtener el estado completo de una celda para la interfaz
(define (obtener-estado-celda tb x y)
  (define celda (tablero-obtener-celda-xy tb x y))
  (cond
    ;Coordenadas inválidas o celda no existe
    [(not celda) 'invalida]
    
    ;Celda descubierta - verificar qué contiene
    [(celda-descubierta? celda)
     (cond
       ;Es una mina descubierta (causa derrota)
       [(celda-mina? celda) 'mina-descubierta]
       ;Celda sin minas alrededor (se expande automáticamente)
       [(= (celda-vecinos celda) 0) 'vacia]
       ;Celda con minas vecinas (retorna el número 1-8)
       [else (celda-vecinos celda)])]
    
    ;Celda marcada con bandera (no descubierta)
    [(celda-marcada? celda) 'marcada]
    
    ;Celda oculta sin marcar
    [else 'oculta]))

;Calcular el número de minas restantes (no marcadas)
(define (obtener-minas-restantes tb)
  (- (tablero-contar-minas tb) 
     (tablero-contar-marcadas tb)))

; Contar banderas en una lista específica de índices
(define (contar-banderas-en-lista tb lista-indices)
  (cond
    ;Caso base: lista vacía
    [(null? lista-indices) 0]
    ;Caso recursivo
    [else
     (define indice-actual (car lista-indices))
     (define celda-actual (tablero-obtener-celda-nodo tb indice-actual))
     (cond
       ;Si no existe la celda, continuar sin contar
       [(not celda-actual) 
        (contar-banderas-en-lista tb (cdr lista-indices))]
       ;Si está marcada, sumar 1 y continuar
       [(celda-marcada? celda-actual)
        (+ 1 (contar-banderas-en-lista tb (cdr lista-indices)))]
       ;Si no está marcada, continuar sin sumar
       [else
        (contar-banderas-en-lista tb (cdr lista-indices))])]))

; Verificar si todas las celdas están descubiertas
(define (verificar-todas-descubiertas? tb)
  (verificar-todas-descubiertas-aux (tablero-celdas tb)))

(define (verificar-todas-descubiertas-aux celdas)
  (cond
    ;Si revisamos todas y ninguna falló, retornar #t
    [(null? celdas) #t]
    ;Si encontramos una no descubierta, retornar #f
    [(not (celda-descubierta? (car celdas))) #f]
    ;Continuar con el resto
    [else (verificar-todas-descubiertas-aux (cdr celdas))]))

;Obtener estadísticas completas del tablero
    ;Retorna: lista de pares (símbolo . valor) con estadísticas
(define (obtener-estadisticas-tablero tb)
  (define total-celdas (* (tablero-filas tb) (tablero-columnas tb)))
  (define minas (tablero-contar-minas tb))
  (define descubiertas (tablero-contar-descubiertas tb))
  (define marcadas (tablero-contar-marcadas tb))
  (define sin-descubrir (- total-celdas descubiertas))
  (define porcentaje-completado 
    (exact->inexact (/ descubiertas (- total-celdas minas))))
  
  (list (cons 'total-celdas total-celdas)
        (cons 'total-minas minas)
        (cons 'celdas-descubiertas descubiertas)
        (cons 'celdas-marcadas marcadas)
        (cons 'celdas-sin-descubrir sin-descubrir)
        (cons 'minas-restantes (- minas marcadas))
        (cons 'porcentaje-completado porcentaje-completado)
        (cons 'estado-juego 
              (cond
                [(verificar-victoria? tb) 'victoria]
                [(verificar-derrota? tb) 'derrota]
                [else 'en-progreso]))))

; Verificar victoria
(define (verificar-victoria? tb)
  (define celdas (tablero-celdas tb))
  (verificar-victoria-aux celdas))

(define (verificar-victoria-aux celdas)
  (cond
    [(null? celdas) #t]  ; Todas verificadas
    [else
     (define c (car celdas))
     (if (and (not (celda-mina? c)) (not (celda-descubierta? c)))
         #f  ; Hay una celda sin mina que no está descubierta
         (verificar-victoria-aux (cdr celdas)))]))

; Verificar derrota (alguna mina descubierta)
(define (verificar-derrota? tb)
  (define celdas (tablero-celdas tb))
  (verificar-derrota-aux celdas))

(define (verificar-derrota-aux celdas)
  (cond
    [(null? celdas) #f]
    [else
     (define c (car celdas))
     (if (and (celda-mina? c) (celda-descubierta? c))
         #t
         (verificar-derrota-aux (cdr celdas)))]))

; --------------------- ESTRUCTURAS DE GRAFO ---------------------
(define (rango a b)
  (if (>= a b) '()
      (cons a (rango (+ a 1) b))))

(define (fila W y x)
  (if (= x W) '()
      (cons (idx x y W) (fila W y (+ x 1)))))

(define (matriz-nodos W H y)
  (if (= y H) '()
      (cons (fila W y 0) (matriz-nodos W H (+ y 1)))))

(define (alist-ady W H i N)
  (if (= i N) '()
      (cons (vecinos-de i W H) (alist-ady W H (+ i 1) N))))

(define (tabla-vecinos W H i N)
  (if (= i N) '()
      (cons (vecinos8 i W H) (tabla-vecinos W H (+ i 1) N))))

(define (grafo-grid W H)
  (grafo W H (* W H)
         (rango 0 (* W H))
         (matriz-nodos W H 0)
         (alist-ady W H 0 (* W H))
         (tabla-vecinos W H 0 (* W H))))

; --------------------- UTILIDADES ---------------------
(define (tablero-contar-minas tb)
  (contar-elementos-con (tablero-celdas tb) celda-mina?))

(define (tablero-contar-descubiertas tb)
  (contar-elementos-con (tablero-celdas tb) celda-descubierta?))

(define (tablero-contar-marcadas tb)
  (contar-elementos-con (tablero-celdas tb) celda-marcada?))

(define (contar-elementos-con lista condicion)
  (cond
    [(null? lista) 0]
    [(condicion (car lista)) (+ 1 (contar-elementos-con (cdr lista) condicion))]
    [else (contar-elementos-con (cdr lista) condicion)]))

; --------------------- FUNCIONES ADICIONALES PARA TABLEROS PERSONALIZADOS ---------------------
; Obtener información sobre el tablero
(define (tablero-info tb)
  (define total-celdas (* (tablero-filas tb) (tablero-columnas tb)))
  (define total-minas (tablero-contar-minas tb))
  (define porcentaje-minas (/ total-minas total-celdas))
  (list (cons 'dimensiones (format "~ax~a" (tablero-columnas tb) (tablero-filas tb)))
        (cons 'total-celdas total-celdas)
        (cons 'total-minas total-minas)
        (cons 'porcentaje-minas (exact->inexact porcentaje-minas))
        (cons 'nivel (tablero-nivel tb))))

; Sugerir número de minas para dimensiones dadas
(define (sugerir-minas filas columnas dificultad)
  (unless (and (>= filas 8) (<= filas 30) (>= columnas 8) (<= columnas 30))
    (error 'sugerir-minas "Dimensiones inválidas"))
  (define total-celdas (* filas columnas))
  (define porcentaje
    (case dificultad
      [(facil) 0.10]
      [(medio) 0.15]
      [(dificil) 0.20]
      [else 0.15]))
  (values (max 1 (inexact->exact (floor (* total-celdas porcentaje))))
          (max 1 (inexact->exact (floor (* total-celdas 0.05))))   ; mínimo
          (min (- total-celdas 1) (inexact->exact (floor (* total-celdas 0.30)))))) ; máximo

; --------------------- EXPORTS ---------------------
(provide
 ; Estructuras
 (struct-out celda)
 (struct-out tablero)
 (struct-out grafo)
 
 ; Constantes
 MIN-SIZE MAX-SIZE
 
 ; Funciones principales
 crear-tablero
 crear-tablero-personalizado
 crear-tablero-vacio
 calcular-numero-minas
 
 ; Validación
 validar-dimensiones 
 
 ; Indexación
 idx x-of y-of dentro?
 
 ; Vecinos
 vecinos-de vecinos8
 
 ; Modificación de celdas
 celda-marcar celda-descubrir celda-establecer-vecinos celda-alternar-marcado
 
 ; Acceso y modificación de tablero
 tablero-obtener-celda-xy tablero-obtener-celda-nodo
 tablero-reemplazar-celda-xy tablero-reemplazar-celda-nodo
 
 ; Lógica del juego
 descubrir-celda marcar-celda
 verificar-victoria? verificar-derrota?
 
 ; Minas
 colocar-minas-aleatorias contar-minas-vecinas
 
 ; Utilidades
 tablero-contar-minas tablero-contar-descubiertas tablero-contar-marcadas
 tablero-info sugerir-minas

 ;Funciones de estado
 obtener-estado-celda
 obtener-minas-restantes
 contar-banderas-en-lista
 verificar-todas-descubiertas?
 obtener-estadisticas-tablero
 
 ; Grafo
 grafo-grid rango matriz-nodos alist-ady tabla-vecinos)