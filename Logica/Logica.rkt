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
; Estructura que representa una celda individual del tablero
; mina?: Boolean - indica si contiene una mina
; vecinos: Integer - numero de minas en las 8 celdas adyacentes (0-8)
; descubierta?: Boolean - si el usuario ya la descubrio (hizo click)
; marcada?: Boolean - si tiene bandera colocada por el usuario
(struct tablero (filas columnas nivel celdas) #:transparent)

; Estructura que representa el tablero completo del juego
; filas/columnas: Integer - dimensiones del tablero (8-15)
; nivel: Symbol - dificultad (facil, medio, dificil, personalizado)
; celdas: List - lista plana de todas las celdas (filas*columnas elementos)
(struct celda (mina? vecinos descubierta? marcada?) #:transparent)

; Estructura auxiliar para representar el tablero como grafo
; W,H,N: Integer - ancho, alto, numero total de nodos
; nodos, matriz, adjacencia, vecinos8: Lists - representaciones del grafo
(struct grafo (W H N nodos matriz adjacencia vecinos8) #:transparent)

;---------------------INDEXACION Y COORDENADAS---------------------
; Convierte coordenadas (x,y) a indice lineal en la lista de celdas
; Formula: indice = x + y * ancho_tablero
; Ejemplo: En tablero 8x8, posicion (3,2) -> indice 3 + 2*8 = 19
(define (idx x y W) (+ x (* y W)))

; Convierte coordenadas (x,y) a índice lineal en la lista de celdas
; Formula: indice = x + y * ancho_tablero
; Ejemplo: En tablero 8x8, posicion (3,2) -> indice 3 + 2*8 = 19
(define (x-of i W) (remainder i W))

; Convierte indice lineal a coordenada Y
; Formula: y = indice / ancho_tablero (division entera)
; Ejemplo: En tablero 8x8, índice 19 -> y = 19 / 8 = 2
(define (y-of i W) (quotient i W))

; Valida si las coordenadas están dentro de los límites del tablero
; Parametros: x,y (coordenadas), W,H (dimensiones del tablero)
; Retorna: #t si esta dentro, #f si esta fuera
; Validacion: 0 <= x < W y 0 <= y < H
(define (dentro? x y W H)
  (and (<= 0 x) (< x W) (<= 0 y) (< y H)))

;---------------------VALIDACION DE DIMENSIONES---------------------
; Limites de tamaño segun especificaciones del proyecto
(define MIN-SIZE 8)   ; Tamaño minimo permitido
(define MAX-SIZE 15)  ; Tamaño maximo permitido

;Valida que las dimensiones estén en el rango permitido (8x8 a 15x15)
(define (validar-dimensiones filas columnas)
  (and (>= filas MIN-SIZE) (<= filas MAX-SIZE)
       (>= columnas MIN-SIZE) (<= columnas MAX-SIZE)))

;---------------------CREACION DE TABLERO PRINCIPAL---------------------
; Crear tablero con dimensiones personalizadas
(define (crear-tablero-personalizado filas columnas porcentaje-minas)
  ;Dimensiones dentro del rango permitido
  (unless (validar-dimensiones filas columnas)
    (error 'crear-tablero-personalizado 
           "Las dimensiones deben estar entre ~a y ~a" MIN-SIZE MAX-SIZE))
  ;Porcentaje de minas razonable (5% a 30%)
  (unless (and (>= porcentaje-minas 0.05) (<= porcentaje-minas 0.30))
    (error 'crear-tablero-personalizado 
           "El porcentaje de minas debe estar entre 5% y 30%"))

  ;Numero total de celdas en el tablero
  (define total-celdas (* filas columnas))

  ;Numero de minas (minimo 1, maximo total-1 para que sea jugable)
  (define num-minas (max 1 (min (- total-celdas 1) 
                               (inexact->exact (floor (* total-celdas porcentaje-minas))))))

  (define tablero-vacio (crear-tablero-vacio filas columnas 'personalizado))
  (define tablero-con-minas (colocar-minas-aleatorias tablero-vacio num-minas))
  (actualizar-conteo-vecinos tablero-con-minas))

; Crear tablero con niveles predefinidos (facil 10%, medio 15%, dificil 20%)
;  Simplifica la creacion con dificultades estándar
; Algoritmo: Similar a crear-tablero-personalizado pero con porcentajes fijos
(define (crear-tablero filas columnas nivel)
  (define num-minas (calcular-numero-minas filas columnas nivel))
  (define tablero-vacio (crear-tablero-vacio filas columnas nivel))
  (define tablero-con-minas (colocar-minas-aleatorias tablero-vacio num-minas))
  (actualizar-conteo-vecinos tablero-con-minas))

; Crear tablero vacio (todas las celdas sin minas)
; Proposito: Base para luego agregar minas
; Estructura: Lista plana de (filas * columnas) celdas iniciales
(define (crear-tablero-vacio filas columnas nivel)
  (unless (validar-dimensiones filas columnas)
    (error 'crear-tablero-vacio 
           "Las dimensiones deben estar entre ~a y ~a" MIN-SIZE MAX-SIZE))
  ; Crear estructura tablero con lista de celdas vacías
  (tablero filas columnas nivel 
           (crear-lista-celdas-vacias (* filas columnas))))

; Recursion: Crear lista de n celdas iniciales (sin minas, sin descubrir, sin marcar)
; Patron: Recursion de cola para eficiencia
; Caso base: n <= 0 retorna lista vacía
; Caso recursivo: cons nueva celda con resto de la lista
(define (crear-lista-celdas-vacias n)
  (if (<= n 0)
      '()
      (cons (celda #f 0 #f #f)
            (crear-lista-celdas-vacias (- n 1)))))

; Calcular número de minas según el nivel de dificultad
; Porcentajes: Facil 10%, Medio 15%, Dificil 20%
; Restriccion: Minimo 1 mina, maximo (total-1) para que sea resoluble
(define (calcular-numero-minas filas columnas nivel)
  (define total-celdas (* filas columnas))
 ; Nivel de dificultad a porcentaje de minas
  (define porcentaje 
    (case nivel
      [(facil) 0.10]        ; 10% de las celdas tendran minas
      [(medio) 0.15]        ; 15% de las celdas tendran minas
      [(dificil) 0.20]      ; 20% de las celdas tendran minas
      [(personalizado) 0.15]  ; Por defecto para tableros personalizados
      [else 0.10]))         ; Fallback por seguridad
  ; Calculo: Aplicar porcentaje y redondear hacia abajo
  (max 1 (min (- total-celdas 1)  ; Asegurar que quede al menos una celda sin mina
              (inexact->exact (floor (* total-celdas porcentaje))))))

;---------------------MANEJO DE VECINOS---------------------

; Calcular las 8 celdas adyacentes a cualquier posición
; Cada celda tiene hasta 8 vecinos (menos en bordes y esquinas)

; Calcular UN vecino especifico usando desplazamiento relativo
; Parametros: i (indice actual), dx,dy (desplazamiento), W,H (dimensiones)
; Retorna: Indice del vecino si esta dentro del tablero, #f si esta fuera
(define (vecino i dx dy W H)
; indice -> coordenadas -> aplicar desplazamiento -> validar -> indice
  (if (dentro? (+ (x-of i W) dx) (+ (y-of i W) dy) W H)
      (idx (+ (x-of i W) dx) (+ (y-of i W) dy) W)
      #f))

; Agregar elemento a lista solo si no es #f
; Construir listas de vecinos validos excluyendo posiciones fuera del tablero
(define (cons-si x xs) 
  (if x (cons x xs) xs))

; Calcular TODOS los vecinos validos de una posicion (lista sin #f)
; Revisar las 8 direcciones posibles y agregar solo las validas
; Direccion: (-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)
; Retorna: Lista de indices de vecinos validos (puede tener 3-8 elementos)
(define (vecinos-de i W H)
  (cons-si (vecino i -1 -1 W H)  ; Noroeste
    (cons-si (vecino i  0 -1 W H)  ; Norte
      (cons-si (vecino i  1 -1 W H)  ; Noreste
        (cons-si (vecino i -1  0 W H)  ; Oeste
          (cons-si (vecino i  1  0 W H)  ; Este
            (cons-si (vecino i -1  1 W H)  ; Suroeste
              (cons-si (vecino i  0  1 W H)  ; Sur
                (cons-si (vecino i  1  1 W H) '())))))))))  ; Sureste

; Calcular vecinos incluyendo #f para posiciones inválidas (siempre 8 elementos)
; Proposito: Mantener estructura fija para procesamiento en otras funciones
; Diferencia con vecinos-de: Esta incluye #f, vecinos-de los filtra
(define (vecinos8 i W H)
  (list (vecino i -1 -1 W H)  ; Siempre 8 elementos
        (vecino i  0 -1 W H)  ; Algunos pueden ser #f
        (vecino i  1 -1 W H)
        (vecino i -1  0 W H)
        (vecino i  1  0 W H)
        (vecino i -1  1 W H)
        (vecino i  0  1 W H)
        (vecino i  1  1 W H)))

;---------------------- COLOCACION DE MINAS ---------------------
; Se tiene como proposito generar posiciones aleatorias únicas para colocar las minas

; Generar lista de posiciones únicas
; Llama funcion auxiliar con acumulador vacio
(define (generar-posiciones-unicas num-minas total-celdas)
  (generar-posiciones-unicas-aux num-minas total-celdas '()))

; Generar posiciones evitando duplicados
(define (generar-posiciones-unicas-aux num-necesarias total-celdas posiciones-actuales)
  (cond
    ; Caso Base: Ya tenemos todas las posiciones necesarias
    [(= (length posiciones-actuales) num-necesarias) posiciones-actuales]
    [else
    ; Generar: Nueva posición aleatoria
     (define nueva-posicion (random total-celdas))
    ; Verificar: Si ya existe, reintentar; si no, agregar
     (if (member nueva-posicion posiciones-actuales)
          ; Reintentar con las mismas posiciones
         (generar-posiciones-unicas-aux num-necesarias total-celdas posiciones-actuales)
          ; Agregar nueva posición y continuar
         (generar-posiciones-unicas-aux num-necesarias total-celdas 
                                       (cons nueva-posicion posiciones-actuales)))]))

; Rercursion: Colocar minas en posiciones específicas de la lista
; Procesa lista de índices uno por uno
; Cada colocación crea nuevo tablero
(define (colocar-minas tablero lista-indices)
  (cond
    ; Caso Base: No mas minas que colocar
    [(null? lista-indices) tablero]
    [else
      ; Tomar primera posición de la lista
     (define indice-actual (car lista-indices))
      ; Crear: Nueva celda con mina (conservando otros valores iniciales)
     (define mina-nueva (celda #t 0 #f #f))
     (define tablero-actualizado 
        ; Actualizar: Crear nuevo tablero con la mina colocada
       (tablero-reemplazar-celda-nodo tablero indice-actual mina-nueva))
      ; Procesar resto de las posiciones
     (colocar-minas tablero-actualizado (cdr lista-indices))]))

; Colocar minas aleatorias en tablero vacío
; Verifica que no se pidan más minas que celdas disponibles
; Genera posiciones -> colocar minas
(define (colocar-minas-aleatorias tablero num-minas)
  (define total-celdas (* (tablero-filas tablero) (tablero-columnas tablero)))
  (cond
    ; Validacion: Imposible colocar más minas que celdas
    [(> num-minas total-celdas)
     (error 'colocar-minas-aleatorias 
            "Se pidieron ~a minas pero solo hay ~a celdas"
            num-minas total-celdas)]
    [else
      ; Generar posiciones aleatorias y colocar minas
     (define posiciones-aleatorias (generar-posiciones-unicas num-minas total-celdas))
     (colocar-minas tablero posiciones-aleatorias)]))

;Colocar minas en posiciones especificas (para primer click seguro)
(define (colocar-minas-en-posiciones tablero lista-indices)
; Similar a colocar-minas pero con conversión de coordenadas
  (cond
    [(null? lista-indices) tablero]
    [else
     (define indice-actual (car lista-indices))
    ; Indice a coordenadas para funcion de reemplazo
     (define x (x-of indice-actual (tablero-columnas tablero)))
     (define y (y-of indice-actual (tablero-columnas tablero)))
     (define mina-nueva (celda #t 0 #f #f))
      ; Funcion de coordenadas en lugar de indices
     (define tablero-actualizado 
       (tablero-reemplazar-celda-xy tablero x y mina-nueva))
     (colocar-minas-en-posiciones tablero-actualizado (cdr lista-indices))]))

;--------------------- CONTEO DE MINAS VECINAS--------------------------
;Calcular cuántas minas rodean cada celda (números 1-8 del Buscaminas)
(define (contar-minas-vecinas tb x y)
;Posición dentro del tablero
  (if (not (dentro? x y (tablero-columnas tb) (tablero-filas tb)))
      0 ; Fuera del tablero = 0 minas vecinas
      ; Usar función de conteo con lista de vecinos
      (contar-minas-en-posiciones 
        tb 
        (vecinos-de (idx x y (tablero-columnas tb)) 
                    (tablero-columnas tb) 
                    (tablero-filas tb)))))

; Contar minas en una lista específica de posiciones
(define (contar-minas-en-posiciones tb lista-indices)
  (cond
    ; Caso base: Lista vacía = 0 minas
    [(null? lista-indices) 0]
    [else
    ; Obtener celda en la primera posición
     (define celda-actual (tablero-obtener-celda-nodo tb (car lista-indices)))
      ; Sumar 1 si tiene mina, 0 si no tiene
     (if (and celda-actual (celda-mina? celda-actual))
         (+ 1 (contar-minas-en-posiciones tb (cdr lista-indices)))  ; Tiene mina
         (contar-minas-en-posiciones tb (cdr lista-indices)))]))    ; No tiene mina

;  Actualizar conteo de vecinos para TODO el tablero
; Despues de colocar minas, calcular números para cada celda
; Usa funcion auxiliar con indices
(define (actualizar-conteo-vecinos tablero)
  (define total (* (tablero-filas tablero) (tablero-columnas tablero)))
  (actualizar-conteo-aux tablero 0 total))

; Recursion por indices: Procesar cada celda del tablero secuencialmente
(define (actualizar-conteo-aux tablero indice total)
  (cond
    ; Se procesan todas las celdas
    [(>= indice total) tablero]
    [else
      ; Obtener la celda en posicion actual
     (define celda-actual (list-ref (tablero-celdas tablero) indice))
      ; ¿Es mina o celda normal?
     (if (celda-mina? celda-actual)
          ; MINA: No calcular vecinos, continuar con siguiente
         (actualizar-conteo-aux tablero (+ indice 1) total)
          ; CELDA NORMAL: Calcular y actualizar vecinos
         (actualizar-conteo-aux 
           (tablero-reemplazar-celda-nodo 
             tablero 
             indice 
              ; Crear nueva celda con conteo de vecinos
             (celda-establecer-vecinos 
               celda-actual 
              ; Contar minas vecinas en esta posicion
               (contar-minas-vecinas 
                 tablero 
                 (x-of indice (tablero-columnas tablero))
                 (y-of indice (tablero-columnas tablero)))))
           (+ indice 1) 
           total))]))

;--------------------- MODIFICACION DE CELDAS ---------------------
;  Funciones puras para cambiar estado de celdas sin mutación

; Cambiar estado de marcado (bandera) de una celda
(define (celda-marcar c nuevo-estado)
  (celda (celda-mina? c) (celda-vecinos c) (celda-descubierta? c) nuevo-estado))

; Marcar celda como descubierta (usuario hizo click)
(define (celda-descubrir c)
  (celda (celda-mina? c) (celda-vecinos c) #t (celda-marcada? c)))

; Actualizar número de minas vecinas en una celda
(define (celda-establecer-vecinos c nuevo-numero)
  (celda (celda-mina? c) nuevo-numero (celda-descubierta? c) (celda-marcada? c)))

; Alternar estado de marcado (poner/quitar bandera)
; Si está marcada -> desmarcar, si no está marcada -> marcar
(define (celda-alternar-marcado c)
  (celda-marcar c (not (celda-marcada? c))))

; --------------------- ACCESO Y MODIFICACION DE TABLERO ---------------------
; Funciones para obtener y modificar celdas del tablero de forma segura
; Siempre validar límites antes de acceder a la lista
(define (tablero-obtener-celda-xy tb x y)
  (if (dentro? x y (tablero-columnas tb) (tablero-filas tb))
      (tablero-obtener-celda-nodo tb (idx x y (tablero-columnas tb)))
      #f))

; Obtener celda usando índice lineal
; Verificar que el indice este en rango valido
; Usar list-ref para obtener elemento de la lista
(define (tablero-obtener-celda-nodo tb nodo)
  (if (and (>= nodo 0) (< nodo (* (tablero-filas tb) (tablero-columnas tb))))
      (list-ref (tablero-celdas tb) nodo)
      #f)) ; indice invalido

; Reemplazar celda usando coordenadas (x,y)
; Solo reemplazar si las coordenadas son validas
; Convertir a indice y usar funcion de nodo
(define (tablero-reemplazar-celda-xy tb x y nueva-celda)
  (if (dentro? x y (tablero-columnas tb) (tablero-filas tb))
      (tablero-reemplazar-celda-nodo tb (idx x y (tablero-columnas tb)) nueva-celda)
      tb)) ; Coordenadas inválidas, retornar tablero sin cambios

; Reemplazar celda usando indice lineal
; Crear nuevo tablero con lista modificada
; Solo reemplazar si el índice es válido
(define (tablero-reemplazar-celda-nodo tb nodo nueva-celda)
  (if (and (>= nodo 0) (< nodo (* (tablero-filas tb) (tablero-columnas tb))))
    ; CREAR: Nuevo tablero con celda reemplazada
      (tablero (tablero-filas tb)
               (tablero-columnas tb)
               (tablero-nivel tb)
               (reemplazar-elemento-lista (tablero-celdas tb) nodo nueva-celda))
      tb)) ; Indice invalido, retornar tablero sin cambios

; Reemplazar elemento en lista por indice
; Procesar lista elemento por elemento hasta encontrar posicion
; Construir nueva lista con elemento reemplazado
(define (reemplazar-elemento-lista lista indice nuevo-elemento)
  (cond
    [(null? lista) '()]                    ; Lista vacia
    [(< indice 0) lista]                   ; indice negativo invalido
    [(= indice 0) (cons nuevo-elemento (cdr lista))]  ; Posicion encontrada
    [else (cons (car lista)                ; Reconstruir lista
                (reemplazar-elemento-lista (cdr lista) (- indice 1) nuevo-elemento))]))

; --------------------- LOGICA DE JUEGO ---------------------
; Implementar las reglas principales del Buscaminas
; Caracteristicas: Descubrimiento, expansion automatica, marcado
; Descubrir celda y manejar cascada si es 0
(define (descubrir-celda tb x y)
  (define celda (tablero-obtener-celda-xy tb x y))
  (cond
    ; Validacion: Coordenadas invalidas
    [(not celda) tb]
    ; Ya esta descubierta, no hacer nada
    [(celda-descubierta? celda) tb]
    ; Esta marcada con bandera, no descubrir
    [(celda-marcada? celda) tb]
    ; Es mina -> descubrir (causa derrota)
    [(celda-mina? celda)
     (tablero-reemplazar-celda-xy tb x y (celda-descubrir celda))]
    ; No es mina -> descubrir y posible expansión
    [else  ; No es mina
     (define celda-descubierta (celda-descubrir celda))
     (define tb-actualizado (tablero-reemplazar-celda-xy tb x y celda-descubierta))
     ; ¿Expandir automaticamente?
     (if (= (celda-vecinos celda) 0)
         ; 0 vecinos -> descubrir celdas adyacentes
         (descubrir-vecinos-recursivo tb-actualizado x y)
         ;Mostrar número de vecinos
         tb-actualizado)]))

; Descubrir vecinos cuando celda tiene 0 minas alrededor
; Obtener vecinos válidos y aplicar descubrir-celda a cada uno
; Recursion: descubrir-celda puede a su vez activar más expansiones
(define (descubrir-vecinos-recursivo tb x y)
  ; Lista de indices de vecinos validos
  (define vecinos (vecinos-de (idx x y (tablero-columnas tb)) 
                              (tablero-columnas tb) 
                              (tablero-filas tb)))
  ; Intentar descubrir cada vecino
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
    [(celda-descubierta? celda) tb]  ; No se puede marcar si ya esta descubierta
    [else (tablero-reemplazar-celda-xy tb x y (celda-alternar-marcado celda))]))

;Obtener el estado completo de una celda para la interfaz
(define (obtener-estado-celda tb x y)
  (define celda (tablero-obtener-celda-xy tb x y))
  (cond
    ;Coordenadas invalidas o celda no existe
    [(not celda) 'invalida]
    
    ;Celda descubierta - verificar que contiene
    [(celda-descubierta? celda)
     (cond
       ;Es una mina descubierta (causa derrota)
       [(celda-mina? celda) 'mina-descubierta]
       ;Celda sin minas alrededor (se expande automaticamente)
       [(= (celda-vecinos celda) 0) 'vacia]
       ;Celda con minas vecinas (retorna el numero 1-8)
       [else (celda-vecinos celda)])]
    
    ;Celda marcada con bandera (no descubierta)
    [(celda-marcada? celda) 'marcada]
    
    ;Celda oculta sin marcar
    [else 'oculta]))

;Calcular el numero de minas restantes (no marcadas)
(define (obtener-minas-restantes tb)
  (- (tablero-contar-minas tb) 
     (tablero-contar-marcadas tb)))

; Contar banderas en una lista especifica de indices
(define (contar-banderas-en-lista tb lista-indices)
  (cond
    ;Caso base: lista vacia
    [(null? lista-indices) 0]
    ;Caso recursivo
    [else
     (define indice-actual (car lista-indices))
     (define celda-actual (tablero-obtener-celda-nodo tb indice-actual))
     (cond
       ;Si no existe la celda, continuar sin contar
       [(not celda-actual) 
        (contar-banderas-en-lista tb (cdr lista-indices))]
       ;Si esta marcada, sumar 1 y continuar
       [(celda-marcada? celda-actual)
        (+ 1 (contar-banderas-en-lista tb (cdr lista-indices)))]
       ;Si no esta marcada, continuar sin sumar
       [else
        (contar-banderas-en-lista tb (cdr lista-indices))])]))

; Verificar si todas las celdas estan descubiertas
(define (verificar-todas-descubiertas? tb)
  (verificar-todas-descubiertas-aux (tablero-celdas tb)))

(define (verificar-todas-descubiertas-aux celdas)
  (cond
    ;Si revisamos todas y ninguna fallo, retornar #t
    [(null? celdas) #t]
    ;Si encontramos una no descubierta, retornar #f
    [(not (celda-descubierta? (car celdas))) #f]
    ;Continuar con el resto
    [else (verificar-todas-descubiertas-aux (cdr celdas))]))

;Obtener estadisticas completas del tablero
    ;Retorna: lista de pares (simbolo . valor) con estadisticas
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
         #f  ; Hay una celda sin mina que no esta descubierta
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
; Obtener informacion sobre el tablero
(define (tablero-info tb)
  (define total-celdas (* (tablero-filas tb) (tablero-columnas tb)))
  (define total-minas (tablero-contar-minas tb))
  (define porcentaje-minas (/ total-minas total-celdas))
  (list (cons 'dimensiones (format "~ax~a" (tablero-columnas tb) (tablero-filas tb)))
        (cons 'total-celdas total-celdas)
        (cons 'total-minas total-minas)
        (cons 'porcentaje-minas (exact->inexact porcentaje-minas))
        (cons 'nivel (tablero-nivel tb))))

; Sugerir numero de minas para dimensiones dadas
(define (sugerir-minas filas columnas dificultad)
  (unless (and (>= filas 8) (<= filas 30) (>= columnas 8) (<= columnas 30))
    (error 'sugerir-minas "Dimensiones invalidas"))
  (define total-celdas (* filas columnas))
  (define porcentaje
    (case dificultad
      [(facil) 0.10]
      [(medio) 0.15]
      [(dificil) 0.20]
      [else 0.15]))
  (values (max 1 (inexact->exact (floor (* total-celdas porcentaje))))
          (max 1 (inexact->exact (floor (* total-celdas 0.05))))   ; minimo
          (min (- total-celdas 1) (inexact->exact (floor (* total-celdas 0.30)))))) ; maximo

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
 
 ; Validacion
 validar-dimensiones 
 
 ; Indexacion
 idx x-of y-of dentro?
 
 ; Vecinos
 vecinos-de vecinos8
 
 ; Modificacion de celdas
 celda-marcar celda-descubrir celda-establecer-vecinos celda-alternar-marcado
 
 ; Acceso y modificacion de tablero
 tablero-obtener-celda-xy tablero-obtener-celda-nodo
 tablero-reemplazar-celda-xy tablero-reemplazar-celda-nodo
 
 ; Logica del juego
 descubrir-celda marcar-celda
 verificar-victoria? verificar-derrota?
 
 ; Minas
 colocar-minas-aleatorias contar-minas-vecinas
 colocar-minas-en-posiciones ; 
 actualizar-conteo-vecinos   ; 
 
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
