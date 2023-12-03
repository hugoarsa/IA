;;; ---------------------------------------------------------
;;; ontologia_simple.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia_procesada.ttl
;;; :Date 03/12/2023 19:10:42

(defclass Persona
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; La edad de una persona particular
    (slot edad
        (type INTEGER)
        (create-accessor read-write))
    ;;; La nacionalidad particular de una persona
    (multislot nacionalidad
        (type STRING)
        (create-accessor read-write))
    ;;; El nombre de una entidad
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Autor
    (is-a Persona)
    (role concrete)
    (pattern-match reactive)
    (multislot haEscrito
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Nos indica si el autor esta vivo en el momento de hacer las preguntas al lector o no
    (slot vivo
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass Lector
    (is-a Persona)
    (role concrete)
    (pattern-match reactive)
    (multislot haLeido
        (type INSTANCE)
        (create-accessor read-write))
    (multislot hablaIdioma
        (type INSTANCE)
        (create-accessor read-write))
    (multislot prefiereAutor
        (type INSTANCE)
        (create-accessor read-write))
    (multislot prefiereGenero
        (type INSTANCE)
        (create-accessor read-write))
    ;;; La cantidad de dias a la semana en los que el usuario lee libros. Un usuario que lee mas amenudo podra afrontar lecuras mas extensas y complejas sin perder el hilo.
    (slot frecuencia_lectura
        (type INTEGER)
        (create-accessor read-write))
    ;;; Esto nos hace ver si el usuario esta intesado en autores extranjeros o nacionales. Esto es independiente del lenguaje en el que se presenta el libro pues un libro traducido a su idioma puede ser una obra extranjera que no le produzca interes o un libro en un idioma exotico puede ser una traduccion de un clasico de su propio pais.
    (slot interes_extranjero
        (type SYMBOL)
        (create-accessor read-write))
    ;;; El lugar de lectura va de menos a mas comodo. A algunos lugares se les atribuyen valores adicionales como la cama siendo un lugar comodo pero inpractico para leer en el cual las lecturas mas extensas pueden verse perjudicadas.
    (slot lugar_lectura
        (type SYMBOL)
        (create-accessor read-write))
    ;;; El momento del dia en el que se suelen realizar las sesiones de lectura por parte del usuario.
    (slot momento_de_lectura
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Si el lector es o no susceptible a las modas al rededor del mundo (valora de una obra que esta sea consumida por un gran numero de otros lectores)
    (slot susceptible_moda
        (type SYMBOL)
        (create-accessor read-write))
    ;;; El tiempo disponible del lector en horas semanales
    (slot tiempo_disponible
        (type INTEGER)
        (create-accessor read-write))
)

(defclass Genero
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot tieneAutorDestacado
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Indica si el genero literario goza de popularidad en el momento en el que el sistema basado en el conocimiento se usa
    (slot esta_de_moda
        (type SYMBOL)
        (create-accessor read-write))
    ;;; El nombre de una entidad
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Idioma
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; El nombre de una entidad
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Libro
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot contieneGenero
        (type INSTANCE)
        (create-accessor read-write))
    (slot estaEscritoEn
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Se denomina superventas a aquel libro que, gracias a la gran aceptacion que tiene entre el publico, pasa a formar parte de las listas de los mas vendidos.
    (slot best_seller
        (type SYMBOL)
        (create-accessor read-write))
    ;;; El numero de ejemplares vendidos del libro hasta la fecha
    (slot ejemplares_vendidos
        (type INTEGER)
        (create-accessor read-write))
    ;;; El ano de salida del libro en su primera edicion
    (slot fecha_salida
        (type INTEGER)
        (create-accessor read-write))
    ;;; El nombre de una entidad
    (slot nombre
        (type STRING)
        (create-accessor read-write))
    ;;; Numero de paginas discretas de un libro
    (slot numero_paginas
        (type INTEGER)
        (create-accessor read-write))
)

(definstances instances
    ([Accion] of Genero
         (nombre  "Accion")
    )

    ([Arabe] of Idioma
         (nombre  "Arabe")
    )

    ([Bengali] of Idioma
         (nombre  "Bengali")
    )

    ([Ciencia_ficcion] of Genero
         (nombre  "Ciencia_ficcion")
    )

    ([Crimen] of Genero
         (nombre  "Crimen")
    )

    ([Dmitri_Glujovski] of Autor
         (haEscrito  [Metro_2033])
         (vivo  "true")
         (edad  44)
         (nacionalidad  "Ruso")
         (nombre  "Dmitri Glujovski")
    )

    ([El_Hobbit] of Libro
         (contieneGenero  [Fantasia])
         (estaEscritoEn  [Ingles])
         (best_seller  "true")
         (ejemplares_vendidos  100000000)
         (fecha_salida  1937)
         (nombre  "El Hobbit")
         (numero_paginas  310)
    )

    ([El_Psicoanalista] of Libro
         (contieneGenero  [Crimen] [Suspense])
         (estaEscritoEn  [Ingles])
         (best_seller  "true")
         (ejemplares_vendidos  10000000)
         (fecha_salida  2002)
         (nombre  "El Psicoanalista")
         (numero_paginas  432)
    )

    ([El_Senor_de_los_anillos] of Libro
         (contieneGenero  [Fantasia])
         (estaEscritoEn  [Ingles])
         (best_seller  "true")
         (ejemplares_vendidos  150000000)
         (fecha_salida  1954)
         (nombre  "El Senor de los anillos")
         (numero_paginas  1392)
    )

    ([En_las_montanas_de_la_locura] of Libro
         (contieneGenero  [Misterio] [Terror])
         (estaEscritoEn  [Ingles])
         (best_seller  "false")
         (ejemplares_vendidos  1000000)
         (fecha_salida  1936 "En las montanas de la locura")
         (numero_paginas  176)
    )

    ([Espanol] of Idioma
         (nombre  "Espanol")
    )

    ([Fantasia] of Genero
         (tieneAutorDestacado  [JRR_Tolkien])
         (nombre  "Fantasia")
    )

    ([Frances] of Idioma
         (nombre  "Frances")
    )

    ([Francisco_de_Paula_Fernandez] of Autor
         (haEscrito  [La_chica_invisible])
         (vivo  "true")
         (edad  45)
         (nacionalidad  "Espanol")
         (nombre  "Francisco de Paula Fernandez")
    )

    ([Fundacion] of Libro
         (contieneGenero  [Ciencia_ficcion])
         (estaEscritoEn  [Ingles])
         (best_seller  "true")
         (ejemplares_vendidos  20000000)
         (fecha_salida  1951)
         (nombre  "Fundacion")
         (numero_paginas  230)
    )

    ([HP_Lovecraft] of Autor
         (haEscrito  [En_las_montanas_de_la_locura] [La_llamada_de_cthulhu])
         (vivo  "false")
         (edad  46)
         (nacionalidad  "Estadounidense")
         (nombre  "HP Lovecraft")
    )

    ([Hindi] of Idioma
         (nombre  "Hindi")
    )

    ([Ingles] of Idioma
         (nombre  "Ingles")
    )

    ([Isaac_Asimov] of Autor
         (haEscrito  [Fundacion] [La_ultima_pregunta])
         (vivo  "false")
         (edad  72)
         (nacionalidad  "Estadounidense" "Ruso")
         (nombre  "Isaac Asimov")
    )

    ([JRR_Tolkien] of Autor
         (haEscrito  [El_Hobbit] [El_Senor_de_los_anillos])
         (vivo  "false")
         (edad  81)
         (nacionalidad  "Britanico")
         (nombre  "JRR Tolkien")
    )

    ([John_Katzenbach] of Autor
         (haEscrito  [El_Psicoanalista])
         (vivo  "true")
         (edad  73)
         (nacionalidad  "Estadounidense")
         (nombre  "John Katzenbach")
    )

    ([Juvenil] of Genero
         (nombre  "Juvenil")
    )

    ([La_chica_invisible] of Libro
         (contieneGenero  [Crimen] [Juvenil] [Suspense])
         (estaEscritoEn  [Espanol])
         (best_seller  "true" "La chica invisible")
         (fecha_salida  2021)
         (numero_paginas  544)
    )

    ([La_llamada_de_cthulhu] of Libro
         (contieneGenero  [Misterio] [Terror])
         (estaEscritoEn  [Ingles])
         (best_seller  "true")
         (ejemplares_vendidos  3000000)
         (fecha_salida  1926)
         (nombre  "La llamada de cthulhu")
         (numero_paginas  96)
    )

    ([La_ultima_pregunta] of Libro
         (contieneGenero  [Ciencia_ficcion])
         (estaEscritoEn  [Ingles])
         (best_seller  "false")
         (ejemplares_vendidos  2500000)
         (fecha_salida  1956)
         (nombre  "La ultima pregunta")
         (numero_paginas  24)
    )

    ([Mandarin] of Idioma
         (nombre  "Mandarin")
    )

    ([Metro_2033] of Libro
         (contieneGenero  [Post_Apocaliptico] [Suspense])
         (estaEscritoEn  [Ruso])
         (best_seller  "true")
         (ejemplares_vendidos  1000000)
         (fecha_salida  2005)
         (nombre  "Metro 2033")
         (numero_paginas  544)
    )

    ([Misterio] of Genero
         (nombre  "Misterio")
    )

    ([Portugues] of Idioma
         (nombre  "Portugues")
    )

    ([Post_Apocaliptico] of Genero
         (nombre  "Post Apocaliptico")
    )

    ([Romance] of Genero
         (nombre  "Romance")
    )

    ([Ruso] of Idioma
         (nombre  "Ruso")
    )

    ([Suspense] of Genero
         (nombre  "Suspense")
    )

    ([Terror] of Genero
         (nombre  "Terror")
    )

    ([Urdu] of Idioma
         (nombre  "Urdu")
    )

)
