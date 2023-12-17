;;; ---------------------------------------------------------
;;; ontologia_completa.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia_completa.ttl
;;; :Date 16/12/2023 12:52:19

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

(defclass Autor "El autor como es bien conocido tiene sus vicios y costumbres e intentamos almacenar las mismas de cara a recomendar libros potencialmente similares o alcanzar a autores que potencialmente le vayan a interesar a un usuario que no se pronuncie fan declarado del mismo pero si que realmente muestre interes por aspectos distintivos de su estilo."
    (is-a Persona)
    (role concrete)
    (pattern-match reactive)
    (multislot destacaEnTemas
        (type INSTANCE)
        (create-accessor read-write))
    (multislot haEscrito
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Nos indica si el autor esta vivo en el momento de hacer las preguntas al lector o no
    (slot vivo
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass Lector "El lector es una clase del sujeto que analizamos y al que pretendemos recomendar los libros, nos basamos en sus preferencias concretas y abstractas, experiencias pasadas y conocimientos del mismo."
    (is-a Persona)
    (role concrete)
    (pattern-match reactive)
    (multislot leIncomoda
        (type INSTANCE)
        (create-accessor read-write))
    (multislot leInteresa
        (type INSTANCE)
        (create-accessor read-write))
    (multislot prefiereEditorial
        (type INSTANCE)
        (create-accessor read-write))
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
    ;;; Esta competencia es la relacionada con la comprensi�n el discurso en si mismo en un sentido de contenido, a menudo va ligada con la competencia linguistica pero no necesriamente. Pues una persona puede tener dislexia o problemas para la lectura y tener una alta comprensi�n del contenido que alberga el libro idependientemente del medio en el que se presenta.
    (multislot competencia_comprension
        (type INTEGER)
        (create-accessor read-write))
    ;;; Competencia relacionada con el dominio de la lengua del individuo. Esta lo hace capaz de entender construcciones linguisticas mas complejas, entender palabras especializadas y trabajar con expresiones y recursos literarios avanzados.
    (multislot competencia_linguistica
        (type INTEGER)
        (create-accessor read-write))
    ;;; Competencia relacionada con la capacidad de extraer de textos su mensaje y tematica por mucho que este este explicado de formas no evidentes mediante recursos literarios complejos y expuesto de forma creativa y compleja.
    (multislot competencia_tematica
        (type INTEGER)
        (create-accessor read-write))
    ;;; Como de susceptible es el usuario a las criticas de los libros.
    (multislot opinion_critica
        (type SYMBOL)
        (create-accessor read-write))
    ;;; una media ponderada sobre 100 de todas las criticas profesionales ofreccidas de los libros reconocidos por nuestro sistema.
    (multislot opinion_traduccion
        (type SYMBOL)
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
    ;;; El tiempo disponible del lector en horas diarias
    (slot tiempo_disponible
        (type INTEGER)
        (create-accessor read-write))
    (slot grado_educacion
        (type SYMBOL)
        (allowed-values Nula Infantil Primaria Secundaria Bachillerato Formacion_Profesional Universitaria Postgrado)
        (create-accessor read-write))
)

(defclass Editorial "Las editoriales son conocidas por a menudo editar libros de un determinado tipo o con unos valores similares. Un usuario que demuestre predileccion por un editor particular demostrara un interes reincidente en otros de la misma."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Tema "Los temas que un libro trata a menudo pueden suponer puntos fuertes para su eleccion o motivos de descarte para el mismo. Sobretodo si estos entran dentro de categorias sensibles como la violencia o el contenido sexual."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(defclass Genero "El genero de un libro sirve para indicar caracteristicas comunes entre libros que tratan sobre temas similares y desarrollan historias comparables. Un usuario que disfruta de un genero es altamente probable que demuestre interes por otros libros del mismo."
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

(defclass Idioma "El idioma resulta una evidente barrera de cara a la eleccion de libros adecuados para un lector pues si este no domina lo sufciente el mismo no podra fisicamente realizar la lectura."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; El nombre de una entidad
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Libro "Los libros establecen el eje central de toda la practica siendo estos el objeto unico de recomendacion con el que interactuamos. Todos los datos se almacenan al rededor de los mismos."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot contieneTema
        (type INSTANCE)
        (create-accessor read-write))
    (multislot estaEditadoPor
        (type INSTANCE)
        (create-accessor read-write))
    (multislot estaTraducidoA
        (type INSTANCE)
        (create-accessor read-write))
    (multislot contieneGenero
        (type INSTANCE)
        (create-accessor read-write))
    (slot estaEscritoEn
        (type INSTANCE)
        (create-accessor read-write))
    ;;; La complejidad asociada como se estructura el discurso y la dificultad que esto aporta a la lectura. Esto se refiere a aspectos no estrictamente linguisticos sino a nivel de contenido del discurso expresado a traves de la lengua. Por lo tanto, aqui tenemos en cuenta si el estilo y la organizacion es simple (se organiza de forma cronologica, uniforme, evidentemente conexa y predecible) o compleja (diferentes situaciones simultaneas, conexiones entre eventos, viajes temporales, elementos implicitos). Se construye con una enumeracion gradiente con cuatro estados de mas a menos complejo
    (multislot complejidad_discurso
        (type SYMBOL)
        (create-accessor read-write))
    ;;; La complejidad asociada a los aspectos linguisticos del libro. Esto se refiere a aspectos formales de la lengua del texto presente en el libro. Aspectos como la complejidad de las oraciones que forman (si son simples y cortas o largas y llenas de relaciones sintacticas complejas), los juegos de palabras o recursos literarios usados y el lexico utilizado. Se construye con una enumeracion gradiente con cuatro estados de mas a menos complejo
    (multislot complejidad_linguistica
        (type SYMBOL)
        (create-accessor read-write))
    ;;; La complejidad asociada como se desarollan los distintos temas en el libro segun la sofisticacion con la que los mismos se tratan. Se construye con una enumeracion gradiente con cuatro estados de mas a menos complejo. Un libro puede presentar los temas que trata de formas muy expl�citas y literales. Otros podr�an ocultar sus mensajes detr�s de complejos arcos que se prolongan durante grandes partes del libro y requier�n un gran nivel de atenci�n por parte del lector.
    (multislot complejidad_tematica
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Este valor nos indica si al usuario le interesan los autores contemporaneos a ellos o no
    (multislot opinion_contemporaneo
        (type SYMBOL)
        (create-accessor read-write))
    ;;; El porcentaje de critica es una media ponderada de los resultados que criticicos profesionales le dan a este texto. No necesariamente son un indicador de calidad o gusto pues la subjetividad juega un papel importante pero pueden afectar al interes que generan en el publico.
    (multislot porcentaje_critica
        (type SYMBOL)
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
         (nombre  Accion)
         (esta_de_moda FALSE)
    )

    ([Arabe] of Idioma
         (nombre  Arabe)
    )

    ([Bengali] of Idioma
         (nombre  Bengali)
    )

    ([Ciencia_ficcion] of Genero
         (nombre  Ciencia_ficcion)
         (esta_de_moda FALSE)
    )

    ([Crimen] of Genero
         (nombre  Crimen)
         (esta_de_moda FALSE)
    )

    ([Dmitri_Glujovski] of Autor
         (haEscrito  [MAIN::Metro_2033])
         (vivo  TRUE)
         (edad  44)
         (nacionalidad  Ruso)
         (nombre  Dmitri_Glujovski)
    )

    ([El_Hobbit] of Libro
         (contieneGenero  [MAIN::Fantasia])
         (estaEscritoEn  [MAIN::Ingles])
         (best_seller  TRUE)
         (ejemplares_vendidos  100000000)
         (fecha_salida  1937)
         (nombre  El_Hobbit)
         (numero_paginas  310)
    )

    ([El_Psicoanalista] of Libro
         (contieneGenero  [MAIN::Crimen] [MAIN::Suspense])
         (estaEscritoEn  [MAIN::Ingles])
         (best_seller  TRUE)
         (ejemplares_vendidos  10000000)
         (fecha_salida  2002)
         (nombre  El_Psicoanalista)
         (numero_paginas  432)
    )

    ([El_Senor_de_los_anillos] of Libro
         (contieneGenero  [MAIN::Fantasia])
         (estaEscritoEn  [MAIN::Ingles])
         (best_seller  TRUE)
         (ejemplares_vendidos  150000000)
         (fecha_salida  1954)
         (nombre  El_Senor_de_los_anillos)
         (numero_paginas  1392)
    )

    ([En_las_montanas_de_la_locura] of Libro
         (contieneGenero  [MAIN::Misterio] [MAIN::Terror])
         (estaEscritoEn  [MAIN::Ingles])
         (best_seller  FALSE)
         (ejemplares_vendidos  1000000)
         (fecha_salida  1936)
         (nombre  En_las_montanas_de_la_locura)
         (numero_paginas  176)
    )

    ([Espanol] of Idioma
         (nombre  Espanol)
    )

    ([Fantasia] of Genero
         (tieneAutorDestacado  [MAIN::JRR_Tolkien])
         (nombre  Fantasia)
         (esta_de_moda FALSE)
    )

    ([Frances] of Idioma
         (nombre  Frances)
    )

    ([Francisco_de_Paula_Fernandez] of Autor
         (haEscrito  [MAIN::La_chica_invisible])
         (vivo  TRUE)
         (edad  45)
         (nacionalidad  Espanol)
         (nombre  Francisco_de_Paula_Fernandez)
    )

    ([Fundacion] of Libro
         (contieneGenero  [MAIN::Ciencia_ficcion])
         (estaEscritoEn  [MAIN::Ingles])
         (best_seller  TRUE)
         (ejemplares_vendidos  20000000)
         (fecha_salida  1951)
         (nombre  Fundacion)
         (numero_paginas  230)
    )

    ([HP_Lovecraft] of Autor
         (haEscrito  [MAIN::En_las_montanas_de_la_locura] [MAIN::La_llamada_de_cthulhu])
         (vivo  FALSE)
         (edad  46)
         (nacionalidad  Estadounidense)
         (nombre  HP_Lovecraft)
    )

    ([Hindi] of Idioma
         (nombre  Hindi)
    )

    ([Ingles] of Idioma
         (nombre  Ingles)
    )

    ([Isaac_Asimov] of Autor
         (haEscrito  [MAIN::Fundacion] [MAIN::La_ultima_pregunta])
         (vivo  FALSE)
         (edad  72)
         (nacionalidad  Estadounidense Ruso)
         (nombre  Isaac_Asimov)
    )

    ([JRR_Tolkien] of Autor
         (haEscrito  [MAIN::El_Hobbit] [MAIN::El_Senor_de_los_anillos])
         (vivo  FALSE)
         (edad  81)
         (nacionalidad  Britanico)
         (nombre  JRR_Tolkien)
    )

    ([John_Katzenbach] of Autor
         (haEscrito  [MAIN::El_Psicoanalista])
         (vivo  TRUE)
         (edad  73)
         (nacionalidad  Estadounidense)
         (nombre  John_Katzenbach)
    )

    ([Juvenil] of Genero
         (nombre  Juvenil)
         (esta_de_moda TRUE)
    )

    ([La_chica_invisible] of Libro
         (contieneGenero  [MAIN::Crimen] [MAIN::Juvenil] [MAIN::Suspense])
         (estaEscritoEn  [MAIN::Espanol])
         (best_seller  TRUE)
         (ejemplares_vendidos  200000)
         (fecha_salida  2021)
         (nombre  La_chica_invisible)
         (numero_paginas  544)
    )

    ([La_llamada_de_cthulhu] of Libro
         (contieneGenero  [MAIN::Misterio] [MAIN::Terror])
         (estaEscritoEn  [MAIN::Ingles])
         (best_seller  TRUE)
         (ejemplares_vendidos  3000000)
         (fecha_salida  1926)
         (nombre  La_llamada_de_cthulhu)
         (numero_paginas  96)
    )

    ([La_ultima_pregunta] of Libro
         (contieneGenero  [MAIN::Ciencia_ficcion])
         (estaEscritoEn  [MAIN::Ingles])
         (best_seller  FALSE)
         (ejemplares_vendidos  2500000)
         (fecha_salida  1956)
         (nombre  La_ultima_pregunta)
         (numero_paginas  24)
    )

    ([Mandarin] of Idioma
         (nombre  Mandarin)
    )

    ([Metro_2033] of Libro
         (contieneGenero  [MAIN::Post_Apocaliptico] [MAIN::Suspense])
         (estaEscritoEn  [MAIN::Ruso])
         (best_seller  TRUE)
         (ejemplares_vendidos  1000000)
         (fecha_salida  2005)
         (nombre  Metro_2033)
         (numero_paginas  544)
    )

    ([Misterio] of Genero
         (nombre  Misterio)
         (esta_de_moda FALSE)
    )

    ([Portugues] of Idioma
         (nombre  Portugues)
    )

    ([Post_Apocaliptico] of Genero
         (nombre  Post_Apocaliptico)
         (esta_de_moda FALSE)
    )

    ([Romance] of Genero
         (nombre  Romance)
         (esta_de_moda TRUE)
    )

    ([Ruso] of Idioma
         (nombre  Ruso)
    )

    ([Suspense] of Genero
         (nombre  Suspense)
         (esta_de_moda FALSE)
    )

    ([Terror] of Genero
         (tieneAutorDestacado  [MAIN::HP_Lovecraft])
         (nombre  Terror)
         (esta_de_moda FALSE)
    )

    ([Urdu] of Idioma
         (nombre  Urdu)
    )

)
