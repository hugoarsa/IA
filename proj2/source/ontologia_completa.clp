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
        ;;(allowed-values Estadounidense Britanico Canadiense Australiano Frances Aleman Japones Chino Indio Brasileno Mexicano Ruso Italiano Espanol Sudafricano Nigeriano Argentino Egipcio Turco Sueco Noruego Danes Holandes Belga Suizo Portugues Griego Polaco Hungaro Checo Eslovaco Croata Serbio Bosnio Esloveno Sueco Finlandes Irlandes Islandes Montenegrino)
        (create-accessor read-write))
    ;;; El nombre de una entidad
    (slot nombre
        (type SYMBOL)
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
        (allowed-values TRUE FALSE)
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
    (slot competencia_comprension
        (type INTEGER)
        (create-accessor read-write))
    ;;; Competencia relacionada con el dominio de la lengua del individuo. Esta lo hace capaz de entender construcciones linguisticas mas complejas, entender palabras especializadas y trabajar con expresiones y recursos literarios avanzados.
    (slot competencia_linguistica
        (type INTEGER)
        (create-accessor read-write))
    ;;; Competencia relacionada con la capacidad de extraer de textos su mensaje y tematica por mucho que este este explicado de formas no evidentes mediante recursos literarios complejos y expuesto de forma creativa y compleja.
    (slot competencia_tematica
        (type INTEGER)
        (create-accessor read-write))
    ;;; Como de susceptible es el usuario a las criticas de los libros.
    (slot opinion_critica
        (type INTEGER)
        (create-accessor read-write))
    ;;; una media ponderada sobre 100 de todas las criticas profesionales ofreccidas de los libros reconocidos por nuestro sistema.
    (slot opinion_traduccion
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Este valor nos indica si al usuario le interesan los autores contemporaneos a ellos o no
    (slot opinion_contemporaneo
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
        (type INTEGER)
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
    (slot nombre
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass Tema "Los temas que un libro trata a menudo pueden suponer puntos fuertes para su eleccion o motivos de descarte para el mismo. Sobretodo si estos entran dentro de categorias sensibles como la violencia o el contenido sexual."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nombre
        (type SYMBOL)
        (create-accessor read-write))
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
        (allowed-values TRUE FALSE)
        (create-accessor read-write))
    ;;; El nombre de una entidad
    (slot nombre
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass Idioma "El idioma resulta una evidente barrera de cara a la eleccion de libros adecuados para un lector pues si este no domina lo sufciente el mismo no podra fisicamente realizar la lectura."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; El nombre de una entidad
    (slot nombre
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass Libro "Los libros establecen el eje central de toda la practica siendo estos el objeto unico de recomendacion con el que interactuamos. Todos los datos se almacenan al rededor de los mismos."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot contieneTema
        (type INSTANCE)
        (create-accessor read-write))
    (slot estaEditadoPor
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
    (slot complejidad_discurso
        (type INTEGER)
		(range 1 10)
        (create-accessor read-write))
    ;;; La complejidad asociada a los aspectos linguisticos del libro. Esto se refiere a aspectos formales de la lengua del texto presente en el libro. Aspectos como la complejidad de las oraciones que forman (si son simples y cortas o largas y llenas de relaciones sintacticas complejas), los juegos de palabras o recursos literarios usados y el lexico utilizado. Se construye con una enumeracion gradiente con cuatro estados de mas a menos complejo
    (slot complejidad_linguistica
        (type INTEGER)
		(range 1 10)
        (create-accessor read-write))
    ;;; La complejidad asociada como se desarollan los distintos temas en el libro segun la sofisticacion con la que los mismos se tratan. Se construye con una enumeracion gradiente con cuatro estados de mas a menos complejo. Un libro puede presentar los temas que trata de formas muy expl�citas y literales. Otros podr�an ocultar sus mensajes detr�s de complejos arcos que se prolongan durante grandes partes del libro y requier�n un gran nivel de atenci�n por parte del lector.
    (slot complejidad_tematica
        (type INTEGER)
		(range 1 10)
        (create-accessor read-write))
    ;;; El porcentaje de critica es una media ponderada de los resultados que criticicos profesionales le dan a este texto. No necesariamente son un indicador de calidad o gusto pues la subjetividad juega un papel importante pero pueden afectar al interes que generan en el publico.
    (slot porcentaje_critica
        (type INTEGER)
		(range 1 100)
        (create-accessor read-write))
    ;;; Se denomina superventas a aquel libro que, gracias a la gran aceptacion que tiene entre el publico, pasa a formar parte de las listas de los mas vendidos.
    (slot best_seller
        (type SYMBOL)
        (allowed-values TRUE FALSE)
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
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Numero de paginas discretas de un libro
    (slot numero_paginas
        (type INTEGER)
        (create-accessor read-write))
)

(definstances instances
([Chino] of Idioma
    (nombre Chino)
)

([Espanol] of Idioma
    (nombre Espanol)
)

([Ingles] of Idioma
    (nombre Ingles)
)

([Hindi] of Idioma
    (nombre Hindi)
)

([Arabe] of Idioma
    (nombre Arabe)
)

([Bengali] of Idioma
    (nombre Bengal)
)

([Portugues] of Idioma
    (nombre Portugues)
)

([Ruso] of Idioma
    (nombre Ruso)
)

([Japones] of Idioma
    (nombre Japones)
)

([Panyabi] of Idioma
    (nombre Panyabi)
)

([Aleman] of Idioma
    (nombre Aleman)
)

([Telugu] of Idioma
    (nombre Telugu)
)

([Marati] of Idioma
    (nombre Marati)
)

([Frances] of Idioma
    (nombre Frances)
)

([Cruilla] of Editorial
    (nombre Cruilla)
)

([Planeta] of Editorial
    (nombre Planeta)
)

([Santillana] of Editorial
    (nombre Santillana)
)

([Anagrama] of Editorial
    (nombre Anagrama)
)

([Alfaguara] of Editorial
    (nombre Alfaguara)
)

([Penguin_Random_House] of Editorial
    (nombre Penguin_Random_House)
)

([Grijalbo] of Editorial
    (nombre Grijalbo)
)

([DeBolsillo] of Editorial
    (nombre DeBolsillo)
)

([Akal] of Editorial
    (nombre Akal)
)

([Taurus] of Editorial
    (nombre Taurus)
)

([Novela_Negra] of Genero
    (nombre Novela_Negra)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Agatha_Christie] [MAIN::Raymond_Chandler])
)

([Aventura] of Genero
    (nombre Aventura)
    (esta_de_moda FALSE)
    (tieneAutorDestacado [MAIN::Jules_Verne] [MAIN::H_Rider_Haggard])
)

([Romance] of Genero
    (nombre Romance)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Jane_Austen] [MAIN::Nicholas_Sparks])
)

([Fantasia_Epica] of Genero
    (nombre Fantasia_Epica)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Brandon_Sanderson] [MAIN::George_RR_Martin])
)

([Ciencia_Ficcion_Cyberpunk] of Genero
    (nombre Ciencia_Ficcion_Cyberpunk)
    (esta_de_moda FALSE)
    (tieneAutorDestacado [MAIN::William_Gibson] [MAIN::Neal_Stephenson])
)

([Drama] of Genero
    (nombre Drama)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Anton_Chekhov] [MAIN::Tennessee_Williams])
)

([Humor] of Genero
    (nombre Humor)
    (esta_de_moda FALSE)
    (tieneAutorDestacado [MAIN::Terry_Pratchett] [MAIN::Douglas_Adams])
)

([Poesia] of Genero
    (nombre Poesia)
    (esta_de_moda FALSE)
    (tieneAutorDestacado [MAIN::Pablo_Neruda] [MAIN::Emily_Dickinson])
)

([Thriller] of Genero
    (nombre Thriller)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Dan_Brown] [MAIN::Gillian_Flynn])
)

([Ciencia_Ficcion_Distopica] of Genero
    (nombre Ciencia_Ficcion_Distopica)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Margaret_Atwood] [MAIN::Aldous_Huxley])
)

([Misterio] of Genero
    (nombre Misterio)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Dashiell_Hammett] [MAIN::Agatha_Christie])
)

([Fantasia_Urbana] of Genero
    (nombre Fantasia_Urbana)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Jim_Butcher] [MAIN::Cassandra_Clare])
)

([Literatura_Infantil] of Genero
    (nombre Literatura_Infantil)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Dr_Seuss] [MAIN::Roald_Dahl])
)

([Historico_Ficcion] of Genero
    (nombre Historico_Ficcion)
    (esta_de_moda FALSE)
    (tieneAutorDestacado [MAIN::Ken_Follett] [MAIN::Hilary_Mantel])
)

([Misterio_Thriller] of Genero
    (nombre Misterio_Thriller)
    (esta_de_moda TRUE)
    (tieneAutorDestacado [MAIN::Gillian_Flynn] [MAIN::Harlan_Coben])
)

([Agatha_Christie] of Autor
    (haEscrito [MAIN::Asesinato_en_el_Orient_Express] [MAIN::Diez_Negritos])
    (vivo TRUE)
    (edad 85) ; Edad estimada para Agatha Christie a lo largo de su carrera.
    (nacionalidad Britanico)
    (nombre Agatha_Christie)
)

([Raymond_Chandler] of Autor
    (haEscrito [MAIN::El_Sueno_Eterno] [MAIN::El_Largo_Adios])
    (vivo FALSE)
    (edad 71) ; Edad estimada para Raymond Chandler a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Raymond_Chandler)
)

([Jane_Austen] of Autor
    (haEscrito [MAIN::Orgullo_y_Prejuicio] [MAIN::Sentido_y_Sensibilidad])
    (vivo FALSE)
    (edad 41) ; Edad estimada para Jane Austen a lo largo de su carrera.
    (nacionalidad Britanico)
    (nombre Jane_Austen)
)

([Nicholas_Sparks] of Autor
    (haEscrito [MAIN::El_Cuaderno_de_Noah] [MAIN::Un_Paseo_para_Recordar])
    (vivo TRUE)
    (edad 56) ; Edad estimada para Nicholas Sparks a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Nicholas_Sparks)
)

([Brandon_Sanderson] of Autor
    (haEscrito [MAIN::El_Camino_de_los_Reyes] [MAIN::Mistborn])
    (vivo TRUE)
    (edad 45) ; Edad estimada para Brandon Sanderson a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Brandon_Sanderson)
)

([George_RR_Martin] of Autor
    (haEscrito [MAIN::Juego_de_Tronos] [MAIN::Choque_de_Reyes])
    (vivo TRUE)
    (edad 73) ; Edad estimada para George R.R. Martin a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre George_RR_Martin)
)

([William_Gibson] of Autor
    (haEscrito [MAIN::Neuromante] [MAIN::Conde_Cero])
    (vivo TRUE)
    (edad 74) ; Edad estimada para William Gibson a lo largo de su carrera.
    (nacionalidad Canadiense)
    (nombre William_Gibson)
)

([Neal_Stephenson] of Autor
    (haEscrito [MAIN::Snow_Crash] [MAIN::Cryptonomicon])
    (vivo TRUE)
    (edad 62) ; Edad estimada para Neal Stephenson a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Neal_Stephenson)
)

([Anton_Chekhov] of Autor
    (haEscrito [MAIN::El_Jardin_de_los_Cerezos] [MAIN::La_Gaviota])
    (vivo FALSE)
    (edad 44) ; Edad estimada para Anton Chekhov a lo largo de su carrera.
    (nacionalidad Ruso)
    (nombre Anton_Chekhov)
)

([Tennessee_Williams] of Autor
    (haEscrito [MAIN::Un_Tranvia_llamado_Deseo] [MAIN::La_Noche_de_la_Iguana])
    (vivo FALSE)
    (edad 71) ; Edad estimada para Tennessee Williams a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Tennessee_Williams)
)

([Terry_Pratchett] of Autor
    (haEscrito [MAIN::Mort] [MAIN::Guardias!_Guardias!])
    (vivo FALSE)
    (edad 66) ; Edad estimada para Terry Pratchett a lo largo de su carrera.
    (nacionalidad Britanico)
    (nombre Terry_Pratchett)
)

([Douglas_Adams] of Autor
    (haEscrito [MAIN::Guia_del_Autoestopista_Galactico] [MAIN::El_Restaurante_del_Fin_del_Mundo])
    (vivo FALSE)
    (edad 49) ; Edad estimada para Douglas Adams a lo largo de su carrera.
    (nacionalidad Britanico)
    (nombre Douglas_Adams)
)

([Pablo_Neruda] of Autor
    (haEscrito [MAIN::Veinte_Poemas_de_Amor_y_Una_Cancion_Desenfrenada] [MAIN::Canto_General])
    (vivo FALSE)
    (edad 69) ; Edad estimada para Pablo Neruda a lo largo de su carrera.
    (nacionalidad Chileno)
    (nombre Pablo_Neruda)
)

([Emily_Dickinson] of Autor
    (haEscrito [MAIN::Poemas] [MAIN::Cartas])
    (vivo FALSE)
    (edad 55) ; Edad estimada para Emily Dickinson a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Emily_Dickinson)
)

([Dan_Brown] of Autor
    (haEscrito [MAIN::El_Codigo_Da_Vinci] [MAIN::Angeles_y_Demonios])
    (vivo TRUE)
    (edad 58) ; Edad estimada para Dan Brown a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Dan_Brown)
)

([Gillian_Flynn] of Autor
    (haEscrito [MAIN::Perdida] [MAIN::Lugares_Oscuros])
    (vivo TRUE)
    (edad 51) ; Edad estimada para Gillian Flynn a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Gillian_Flynn)
)

([Margaret_Atwood] of Autor
    (haEscrito [MAIN::El_Cuento_de_la_Criada] [MAIN::Oryx_y_Crake])
    (vivo TRUE)
    (edad 82) ; Edad estimada para Margaret Atwood a lo largo de su carrera.
    (nacionalidad Canadiense)
    (nombre Margaret_Atwood)
)

([Aldous_Huxley] of Autor
    (haEscrito [MAIN::Un_Mundo_Feliz] [MAIN::Las_Puertas_de_la_Percepcion])
    (vivo FALSE)
    (edad 69) ; Edad estimada para Aldous Huxley a lo largo de su carrera.
    (nacionalidad Britanico)
    (nombre Aldous_Huxley)
)

([Dashiell_Hammett] of Autor
    (haEscrito [MAIN::El_Halcon_Maltes] [MAIN::Cosecha_Roja])
    (vivo FALSE)
    (edad 66) ; Edad estimada para Dashiell Hammett a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Dashiell_Hammett)
)

([Harlan_Coben] of Autor
    (haEscrito [MAIN::No_Hay_Segunda_Oportunidad] [MAIN::Desaparecida])
    (vivo TRUE)
    (edad 60) ; Edad estimada para Harlan Coben a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Harlan_Coben)
)

([Jim_Butcher] of Autor
    (haEscrito [MAIN::Tormenta] [MAIN::Furia])
    (vivo TRUE)
    (edad 50) ; Edad estimada para Jim Butcher a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Jim_Butcher)
)

([Cassandra_Clare] of Autor
    (haEscrito [MAIN::Cazadores_de_Sombras] [MAIN::El_Principito_Mecanico])
    (vivo TRUE)
    (edad 49) ; Edad estimada para Cassandra Clare a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Cassandra_Clare)
)

([Dr_Seuss] of Autor
    (haEscrito [MAIN::El_Gato_con_Sombrero] [MAIN::El_Lorax])
    (vivo FALSE)
    (edad 87) ; Edad estimada para Dr. Seuss a lo largo de su carrera.
    (nacionalidad Estadounidense)
    (nombre Dr_Seuss)
)

([Roald_Dahl] of Autor
    (haEscrito [MAIN::Charlie_y_la_Fabrica_de_Chocolate] [MAIN::Matilda])
    (vivo FALSE)
    (edad 74) ; Edad estimada para Roald Dahl a lo largo de su carrera.
    (nacionalidad Britanico)
    (nombre Roald_Dahl)
)

([Ken_Follett] of Autor
    (haEscrito [MAIN::Los_Pilares_de_la_Tierra] [MAIN::Un_Mundo_Sin_Fin])
    (vivo TRUE)
    (edad 72) ; Edad estimada para Ken Follett a lo largo de su carrera.
    (nacionalidad Britanico)
    (nombre Ken_Follett)
)

([Hilary_Mantel] of Autor
    (haEscrito [MAIN::Wolf_Hall] [MAIN::Bring_Up_the_Bodies])
    (vivo TRUE)
    (edad 69) ; Edad estimada para Hilary Mantel a lo largo de su carrera.
    (nacionalidad Britanico)
    (nombre Hilary_Mantel)
)

([Asesinato_en_el_Orient_Express] of Libro
    (contieneGenero [MAIN::Misterio] [MAIN::Misterio_Thriller])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Cruilla])
    (best_seller TRUE)
    (ejemplares_vendidos 30000000)
    (fecha_salida 1934)
    (nombre Asesinato_en_el_Orient_Express)
    (numero_paginas 256)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 4)
    (porcentaje_critica 94)
)

([Diez_Negritos] of Libro
    (contieneGenero [MAIN::Misterio] [MAIN::Misterio_Thriller])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Planeta])
    (best_seller TRUE)
    (ejemplares_vendidos 100000000)
    (fecha_salida 1939)
    (nombre Diez_Negritos)
    (numero_paginas 272)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 4)
    (porcentaje_critica 92)
)

([El_Sueno_Eterno] of Libro
    (contieneGenero [MAIN::Novela_Negra])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Santillana])
    (best_seller TRUE)
    (ejemplares_vendidos 10000000)
    (fecha_salida 1939)
    (nombre El_Sueno_Eterno)
    (numero_paginas 300)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 4)
    (porcentaje_critica 90)
)

([El_Largo_Adios] of Libro
    (contieneGenero [MAIN::Novela_Negra])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Anagrama])
    (best_seller TRUE)
    (ejemplares_vendidos 8000000)
    (fecha_salida 1953)
    (nombre El_Largo_Adios)
    (numero_paginas 382)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 4)
    (porcentaje_critica 91)
)

([Orgullo_y_Prejuicio] of Libro
    (contieneGenero [MAIN::Romance])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Alfaguara])
    (best_seller TRUE)
    (ejemplares_vendidos 30000000)
    (fecha_salida 1813)
    (nombre Orgullo_y_Prejuicio)
    (numero_paginas 432)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 4)
    (porcentaje_critica 95)
)

([Sentido_y_Sensibilidad] of Libro
    (contieneGenero [MAIN::Romance])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Penguin_Random_House])
    (best_seller TRUE)
    (ejemplares_vendidos 20000000)
    (fecha_salida 1811)
    (nombre Sentido_y_Sensibilidad)
    (numero_paginas 368)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 4)
    (porcentaje_critica 94)
)

([El_Cuaderno_de_Noah] of Libro
    (contieneGenero [MAIN::Romance])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Grijalbo])
    (best_seller TRUE)
    (ejemplares_vendidos 15000000)
    (fecha_salida 1996)
    (nombre El_Cuaderno_de_Noah)
    (numero_paginas 213)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 4)
    (porcentaje_critica 90)
)

([Un_Paseo_para_Recordar] of Libro
    (contieneGenero [MAIN::Romance])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::DeBolsillo])
    (best_seller TRUE)
    (ejemplares_vendidos 10000000)
    (fecha_salida 1999)
    (nombre Un_Paseo_para_Recordar)
    (numero_paginas 240)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 4)
    (porcentaje_critica 89)
)

([El_Camino_de_los_Reyes] of Libro
    (contieneGenero [MAIN::Fantasia_Epica])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Akal])
    (best_seller TRUE)
    (ejemplares_vendidos 7000000)
    (fecha_salida 2010)
    (nombre El_Camino_de_los_Reyes)
    (numero_paginas 1250)
    (complejidad_discurso 5)
    (complejidad_linguistica 6)
    (complejidad_tematica 5)
    (porcentaje_critica 92)
)

([Mistborn] of Libro
    (contieneGenero [MAIN::Fantasia_Epica])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Taurus])
    (best_seller TRUE)
    (ejemplares_vendidos 5000000)
    (fecha_salida 2006)
    (nombre Mistborn)
    (numero_paginas 541)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 4)
    (porcentaje_critica 91)
)

([Juego_de_Tronos] of Libro
    (contieneGenero [MAIN::Fantasia_Epica] [MAIN::Drama])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Taurus])
    (best_seller TRUE)
    (ejemplares_vendidos 25000000)
    (fecha_salida 1996)
    (nombre Juego_de_Tronos)
    (numero_paginas 694)
    (complejidad_discurso 5)
    (complejidad_linguistica 6)
    (complejidad_tematica 5)
    (porcentaje_critica 96)
)

([Choque_de_Reyes] of Libro
    (contieneGenero [MAIN::Fantasia_Epica] [MAIN::Drama])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Taurus])
    (best_seller TRUE)
    (ejemplares_vendidos 20000000)
    (fecha_salida 1998)
    (nombre Choque_de_Reyes)
    (numero_paginas 768)
    (complejidad_discurso 5)
    (complejidad_linguistica 6)
    (complejidad_tematica 5)
    (porcentaje_critica 95)
)

([Neuromante] of Libro
    (contieneGenero [MAIN::Ciencia_Ficcion_Cyberpunk])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Cruilla])
    (best_seller TRUE)
    (ejemplares_vendidos 6000000)
    (fecha_salida 1984)
    (nombre Neuromante)
    (numero_paginas 271)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 5)
    (porcentaje_critica 93)
)

([Conde_Cero] of Libro
    (contieneGenero [MAIN::Ciencia_Ficcion_Cyberpunk])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Santillana])
    (best_seller TRUE)
    (ejemplares_vendidos 4000000)
    (fecha_salida 1986)
    (nombre Conde_Cero)
    (numero_paginas 278)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 5)
    (porcentaje_critica 91)
)

([Snow_Crash] of Libro
    (contieneGenero [MAIN::Ciencia_Ficcion_Cyberpunk])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Santillana])
    (best_seller TRUE)
    (ejemplares_vendidos 3000000)
    (fecha_salida 1992)
    (nombre Snow_Crash)
    (numero_paginas 440)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 5)
    (porcentaje_critica 92)
)

([Cryptonomicon] of Libro
    (contieneGenero [MAIN::Ciencia_Ficcion_Cyberpunk] [MAIN::Historico_Ficcion])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Planeta])
    (best_seller TRUE)
    (ejemplares_vendidos 500000)
    (fecha_salida 1999)
    (nombre Cryptonomicon)
    (numero_paginas 928)
    (complejidad_discurso 5)
    (complejidad_linguistica 6)
    (complejidad_tematica 5)
    (porcentaje_critica 94)
)

([El_Jardin_de_los_Cerezos] of Libro
    (contieneGenero [MAIN::Drama])
    (estaEscritoEn [MAIN::Ruso])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Ingles] [MAIN::Frances])
    (estaEditadoPor [MAIN::DeBolsillo])
    (best_seller FALSE)
    (ejemplares_vendidos 50000)
    (fecha_salida 1904)
    (nombre El_Jardin_de_los_Cerezos)
    (numero_paginas 96)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 5)
    (porcentaje_critica 90)
)

([La_Gaviota] of Libro
    (contieneGenero [MAIN::Drama])
    (estaEscritoEn [MAIN::Ruso])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Ingles] [MAIN::Frances])
    (estaEditadoPor [MAIN::Anagrama])
    (best_seller FALSE)
    (ejemplares_vendidos 60000)
    (fecha_salida 1896)
    (nombre La_Gaviota)
    (numero_paginas 96)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 5)
    (porcentaje_critica 91)
)

([Un_Tranvia_llamado_Deseo] of Libro
    (contieneGenero [MAIN::Drama])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Ruso] [MAIN::Frances] [MAIN::Espanol])
    (estaEditadoPor [MAIN::Anagrama])
    (best_seller TRUE)
    (ejemplares_vendidos 4000000)
    (fecha_salida 1947)
    (nombre Un_Tranvia_llamado_Deseo)
    (numero_paginas 142)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 5)
    (porcentaje_critica 95)
)

([La_Noche_de_la_Iguana] of Libro
    (contieneGenero [MAIN::Drama])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Ruso] [MAIN::Frances] [MAIN::Espanol])
    (estaEditadoPor [MAIN::Alfaguara])
    (best_seller TRUE)
    (ejemplares_vendidos 3000000)
    (fecha_salida 1961)
    (nombre La_Noche_de_la_Iguana)
    (numero_paginas 128)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 5)
    (porcentaje_critica 94)
)

([Mort] of Libro
    (contieneGenero [MAIN::Humor] [MAIN::Fantasia_Urbana])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Akal])
    (best_seller TRUE)
    (ejemplares_vendidos 2000000)
    (fecha_salida 1987)
    (nombre Mort)
    (numero_paginas 272)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 4)
    (porcentaje_critica 92)
)

([Guardias!_Guardias!] of Libro
    (contieneGenero [MAIN::Humor] [MAIN::Fantasia_Urbana])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Akal])
    (best_seller TRUE)
    (ejemplares_vendidos 1800000)
    (fecha_salida 1989)
    (nombre Guardias!_Guardias!)
    (numero_paginas 288)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 4)
    (porcentaje_critica 91)
)

([Guia_del_Autoestopista_Galactico] of Libro
    (contieneGenero [MAIN::Humor] [MAIN::Ciencia_Ficcion_Distopica])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Penguin_Random_House])
    (best_seller TRUE)
    (ejemplares_vendidos 10000000)
    (fecha_salida 1979)
    (nombre Guia_del_Autoestopista_Galactico)
    (numero_paginas 224)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 4)
    (porcentaje_critica 94)
)

([El_Restaurante_del_Fin_del_Mundo] of Libro
    (contieneGenero [MAIN::Humor] [MAIN::Ciencia_Ficcion_Distopica])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Aleman])
    (estaEditadoPor [MAIN::Penguin_Random_House])
    (best_seller TRUE)
    (ejemplares_vendidos 8000000)
    (fecha_salida 1980)
    (nombre El_Restaurante_del_Fin_del_Mundo)
    (numero_paginas 240)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 4)
    (porcentaje_critica 93)
)

([Veinte_Poemas_de_Amor_y_Una_Cancion_Desefrenada] of Libro
    (contieneGenero [MAIN::Poesia] [MAIN::Romance])
    (estaEscritoEn [MAIN::Espanol])
    (estaTraducidoA [MAIN::Ingles] [MAIN::Frances] [MAIN::Ruso])
    (estaEditadoPor [MAIN::Santillana])
    (best_seller TRUE)
    (ejemplares_vendidos 4000000)
    (fecha_salida 1924)
    (nombre Veinte_Poemas_de_Amor_y_Una_Cancion_Desefrenada)
    (numero_paginas 70)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 3)
    (porcentaje_critica 92)
)

([Canto_General] of Libro
    (contieneGenero [MAIN::Poesia])
    (estaEscritoEn [MAIN::Espanol])
    (estaTraducidoA [MAIN::Ingles] [MAIN::Frances] [MAIN::Ruso])
    (estaEditadoPor [MAIN::Cruilla])
    (best_seller FALSE)
    (ejemplares_vendidos 300000)
    (fecha_salida 1950)
    (nombre Canto_General)
    (numero_paginas 500)
    (complejidad_discurso 4)
    (complejidad_linguistica 5)
    (complejidad_tematica 5)
    (porcentaje_critica 91)
)

([Poemas] of Libro
    (contieneGenero [MAIN::Poesia])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Ruso])
    (estaEditadoPor [MAIN::DeBolsillo])
    (best_seller FALSE)
    (ejemplares_vendidos 100000)
    (fecha_salida 1890)
    (nombre Poemas)
    (numero_paginas 150)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 4)
    (porcentaje_critica 90)
)

([Cartas] of Libro
    (contieneGenero [MAIN::Poesia])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances] [MAIN::Ruso])
    (estaEditadoPor [MAIN::Cruilla])
    (best_seller FALSE)
    (ejemplares_vendidos 80000)
    (fecha_salida 1880)
    (nombre Cartas)
    (numero_paginas 200)
    (complejidad_discurso 3)
    (complejidad_linguistica 4)
    (complejidad_tematica 4)
    (porcentaje_critica 89)
)

([El_Gato_con_Sombrero] of Libro
    (contieneGenero [MAIN::Literatura_Infantil])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances])
    (estaEditadoPor [MAIN::Cruilla])
    (best_seller FALSE)
    (ejemplares_vendidos 15000000)
    (fecha_salida 1957)
    (nombre El_Gato_con_Sombrero)
    (numero_paginas 61)
    (complejidad_discurso 1)
    (complejidad_linguistica 2)
    (complejidad_tematica 3)
    (porcentaje_critica 50)
)

([El_Lorax] of Libro
    (contieneGenero [MAIN::Literatura_Infantil])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances])
    (estaEditadoPor [MAIN::Cruilla])
    (best_seller FALSE)
    (ejemplares_vendidos 12000000)
    (fecha_salida 1971)
    (nombre El_Lorax)
    (numero_paginas 72)
    (complejidad_discurso 8)
    (complejidad_linguistica 9)
    (complejidad_tematica 10)
    (porcentaje_critica 60)
)

([Charlie_y_la_Fabrica_de_Chocolate] of Libro
    (contieneGenero [MAIN::Literatura_Infantil])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances])
    (estaEditadoPor [MAIN::Planeta])
    (best_seller TRUE)
    (ejemplares_vendidos 25000000)
    (fecha_salida 1964)
    (nombre Charlie_y_la_Fabrica_de_Chocolate)
    (numero_paginas 180)
    (complejidad_discurso 2)
    (complejidad_linguistica 3)
    (complejidad_tematica 2)
    (porcentaje_critica 65)
)

([Matilda] of Libro
    (contieneGenero [MAIN::Literatura_Infantil])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances])
    (estaEditadoPor [MAIN::Planeta])
    (best_seller TRUE)
    (ejemplares_vendidos 18000000)
    (fecha_salida 1988)
    (nombre Matilda)
    (numero_paginas 240)
    (complejidad_discurso 3)
    (complejidad_linguistica 2)
    (complejidad_tematica 1)
    (porcentaje_critica 68)
)

([Los_Pilares_de_la_Tierra] of Libro
    (contieneGenero [MAIN::Historico_Ficcion])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances])
    (estaEditadoPor [MAIN::Penguin_Random_House])
    (best_seller TRUE)
    (ejemplares_vendidos 30000000)
    (fecha_salida 1989)
    (nombre Los_Pilares_de_la_Tierra)
    (numero_paginas 973)
    (complejidad_discurso 2)
    (complejidad_linguistica 3)
    (complejidad_tematica 2)
    (porcentaje_critica 68)
)

([Un_Mundo_Sin_Fin] of Libro
    (contieneGenero [MAIN::Historico_Ficcion])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances])
    (estaEditadoPor [MAIN::Penguin_Random_House])
    (best_seller TRUE)
    (ejemplares_vendidos 15000000)
    (fecha_salida 2007)
    (nombre Un_Mundo_Sin_Fin)
    (numero_paginas 1231)
    (complejidad_discurso 2)
    (complejidad_linguistica 3)
    (complejidad_tematica 2)
    (porcentaje_critica 65)
)

([Wolf_Hall] of Libro
    (contieneGenero [MAIN::Historico_Ficcion])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances])
    (estaEditadoPor [MAIN::Planeta])
    (best_seller TRUE)
    (ejemplares_vendidos 1000000)
    (fecha_salida 2009)
    (nombre Wolf_Hall)
    (numero_paginas 650)
    (complejidad_discurso 2)
    (complejidad_linguistica 3)
    (complejidad_tematica 2)
    (porcentaje_critica 69)
)

([Bring_Up_the_Bodies] of Libro
    (contieneGenero [MAIN::Historico_Ficcion])
    (estaEscritoEn [MAIN::Ingles])
    (estaTraducidoA [MAIN::Espanol] [MAIN::Frances])
    (estaEditadoPor [MAIN::Planeta])
    (best_seller TRUE)
    (ejemplares_vendidos 900000)
    (fecha_salida 2012)
    (nombre Bring_Up_the_Bodies)
    (numero_paginas 432)
    (complejidad_discurso 3)
    (complejidad_linguistica 2)
    (complejidad_tematica 1)
    (porcentaje_critica 66)
)


)
