;;#############################################################################################
;;################################### MAIN ####################################################
;;#############################################################################################

;; Este es el modulo principal
(defmodule MAIN (export ?ALL))

;; Modulo de entrada de datos del lector
(defmodule INPUT
	(import MAIN ?ALL)
	(export ?ALL)
)

;; Modulo de abstraccion de datos del lector
(defmodule ABSTRACTION
	(import INPUT ?ALL)
	(export ?ALL)
)

;; Modulo de inferencia de libros potenciales
(defmodule ASSOCIATION
	(import ABSTRACTION ?ALL)
	(export ?ALL)
)

;; Modulo de descarte de libros no aptos
(defmodule SYNTHESIS
	(import MAIN ?ALL)
	(import ASSOCIATION ?ALL)
	(export ?ALL)
)

;; Modulo de reconstruccion resolutiva de libros recomendados
(defmodule RECONSTRUCTION
	(import MAIN ?ALL)
	(import SYNTHESIS ?ALL)
	(export ?ALL)
)

;; Modulo de printado de resultados
(defmodule OUTPUT
	(import MAIN ?ALL)
	(import RECONSTRUCTION ?ALL)
	(export ?ALL)
)

(defrule MAIN::initialRule "regla inicial"
	(declare (salience 10))
	=>
	(printout t crlf)
	(printout t "##############################################################" crlf)
	(printout t "############# Sistema de Recomendacion de Libros #############" crlf)
	(printout t "##############################################################" crlf)
	(printout t crlf)
)

(defrule MAIN::switchToINPUT
	(declare (salience -50))
	=>
	(printout t "Pasando a INPUT" crlf)
	(focus INPUT)
)

;;#############################################################################################
;;################################ INPUT ######################################################
;;#############################################################################################

;; Este modulo se encarga de hacertodas las preguntas del usuario (lector) para obtener toda
;; la informacion sobre sus preferencias, caracteristicas, etc. Para hacer una buena recomendacion

;;############################### Classes #####################################################

;;############################### Funciones ###################################################

;; Funcion para hacer pregunta categorica dentro de un rango de valores permitidos
(deffunction INPUT::pregunta-choice (?pregunta $?valores_permitidos)
   (bind ?var (lowcase (implode$ $?valores_permitidos)))
   (format t "%s? [%s] " ?pregunta (implode$ ?valores_permitidos))
   (bind ?respuesta (readline))
   (bind ?respuesta (sym-cat ?respuesta))
   (while (not (member$ ?respuesta $?valores_permitidos)) do
      (format t "%s? [%s] " ?pregunta (implode$ ?valores_permitidos))
      (bind ?respuesta (readline))
   	  (bind ?respuesta (sym-cat ?respuesta))
   )
   ?respuesta
)

;; Funcion para hacer una pregunta numerica dentro de un rango de valores permitidos
(deffunction INPUT::pregunta-numerica (?pregunta ?rangini ?rangfi)
	(format t "%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
	(bind ?respuesta (read))
	(while (not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi))) do
		(format t "%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
		(bind ?respuesta (read))
	)
	?respuesta
)

;; Funcion para hacer una pregunta binaria
(deffunction INPUT::pregunta-binaria (?pregunta)
    (format t "%s? [Si/No] " ?pregunta)
    (bind ?respuesta (readline))
    (while (not (or (eq (lowcase ?respuesta) "si")
                    (eq (lowcase ?respuesta) "no"))) do
        (format t "%s? [Si/No] " ?pregunta)
        (bind ?respuesta (readline))
    )
    (if (eq (lowcase ?respuesta) "si")
        then TRUE
        else FALSE
    )
)

;; Funcion para hacer una pregunta sobre una lista de respuestas posibles
(deffunction INPUT::pregunta-lista (?pregunta)
	(format t "%s? " ?pregunta)
	(bind ?resposta (readline))
	(bind ?res (explode$ ?resposta))
	?res
)

;; Funcion para hacer una pregunta general
(deffunction INPUT::pregunta-general (?pregunta)
	(format t "%s? " ?pregunta)
	(bind ?respuesta (read))
	?respuesta
)

;; Funcion para convertir una lista de identificadores string en sus instancias respectivas
(deffunction INPUT::string-a-idioma ($?lista)
    (printout t "Original List" ?lista crlf)
	(bind ?listaRes (create$))
	(loop-for-count (?index 1 (length ?lista)) do
		(bind ?current_item (nth$ ?index ?lista))
		(printout t "Current Item " ?current_item crlf)
		(bind ?aux (find-instance ((?inst Idioma))(eq (lowcase ?inst:nombre) (lowcase ?current_item))))
		(bind ?listaRes (insert$ ?listaRes (+ (length$ $?listaRes) 1) ?aux))
		(printout t "Instance found for: " ?listaRes crlf)
   	)
   	?listaRes
)

(deffunction INPUT::string-a-autor ($?lista)
    (printout t "Original List" ?lista crlf)
	(bind ?listaRes (create$))
	(loop-for-count (?index 1 (length ?lista)) do
		(bind ?current_item (nth$ ?index ?lista))
		(printout t "Current Item " ?current_item crlf)
		(bind ?aux (find-instance ((?inst Autor))(eq (lowcase ?inst:nombre) (lowcase ?current_item))))
		(bind ?listaRes (insert$ ?listaRes (+ (length$ $?listaRes) 1) ?aux))
		(printout t "Instance found for: " ?listaRes crlf)
   	)
   	?listaRes
)

(deffunction INPUT::string-a-genero ($?lista)
    (printout t "Original List" ?lista crlf)
	(bind ?listaRes (create$))
	(loop-for-count (?index 1 (length ?lista)) do
		(bind ?current_item (nth$ ?index ?lista))
		(printout t "Current Item " ?current_item crlf)
		(bind ?aux (find-instance ((?inst Genero))(eq (lowcase ?inst:nombre) (lowcase ?current_item))))
		(bind ?listaRes (insert$ ?listaRes (+ (length$ $?listaRes) 1) ?aux))
		(printout t "Instance found for: " ?listaRes crlf)
   	)
   	?listaRes
)

(deffunction INPUT::string-a-editorial ($?lista)
    (printout t "Original List" ?lista crlf)
	(bind ?listaRes (create$))
	(loop-for-count (?index 1 (length ?lista)) do
		(bind ?current_item (nth$ ?index ?lista))
		(printout t "Current Item " ?current_item crlf)
		(bind ?aux (find-instance ((?inst Editorial))(eq (lowcase ?inst:nombre) (lowcase ?current_item))))
		(bind ?listaRes (insert$ ?listaRes (+ (length$ $?listaRes) 1) ?aux))
		(printout t "Instance found for: " ?listaRes crlf)
   	)
   	?listaRes
)

(deffunction INPUT::string-a-libro ($?lista)
    (printout t "Original List" ?lista crlf)
	(bind ?listaRes (create$))
	(loop-for-count (?index 1 (length ?lista)) do
		(bind ?current_item (nth$ ?index ?lista))
		(printout t "Current Item " ?current_item crlf)
		(bind ?aux (find-instance ((?inst Libro))(eq (lowcase ?inst:nombre) (lowcase ?current_item))))
		(bind ?listaRes (insert$ ?listaRes (+ (length$ $?listaRes) 1) ?aux))
		(printout t "Instance found for: " ?listaRes crlf)
   	)
   	?listaRes
)


(deffunction INPUT::instanciar-lector ()
	(bind ?nombre (pregunta-general "Cual es tu nombre" ))
	(bind ?edad (pregunta-numerica "Cual es tu edad" 1 100))
	(bind $?nacionalidades (pregunta-lista "Cual/es son tus nacionalidades"))
	(bind $?libros (pregunta-lista "Que libros has leido y son tus favoritos"))
	(bind $?idiomas (pregunta-lista "Que idiomas hablas/entiendes bien"))
	(bind $?autores (pregunta-lista "Tienes algun/os autores favoritos"))
	(bind $?generos (pregunta-lista "Que generos te suelen gustar"))
	(bind $?editoriales (pregunta-lista "Que editoriales te suelen gustar"))
	(bind ?interes_extranjero (pregunta-binaria "Tienes interes en obras y autores extranjeros"))
	(bind ?lugar_lectura (pregunta-choice "Donde sueles leer" (create$ Casa Cama Exteriores Transporte_Publico)))
	(bind ?momento_de_lectura (pregunta-choice "Cuando sueles leer" (create$ Manana Tarde Noche Fin_de_semana)))
	(bind ?susceptible_moda (pregunta-numerica "Del 1 al 10 (1 poco - 10 mucho) Cuan de susceptible a la moda te consideras" 1 10))
	(bind ?frecuencia_lectura (pregunta-numerica "Como de frecuentemente lees en dias a la semana" 0 7))
	(bind ?tiempo_disponible (pregunta-numerica "Cuantas horas al dia sueles leer" 0 8))
	(bind ?competencia_linguistica (pregunta-numerica "Del 1 al 10 (1 peor - 10 mejor) cual considerarias que es tu nivel de competencia linguistica (capacidad de entender palabras complicadas, recursos literarios como alegorias, metaforas, personificaciones, etc.)" 1 10))
	(bind ?competencia_tematica (pregunta-numerica "Del 1 al 10 (1 peor - 10 mejor) cual considerarias que es tu nivel de competencia tematica (capacidad de entender mensajes literarios complejos (reflexiones filosoficas, retorica psicologica, referencias historicas, etc.) )" 1 10))
	(bind ?competencia_comprension (pregunta-numerica "Del 1 al 10 (1 peor - 10 mejor) cual considerarias que es tu nivel de competencia de discurso (capacidad de leer textos con parrafos complejos, tramas con saltos temporales, ambiguedades, etc.)" 1 10))
	(bind ?opinion_traduccion (pregunta-binaria "Te importa leer libros traducidos o solo quieres originales (si = traducidos - no = solo originales)"))
	(bind ?opinion_contemporaneo (pregunta-choice "Prefieres leer solo libros contemporaneos, solo verdaderos clasicos o cualquier tipo de libro" (create$ Contemporaneos Clasicos Todos)))
	(bind ?opinion_critica (pregunta-numerica "Del 1 al 10 (1 poco - 10 mucho) como de relevante es para ti la opinion critica de un libro" 1 10))

	(bind ?librosProc (string-a-libro ?libros))
	(bind ?idiomasProc (string-a-idioma ?idiomas))
	(bind ?autoresProc (string-a-autor ?autores))
	(bind ?generosProc (string-a-genero ?generos))
	(bind ?editorialesProc (string-a-editorial ?editoriales))
	
	(make-instance Usuario of Lector
		(edad ?edad)
		(nacionalidad ?nacionalidades)
		(nombre ?nombre)
		(haLeido ?librosProc) ; Assuming Book1 and Book2 are instances of books
		(hablaIdioma ?idiomasProc)
		(prefiereAutor ?autoresProc) ; Assuming Author1 and Author2 are instances of authors
		(prefiereGenero ?generosProc)
		(prefiereEditorial ?editorialesProc)
		(frecuencia_lectura ?frecuencia_lectura)
		(interes_extranjero ?interes_extranjero) ; Assuming SI is a symbol indicating interest
		(lugar_lectura ?lugar_lectura)
		(momento_de_lectura ?momento_de_lectura)
		(susceptible_moda ?susceptible_moda) ; Assuming NO is a symbol indicating not susceptible
		(tiempo_disponible ?tiempo_disponible)
		(competencia_linguistica ?competencia_linguistica)
		(competencia_tematica ?competencia_tematica)
		(competencia_comprension ?competencia_comprension)
		(opinion_traduccion ?opinion_traduccion)
		(opinion_contemporaneo ?opinion_contemporaneo)
		(opinion_critica ?opinion_critica)
	)
)

;;################################ Reglas #####################################################

(defrule INPUT::instanciarLector
	(declare (salience 10))
	(not (Lector))
	=> 
	(printout t "Ahora te haremos unas preguntas para poder recomendar los libros que mejor se adapten a tus preferencias." crlf crlf)
	(instanciar-lector)
)

(defrule INPUT::switchToABSTRACTION
	(declare (salience -50))
	=> 
	(printout t "Pasando a ABSTRACTION" crlf)
	(focus ABSTRACTION)
)

;;#############################################################################################
;;################################ ABSTRACTION ################################################
;;#############################################################################################

;;############################### Classes #####################################################

(defclass ABSTRACTION::LectorAbs
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot hablaIdioma
        (type INSTANCE)
        (create-accessor read-write))
	(multislot prefiereGenero
        (type INSTANCE)
        (create-accessor read-write))
	(slot tiempoDisponible
        (type SYMBOL)
		(allowed-values POCO MEDIO MUCHO INFINITO)
        (create-accessor read-write))
	(slot implicacionLector
		(type SYMBOL)
		(allowed-values POCA MEDIANA GRANDE ENORME)
        (create-accessor read-write))
	(slot competenciaForma
		(type INTEGER)
		(create-accessor read-write))
	(slot competenciaFondo
		(type INTEGER)
		(create-accessor read-write))
	(slot opinionContemporaneo
		(type SYMBOL)
		(allowed-values CONTEMPORANEO CLASICO TODOS)
		(create-accessor read-write))
	(slot nivelCritica
		(type SYMBOL)
		(allowed-values NULO POCO MUCHO)
		(create-accessor read-write))
	(slot susceptibleModa
		(type SYMBOL)
		(allowed-values NULO POCO MUCHO)
		(create-accessor read-write))
)

;;############################### Funciones ###################################################

;;############################### Templates ###################################################

(deftemplate ABSTRACTION::tiempoAbstraido)
(deftemplate ABSTRACTION::implicacionAbstraida)
(deftemplate ABSTRACTION::competenciaFormaAbstraida)
(deftemplate ABSTRACTION::competenciaFondoAbstraida)
(deftemplate ABSTRACTION::modificadorEducacionAplicado)
(deftemplate ABSTRACTION::contempAbstraido)
(deftemplate ABSTRACTION::criticaAbstraida)
(deftemplate ABSTRACTION::modaAbstraida)

;;################################ Reglas #####################################################

(defrule ABSTRACTION::instanciarLectorAbs
   (declare (salience 50))
   (not (LectorAbs))
   =>
   (printout t "Instanciando LectorAbs" crlf)
   (make-instance UsuarioAbs of LectorAbs)
)

(defrule ABSTRACTION::abstraerIdioma
	?usuario <- (object (is-a Lector))
	?usuarioAbs <- (object (is-a LectorAbs))
	=>
   	(printout t "Abstrayendo Idiomas" crlf)
	(bind ?idiomas (send ?usuario get-hablaIdioma))
	(send ?usuarioAbs put-hablaIdioma ?idiomas)
)

(defrule ABSTRACTION::abstraerGeneroSimple
	(declare (salience 40))
    ?usuario <- (object (is-a Lector))
    ?usuarioAbs <- (object (is-a LectorAbs))
    =>
       (printout t "Abstrayendo Generos por via directa" crlf)
    (bind ?genero (send ?usuario get-prefiereGenero))
    (send ?usuarioAbs put-prefiereGenero ?genero)
)

(defrule ABSTRACTION::abstraerGeneroFamoso
    ?usuario <- (object (is-a Lector))
    ?usuarioAbs <- (object (is-a LectorAbs) (prefiereGenero $?generos))
    ?genero <- (object (is-a Genero) (nombre ?nombre-genero))
    =>
       (printout t "Abstrayendo Generos por via estar de moda" crlf)
    (bind ?generof (send ?genero get-esta_de_moda))
    (bind ?usuario_moda (send ?usuario get-susceptible_moda))
	(bind ?inst-genero (find-instance ((?inst Genero)) (eq ?inst:nombre ?nombre-genero))) ;;extraer la instancia de un nombre
    (if (and (eq ?generof TRUE)(>= ?usuario_moda 7)(not (member$ ?inst-genero $?generos)))
        then 
		(bind $?actual-prefiere (send ?usuarioAbs get-prefiereGenero)) ;;obtener una lista
		(bind $?list-genero (insert$ $?actual-prefiere (+ (length$ $?actual-prefiere) 1) ?inst-genero)) ;;insertar en la cola
		(send ?usuarioAbs put-prefiereGenero $?list-genero)) ;;settear una lista
)

(defrule ABSTRACTION::abstraerGeneroAutorFav
    ?usuario <- (object (is-a Lector) (prefiereAutor $?autorFav))
    ?usuarioAbs <- (object (is-a LectorAbs) (prefiereGenero $?generos))
    ?genero <- (object (is-a Genero) (nombre ?nombre-genero) (tieneAutorDestacado $?autoresDestacados))
    =>
    (printout t "Abstrayendo Generos por autores favoritos" crlf)
	(bind ?inst-genero (find-instance ((?inst Genero)) (eq ?inst:nombre ?nombre-genero))) ;;extraer la instancia de un nombre
	;;(printout t "?inst-genero " ?inst-genero " $?generos " $?generos crlf)

	(if (not (member$ ?inst-genero $?generos))
		then
		(loop-for-count (?i 1 (length$ $?autorFav)) do
			(bind ?curr-aut (nth$ ?i $?autorFav))
			(if (member$ ?curr-aut $?autoresDestacados)
				then 
				(bind $?actual-prefiere (send ?usuarioAbs get-prefiereGenero)) ;;obtener una lista
				(bind $?list-genero (insert$ $?actual-prefiere (+ (length$ $?actual-prefiere) 1) ?inst-genero)) ;;insertar en la cola
				(send ?usuarioAbs put-prefiereGenero $?list-genero) ;;settear una lista
				(break)
			)
		)
	)
)

(defrule ABSTRACTION::abstraerGeneroLeidos
    ?usuario <- (object (is-a Lector) (haLeido $?librosLeidos))
    ?usuarioAbs <- (object (is-a LectorAbs) (prefiereGenero $?generos))
    ?genero <- (object (is-a Genero) (nombre ?nombre-genero))
    =>
    (printout t "Abstrayendo Generos por libros leidos" crlf)
	(bind ?inst-genero (find-instance ((?inst Genero)) (eq ?inst:nombre ?nombre-genero))) ;;extraer la instancia de un nombre
	(if (not (member$ ?inst-genero $?generos))
		then
		(bind ?count 0)
		(loop-for-count    (?i 1 (length$ $?librosLeidos)) do
			(bind ?curr-lib (nth$ ?i $?librosLeidos))
			(bind $?gen_list (send ?curr-lib get-contieneGenero))
			(if (member$ ?inst-genero $?gen_list)
				then 
				(bind ?count (+ ?count 1))
			)
		)
		(if (>= ?count 3) 
			then
			(bind $?actual-prefiere (send ?usuarioAbs get-prefiereGenero)) ;;obtener una lista
			(bind $?list-genero (insert$ $?actual-prefiere (+ (length$ $?actual-prefiere) 1) ?inst-genero)) ;;insertar en la cola
			(send ?usuarioAbs put-prefiereGenero $?list-genero) ;;settear una lista
		)
	)
)

(defrule ABSTRACTION::abstraerTiempoLectorPOCO
	(declare (salience 23))
	(not (tiempoAbstraido))
	?usuario <- (object (is-a Lector) (frecuencia_lectura ?frecuencia) (tiempo_disponible ?tiempo))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (<= (* ?frecuencia ?tiempo) 4))
	=>
	(send ?usuarioAbs put-tiempoDisponible POCO)
	(assert (tiempoAbstraido))
)

(defrule ABSTRACTION::abstraerTiempoLectorMEDIO
	(declare (salience 22))
	(not (tiempoAbstraido))
	?usuario <- (object (is-a Lector) (frecuencia_lectura ?frecuencia) (tiempo_disponible ?tiempo))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (<= (* ?frecuencia ?tiempo) 8))
	=>
	(send ?usuarioAbs put-tiempoDisponible MEDIO)
	(assert (tiempoAbstraido))
)

(defrule ABSTRACTION::abstraerTiempoLectorMUCHO
	(declare (salience 21))
	(not (tiempoAbstraido))
	?usuario <- (object (is-a Lector) (frecuencia_lectura ?frecuencia) (tiempo_disponible ?tiempo))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (<= (* ?frecuencia ?tiempo) 16))
	=>
	(send ?usuarioAbs put-tiempoDisponible MUCHO)
	(assert (tiempoAbstraido))
)

(defrule ABSTRACTION::abstraerTiempoLectorINFINITO
	(declare (salience 20))
	(not (tiempoAbstraido))
	?usuario <- (object (is-a Lector) (frecuencia_lectura ?frecuencia) (tiempo_disponible ?tiempo))
	?usuarioAbs <- (object (is-a LectorAbs))
	=>
	(send ?usuarioAbs put-tiempoDisponible INFINITO)
	(assert (tiempoAbstraido))
)

(defrule ABSTRACTION::abstraerImplicacionLectorPOCA
	(not (implicacionAbstraida))
	?usuario <- (object (is-a Lector) (momento_de_lectura ?momento) (lugar_lectura ?lugar))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (or (eq ?momento Manana) (eq ?lugar Transporte_Publico)))
	=>
    (printout t "Abstrayendo implicacion lector POCA" crlf) 
	(send ?usuarioAbs put-implicacionLector POCA)
	(assert (implicacionAbstraida))
)

(defrule ABSTRACTION::abstraerImplicacionLectorMEDIANA
	(not (implicacionAbstraida))
	?usuario <- (object (is-a Lector) (momento_de_lectura ?momento) (lugar_lectura ?lugar))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (or (and (eq ?momento Tarde) (eq ?lugar Exteriores)) (and (eq ?momento Noche) (eq ?lugar Exteriores)) (and (eq ?momento Tarde) (eq ?lugar Cama))))
	=>
    (printout t "Abstrayendo implicacion lector MEDIANA" crlf) 
	(send ?usuarioAbs put-implicacionLector MEDIANA)
	(assert (implicacionAbstraida))
)

(defrule ABSTRACTION::abstraerImplicacionLectorGRANDE
	(not (implicacionAbstraida))
	?usuario <- (object (is-a Lector) (momento_de_lectura ?momento) (lugar_lectura ?lugar))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (or (and (eq ?momento Tarde) (eq ?lugar Casa)) (and (eq ?momento Noche) (eq ?lugar Casa)) (and (eq ?momento Noche) (eq ?lugar Cama)) (and (eq ?momento Fin_de_semana) (eq ?lugar Cama))))
	=>
    (printout t "Abstrayendo implicacion lector GRANDE" crlf) 
	(send ?usuarioAbs put-implicacionLector GRANDE)
	(assert (implicacionAbstraida))
)

(defrule ABSTRACTION::abstraerImplicacionLectorENORME
	(not (implicacionAbstraida))
	?usuario <- (object (is-a Lector) (momento_de_lectura ?momento) (lugar_lectura ?lugar))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (or (and (eq ?momento Fin_de_semana) (eq ?lugar Casa)) (and (eq ?momento Fin_de_semana) (eq ?lugar Exteriores))))
	=>
    (printout t "Abstrayendo implicacion lector ENORME" crlf) 
	(send ?usuarioAbs put-implicacionLector ENORME)
	(assert (implicacionAbstraida))
)

(defrule ABSTRACTION::abstraerCompetenciaForma
	(not (competenciaFormaAbstraida))
	?usuario <- (object (is-a Lector) (competencia_linguistica ?linguistica) (competencia_comprension ?comprension))
	?usuarioAbs <- (object (is-a LectorAbs))
	=>
	(bind ?sum (+ ?linguistica ?comprension))
	(bind ?avg (div ?sum 2))
	(send ?usuarioAbs put-competenciaForma ?avg)
	(assert (competenciaFormaAbstraida))
)

(defrule ABSTRACTION::abstraerCompetenciaFondo
	(not (competenciaFondoAbstraida))
	?usuario <- (object (is-a Lector) (competencia_tematica ?tematica))
	?usuarioAbs <- (object (is-a LectorAbs))
	=>
	(send ?usuarioAbs put-competenciaFondo ?tematica)
	(assert (competenciaFondoAbstraida))
)

(defrule ABSTRACTION::modificadorCompetenciasEducacionMUY_BAJO
	(not (modificadorEducacionAplicado))
	(and (competenciaFormaAbstraida) (competenciaFondoAbstraida))
	?usuario <- (object (is-a Lector) (grado_educacion ?educacion))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?forma) (competenciaFondo ?fondo))
    (test (or (eq ?educacion Nula) (eq ?educacion Infantil)))
	=>
	(bind ?resForma (max 0 (- ?forma 2)))
	(bind ?resFondo (max 0 (- ?fondo 2)))
	(send ?usuarioAbs put-competenciaForma ?resForma)
	(send ?usuarioAbs put-competenciaFondo ?resFondo)
	(assert (modificadorEducacionAplicado))
)

(defrule ABSTRACTION::modificadorCompetenciasEducacionBAJO
	(not (modificadorEducacionAplicado))
	(and (competenciaFormaAbstraida) (competenciaFondoAbstraida))
	?usuario <- (object (is-a Lector) (grado_educacion ?educacion))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?forma) (competenciaFondo ?fondo))
	(test (eq ?educacion Primaria))
	=>
	(bind ?resForma (max 0 (- ?forma 1)))
	(bind ?resFondo (max 0 (- ?fondo 1)))
	(send ?usuarioAbs put-competenciaForma ?resForma)
	(send ?usuarioAbs put-competenciaFondo ?resFondo)
	(assert (modificadorEducacionAplicado))
)

(defrule ABSTRACTION::modificadorCompetenciasEducacionALTO
	(not (modificadorEducacionAplicado))
	(and (competenciaFormaAbstraida) (competenciaFondoAbstraida))
	?usuario <- (object (is-a Lector) (grado_educacion ?educacion))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?forma) (competenciaFondo ?fondo))
    (test (or (eq ?educacion Bachillerato) (eq ?educacion Formacion_Profesional)))
	=>
	(bind ?resForma (min 10 (+ ?forma 1)))
	(bind ?resFondo (min 10 (+ ?fondo 1)))
	(send ?usuarioAbs put-competenciaForma ?resForma)
	(send ?usuarioAbs put-competenciaFondo ?resFondo)
	(assert (modificadorEducacionAplicado))
)

(defrule ABSTRACTION::modificadorCompetenciasEducacionMUY_ALTO
	(not (modificadorEducacionAplicado))
	(and (competenciaFormaAbstraida) (competenciaFondoAbstraida))
	?usuario <- (object (is-a Lector) (grado_educacion ?educacion))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?forma) (competenciaFondo ?fondo))
    (test (or (eq ?educacion Universitaria) (eq ?educacion Postgrado)))
	=>
	(bind ?resForma (min 10 (+ ?forma 2)))
	(bind ?resFondo (min 10 (+ ?fondo 2)))
	(send ?usuarioAbs put-competenciaForma ?resForma)
	(send ?usuarioAbs put-competenciaFondo ?resFondo)
	(assert (modificadorEducacionAplicado))
)

(defrule ABSTRACTION::abstraerOpinionContemporaneoCONTEMPORANEO
	(not (contempAbstraido))
	?usuario <- (object (is-a Lector) (opinion_contemporaneo ?opinionContemp))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (eq ?opinionContemp Contemporaneos))
	=>
	(send ?usuarioAbs put-opinionContemporaneo CONTEMPORANEO)
	(assert (contempAbstraido))
)

(defrule ABSTRACTION::abstraerOpinionContemporaneoCLASICO
	(not (contempAbstraido))
	?usuario <- (object (is-a Lector) (opinion_contemporaneo ?opinionContemp))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (eq ?opinionContemp Clasicos))
	=>
	(send ?usuarioAbs put-opinionContemporaneo CLASICO)
	(assert (contempAbstraido))
)

(defrule ABSTRACTION::abstraerOpinionContemporaneoTODOS
	(not (contempAbstraido))
	?usuario <- (object (is-a Lector) (opinion_contemporaneo ?opinionContemp))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (eq ?opinionContemp Todos))
	=>
	(send ?usuarioAbs put-opinionContemporaneo TODOS)
	(assert (contempAbstraido))
)

(defrule ABSTRACTION::abstraerOpinionCriticaNULO
	(declare (salience 22))
	(not (criticaAbstraida))
	?usuario <- (object (is-a Lector) (opinion_critica ?critica))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (< ?critica 5))
	=>
	(send ?usuarioAbs put-nivelCritica NULO)
	(assert (criticaAbstraida))
)

(defrule ABSTRACTION::abstraerOpinionCriticaPOCO
	(declare (salience 21))
	(not (criticaAbstraida))
	?usuario <- (object (is-a Lector) (opinion_critica ?critica))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (< ?critica 8))
	=>
	(send ?usuarioAbs put-nivelCritica POCO)
	(assert (criticaAbstraida))
)

(defrule ABSTRACTION::abstraerOpinionCriticaMUCHO
	(declare (salience 20))
	(not (criticaAbstraida))
	?usuario <- (object (is-a Lector) (opinion_critica ?critica))
	?usuarioAbs <- (object (is-a LectorAbs))
	=>
	(send ?usuarioAbs put-nivelCritica MUCHO)
	(assert (criticaAbstraida))
)

(defrule ABSTRACTION::abstraerSusceptibleModaNULO
	(declare (salience 22))
	(not (modaAbstraida))
	?usuario <- (object (is-a Lector) (susceptible_moda ?moda))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (< ?moda 5))
	=>
	(send ?usuarioAbs put-susceptibleModa NULO)
	(assert (modaAbstraida))
)

(defrule ABSTRACTION::abstraerSusceptibleModaPOCO
	(declare (salience 21))
	(not (modaAbstraida))
	?usuario <- (object (is-a Lector) (susceptible_moda ?moda))
	?usuarioAbs <- (object (is-a LectorAbs))
	(test (< ?moda 8))
	=>
	(send ?usuarioAbs put-susceptibleModa POCO)
	(assert (modaAbstraida))
)

(defrule ABSTRACTION::abstraerSusceptibleModaMUCHO
	(declare (salience 20))
	(not (modaAbstraida))
	?usuario <- (object (is-a Lector) (susceptible_moda ?moda))
	?usuarioAbs <- (object (is-a LectorAbs))
	=>
	(send ?usuarioAbs put-susceptibleModa MUCHO)
	(assert (modaAbstraida))
)
(defrule ABSTRACTION::switchToASSOCIATION
	(declare (salience -50))
	=>
	(printout t "Pasando a ASSOCIATION" crlf)
	(focus ASSOCIATION)
)

;;#############################################################################################
;;################################ ASSOCIATION ################################################
;;#############################################################################################

;;############################### Classes #####################################################

(defclass ASSOCIATION::LibroAbs
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot estaEscritoEn
        (type INSTANCE)
        (create-accessor read-write))
	(multislot contieneGenero
		(type INSTANCE)
		(create-accessor read-write))
	(multislot longitud
		(type SYMBOL)
		(allowed-values CORTO MEDIANO LARGO ENORME)
		(create-accessor read-write))
	(slot complejidadForma
		(type SYMBOL)
		(allowed-values MUY_SIMPLE SIMPLE COMPLEJO MUY_COMPLEJO)
		(create-accessor read-write))
	(slot complejidadFondo
		(type SYMBOL)
		(allowed-values MUY_SIMPLE SIMPLE COMPLEJO MUY_COMPLEJO)
		(create-accessor read-write))
	(slot isContemporaneo
		(type SYMBOL)
		(allowed-values FALSE TRUE)
		(create-accessor read-write))
	(slot isClasico
		(type SYMBOL)
		(allowed-values FALSE TRUE)
		(create-accessor read-write))
	(slot nivelCritica
		(type SYMBOL)
		(allowed-values NULO REGULAR BUENO)
		(create-accessor read-write))
	(slot nivelModa
		(type SYMBOL)
		(allowed-values NULO CONOCIDO BEST_SELLER)
		(create-accessor read-write))
)

;;############################### Funciones ###################################################

;;############################### Templates ###################################################

(deftemplate ASSOCIATION::competenciaFormaAsociada)
(deftemplate ASSOCIATION::competenciaFondoAsociada)
(deftemplate ASSOCIATION::contempAsociado)
(deftemplate ASSOCIATION::criticaAsociada)
(deftemplate ASSOCIATION::modaAsociada)

;;################################ Reglas #####################################################

(defrule ASSOCIATION::instanciarLibroAbs
   (declare (salience 50))
   (not (LibroAbs))
   =>
   (printout t "Instanciando LibroAbs" crlf)
   (make-instance LibroAbs of LibroAbs)
)

(defrule ASSOCIATION::asociarIdioma
	?usuarioAbs <- (object (is-a LectorAbs))
	?libroAbs <- (object (is-a LibroAbs))
	=>
   	(printout t "Asociando Idiomas" crlf)
	(bind $?idiomas (send ?usuarioAbs get-hablaIdioma))
	(send ?libroAbs put-estaEscritoEn ?idiomas)
)

(defrule ASSOCIATION::asociarGenero
	?usuarioAbs <- (object (is-a LectorAbs))
	?libroAbs <- (object (is-a LibroAbs))
	=>
	(printout t "Asociando generos" crlf)
	(bind $?generos (send ?usuarioAbs get-prefiereGenero))
	(send ?libroAbs put-contieneGenero ?generos)
)

(defrule ASSOCIATION::asociarLongitudCORTO
	?usuarioAbs <- (object (is-a LectorAbs) (tiempoDisponible ?tiempo) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (and (eq ?tiempo POCO) (eq ?implicacion MEDIANA)) (and (eq ?tiempo MEDIO) (eq ?implicacion POCA))))
	=>
	(bind $?longitudesAvailable (create$ CORTO))
	(send ?libroAbs put-longitud $?longitudesAvailable)
)

(defrule ASSOCIATION::asociarLongitudMEDIANO
	?usuarioAbs <- (object (is-a LectorAbs) (tiempoDisponible ?tiempo) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (and (eq ?tiempo MEDIO) (eq ?implicacion MEDIANA)) (and (eq ?tiempo POCO) (eq ?implicacion GRANDE)) (and (eq ?tiempo MUCHO) (eq ?implicacion POCA))))
	=>
	(bind $?longitudesAvailable (create$ CORTO MEDIANO))
	(send ?libroAbs put-longitud $?longitudesAvailable)
)

(defrule ASSOCIATION::asociarLongitudLARGO
	?usuarioAbs <- (object (is-a LectorAbs) (tiempoDisponible ?tiempo) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (and (eq ?tiempo MUCHO) (eq ?implicacion MEDIANA)) (and (eq ?tiempo POCO) (eq ?implicacion ENORME)) (and (eq ?tiempo MEDIO) (eq ?implicacion GRANDE)) (and (eq ?tiempo INFINITO) (eq ?implicacion POCA))))
	=>
	(bind $?longitudesAvailable (create$ CORTO MEDIANO LARGO))
	(send ?libroAbs put-longitud $?longitudesAvailable)
)

(defrule ASSOCIATION::asociarLongitudENORME
	?usuarioAbs <- (object (is-a LectorAbs) (tiempoDisponible ?tiempo) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (and (eq ?tiempo INFINITO) (eq ?implicacion MEDIANA)) (and (eq ?tiempo INFINITO) (eq ?implicacion GRANDE)) (and (eq ?tiempo INFINITO) (eq ?implicacion ENORME)) (and (eq ?tiempo MEDIO) (eq ?implicacion ENORME)) (and (eq ?tiempo MUCHO) (eq ?implicacion GRANDE))))
	=>
	(bind $?longitudesAvailable (create$ CORTO MEDIANO LARGO ENORME))
	(send ?libroAbs put-longitud $?longitudesAvailable)
)

(defrule ASSOCIATION::asociarComplejidadFormaMUY_SIMPLE
	(declare (salience 23))
	(not (competenciaFormaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?forma) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (<= ?forma 2) (and (<= ?forma 5) (eq ?implicacion POCA))))
	=>
	(send ?libroAbs put-complejidadForma MUY_SIMPLE)
	(assert (competenciaFormaAsociada))
)

(defrule ASSOCIATION::asociarComplejidadFormaSIMPLE
	(declare (salience 22))
	(not (competenciaFormaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?forma) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (<= ?forma 5) (and (<= ?forma 8) (eq ?implicacion POCA)) (and (<= ?forma 2) (eq ?implicacion ENORME))))
	=>
	(send ?libroAbs put-complejidadForma SIMPLE)
	(assert (competenciaFormaAsociada))
)

(defrule ASSOCIATION::asociarComplejidadFormaCOMPLEJO
	(declare (salience 21))
	(not (competenciaFormaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?forma) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (<= ?forma 8) (and (<= ?forma 10) (eq ?implicacion POCA)) (and (<= ?forma 5) (eq ?implicacion ENORME))))
	=>
	(send ?libroAbs put-complejidadForma COMPLEJO)
	(assert (competenciaFormaAsociada))
)

(defrule ASSOCIATION::asociarComplejidadFormaMUY_COMPLEJO
	(declare (salience 20))
	(not (competenciaFormaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?forma) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (<= ?forma 10) (and (<= ?forma 8) (eq ?implicacion ENORME))))
	=>
	(send ?libroAbs put-complejidadForma MUY_COMPLEJO)
	(assert (competenciaFormaAsociada))
)

(defrule ASSOCIATION::asociarComplejidadFondoMUY_SIMPLE
	(declare (salience 23))
	(not (competenciaFondoAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaForma ?fondo) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (<= ?fondo 2) (and (<= ?fondo 5) (eq ?implicacion POCA))))
	=>
	(send ?libroAbs put-complejidadFondo MUY_SIMPLE)
	(assert (competenciaFondoAsociada))
)

(defrule ASSOCIATION::asociarComplejidadFondoSIMPLE
	(declare (salience 22))
	(not (competenciaFondoAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaFondo ?Fondo) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (<= ?Fondo 5) (and (<= ?Fondo 8) (eq ?implicacion POCA)) (and (<= ?Fondo 2) (eq ?implicacion ENORME))))
	=>
	(send ?libroAbs put-complejidadFondo SIMPLE)
	(assert (competenciaFondoAsociada))
)

(defrule ASSOCIATION::asociarComplejidadFondoCOMPLEJO
	(declare (salience 21))
	(not (competenciaFondoAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaFondo ?Fondo) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (<= ?Fondo 8) (and (<= ?Fondo 10) (eq ?implicacion POCA)) (and (<= ?Fondo 5) (eq ?implicacion ENORME))))
	=>
	(send ?libroAbs put-complejidadFondo COMPLEJO)
	(assert (competenciaFondoAsociada))
)

(defrule ASSOCIATION::asociarComplejidadFondoMUY_COMPLEJO
	(declare (salience 20))
	(not (competenciaFondoAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (competenciaFondo ?Fondo) (implicacionLector ?implicacion))
	?libroAbs <- (object (is-a LibroAbs))
	(test (or (<= ?Fondo 10) (and (<= ?Fondo 8) (eq ?implicacion ENORME))))
	=>
	(send ?libroAbs put-complejidadFondo MUY_COMPLEJO)
	(assert (competenciaFondoAsociada))
)

(defrule ASSOCIATION::asociarOpinionContemporaneoCONTEMPORANEO
	(not (contempAsociado))
	?usuarioAbs <- (object (is-a LectorAbs) (opinionContemporaneo ?opinionContemp))
	?libroAbs <- (object (is-a LibroAbs))
	(test (eq ?opinionContemp CONTEMPORANEO))
	=>
	(send ?libroAbs put-isContemporaneo TRUE)
	(assert (contempAsociado))
)

(defrule ASSOCIATION::asociarOpinionContemporaneoCLASICO
	(not (contempAsociado))
	?usuarioAbs <- (object (is-a LectorAbs) (opinionContemporaneo ?opinionContemp))
	?libroAbs <- (object (is-a LibroAbs))
	(test (eq ?opinionContemp CLASICO))
	=>
	(send ?libroAbs put-isClasico TRUE)
	(assert (contempAsociado))
)

(defrule ASSOCIATION::asociarOpinionCriticaNULO
	(not (criticaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (nivelCritica ?critica))
	?libroAbs <- (object (is-a LibroAbs))
	(test (eq ?critica NULO))
	=>
	(send ?libroAbs put-nivelCritica NULO)
	(assert (criticaAsociada))
)

(defrule ASSOCIATION::asociarOpinionCriticaPOCO
	(not (criticaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (nivelCritica ?critica))
	?libroAbs <- (object (is-a LibroAbs))
	(test (eq ?critica POCO))
	=>
	(send ?libroAbs put-nivelCritica REGULAR)
	(assert (criticaAsociada))
)

(defrule ASSOCIATION::asociarOpinionCriticaMUCHO
	(not (criticaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (nivelCritica ?critica))
	?libroAbs <- (object (is-a LibroAbs))
	(test (eq ?critica MUCHO))
	=>
	(send ?libroAbs put-nivelCritica BUENO)
	(assert (criticaAsociada))
)

(defrule ASSOCIATION::asociarSusceptibleModaNULO
	(not (modaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (susceptibleModa ?moda))
	?libroAbs <- (object (is-a LibroAbs))
	(test (eq ?moda NULO))
	=>
	(send ?libroAbs put-nivelModa NULO)
	(assert (modaAsociada))
)

(defrule ASSOCIATION::asociarSusceptibleModaPOCO
	(not (modaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (susceptibleModa ?moda))
	?libroAbs <- (object (is-a LibroAbs))
	(test (eq ?moda POCO))
	=>
	(send ?libroAbs put-nivelModa CONOCIDO)
	(assert (modaAsociada))
)

(defrule ASSOCIATION::asociarSusceptibleModaMUCHO
	(not (modaAsociada))
	?usuarioAbs <- (object (is-a LectorAbs) (susceptibleModa ?moda))
	?libroAbs <- (object (is-a LibroAbs))
	(test (eq ?moda MUCHO))
	=>
	(send ?libroAbs put-nivelModa BEST_SELLER)
	(assert (modaAsociada))
)

(defrule ASSOCIATION::switchToSYNTHESIS
	(declare (salience -50))
	=>
	(printout t "Pasando a SYNTHESIS" crlf)
	(focus SYNTHESIS)
)

;;#############################################################################################
;;################################ SYNTHESIS (descarte) #######################################
;;#############################################################################################

;;############################### Classes #####################################################

;;############################### Funciones ###################################################

;;################################ Reglas #####################################################

(defrule SYNTHESIS::sintetizarIdioma
	?usuario <- (object (is-a Lector) (opinion_traduccion ?traduccion))
	?libroAbs <- (object (is-a LibroAbs) (estaEscritoEn $?idiomas))
	?inst <- (object (is-a Libro) (estaEscritoEn ?idioma))
	(test (and (eq ?traduccion FALSE) (not (member$ ?idioma ?idiomas))))
	=>
   	(printout t "Sintetizando Idiomas" ?idioma ?idiomas crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarIdiomaTrad
	?usuario <- (object (is-a Lector) (opinion_traduccion ?traduccion))
	?libroAbs <- (object (is-a LibroAbs) (estaEscritoEn $?idiomas))
	?inst <- (object (is-a Libro) (estaEscritoEn ?idioma) (estaTraducidoA $?idiomasTrad))
	(test (and (eq ?traduccion TRUE) (not (member$ ?idioma ?idiomas))))
	=>
   	(printout t "Sintetizando Idiomas" ?idioma ?idiomas crlf)
	(bind ?found FALSE)
	(loop-for-count (?i 1 (length$ $?idiomas)) do
		(bind ?currIdioma (nth$ ?i $?idiomas))
		(if (member$ ?currIdioma $?idiomasTrad)
			then
			(bind ?found TRUE)
			(break)
		)
	)
	(if (eq ?found FALSE) then (send ?inst delete))
)

(defrule SYNTHESIS::sintetizarGenero
	?libroAbs <- (object (is-a LibroAbs) (contieneGenero $?generos-abs))
	?inst <- (object (is-a Libro) (contieneGenero $?generos-lib))
	=>
	(printout t "Sintetizando Generos" $?generos-lib $?generos-abs crlf)
	(bind ?found FALSE)
	(loop-for-count (?i 1 (length$ $?generos-lib)) do
		(bind ?curr-gen (nth$ ?i $?generos-lib))
		(if (member$ ?curr-gen $?generos-abs)
			then
			(bind ?found TRUE)
			(break)
		)
	)
	(if (eq ?found FALSE) then (send ?inst delete))
)

(defrule SYNTHESIS::sintetizarPaginasCORTO
	?libroAbs <- (object (is-a LibroAbs) (longitud $?longitudes))
	?inst <- (object (is-a Libro) (numero_paginas ?paginas))
	(test (and (> ?paginas 250)(member$ CORTO $?longitudes)(not (member$ MEDIANO $?longitudes))(not (member$ LARGO $?longitudes))(not (member$ ENORME $?longitudes))))
	=>
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarPaginasMEDIANO
	?libroAbs <- (object (is-a LibroAbs) (longitud $?longitudes))
	?inst <- (object (is-a Libro) (numero_paginas ?paginas))
	(test (and (> ?paginas 500)(member$ MEDIANO $?longitudes)(not (member$ LARGO $?longitudes))(not (member$ ENORME $?longitudes))))
	=>
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarPaginasLARGO
	?libroAbs <- (object (is-a LibroAbs) (longitud $?longitudes))
	?inst <- (object (is-a Libro) (numero_paginas ?paginas))
	(test (and (> ?paginas 1000)(member$ LARGO $?longitudes)(not (member$ ENORME $?longitudes))))
	=>
	(send ?inst delete)
)

(defrule SYNTHESIS::descartarLibrosLeidos
	?usuario <- (object (is-a Lector) (haLeido $?librosLeidos))
	?inst <- (object (is-a Libro) (nombre ?nombre))
	(test (member$ (find-instance ((?instLibro Libro))(eq ?instLibro:nombre ?nombre)) $?librosLeidos))
	=>
	(printout t "descartando libros leidos " ?nombre crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::descartarLibrosPorNacionalidadAutor
	?usuario <- (object (is-a Lector) (nacionalidad $?nacionalidadesLector) (interes_extranjero ?interes))
	?instAutor <- (object (is-a Autor) (nacionalidad $?nacionalidadesAutor) (haEscrito $?libros))
	(test (eq ?interes FALSE))
	=>
	(bind ?found FALSE)
	(loop-for-count (?i 1 (length$ $?nacionalidadesLector)) do
		(bind ?currNat (nth$ ?i $?nacionalidadesLector))
		(if (member$ ?currNat $?nacionalidadesAutor)
			then
			(bind ?found TRUE)
			(break)
		)
	)
	(if (eq ?found FALSE) 
		then 
		(loop-for-count (?i 1 (length$ $?libros)) do
		(bind ?currLibr (nth$ ?i $?libros))
		(send ?currLibr delete)
		)
	)
)

(defrule SYNTHESIS::sintetizarComplejidadFondoMUY_SIMPLE
	?libroAbs <- (object (is-a LibroAbs) (complejidadFondo ?fondo))
	?inst <- (object (is-a Libro) (complejidad_tematica ?tematica))
	(test (and (eq ?fondo MUY_SIMPLE) (> ?tematica 2)))
	=>
	(printout t "descartando libros por complejidad de forma " crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarComplejidadFondoSIMPLE
	?libroAbs <- (object (is-a LibroAbs) (complejidadFondo ?fondo))
	?inst <- (object (is-a Libro) (complejidad_tematica ?tematica))
	(test (and (eq ?fondo SIMPLE) (> ?tematica 5) (< ?tematica 3)))
	=>
	(printout t "descartando libros por complejidad de forma " crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarComplejidadFondoCOMPLEJO
	?libroAbs <- (object (is-a LibroAbs) (complejidadFondo ?fondo))
	?inst <- (object (is-a Libro) (complejidad_tematica ?tematica))
	(test (and (eq ?fondo COMPLEJO) (> ?tematica 8) (< ?tematica 6)))
	=>
	(printout t "descartando libros por complejidad de forma " crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarComplejidadFondoMUY_COMPLEJO
	?libroAbs <- (object (is-a LibroAbs) (complejidadFondo ?fondo))
	?inst <- (object (is-a Libro) (complejidad_tematica ?tematica))
	(test (and (eq ?fondo MUY_COMPLEJO) (< ?tematica 9)))
	=>
	(printout t "descartando libros por complejidad de forma " crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarComplejidadFormaMUY_SIMPLE
	?libroAbs <- (object (is-a LibroAbs) (complejidadForma ?Forma))
	?inst <- (object (is-a Libro) (complejidad_discurso ?discurso) (complejidad_linguistica ?linguistica))
	(test (and (eq ?Forma MUY_SIMPLE) (> (div (+ ?discurso ?linguistica) 2) 2)))
	=>
	(printout t "descartando libros por complejidad de forma " crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarComplejidadFormaSIMPLE
	?libroAbs <- (object (is-a LibroAbs) (complejidadForma ?Forma))
	?inst <- (object (is-a Libro) (complejidad_discurso ?discurso) (complejidad_linguistica ?linguistica))
	(test (and (eq ?Forma SIMPLE) (> (div (+ ?discurso ?linguistica) 2) 5) (< (div (+ ?discurso ?linguistica) 2) 3)))
	=>
	(printout t "descartando libros por complejidad de forma " crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarComplejidadFormaCOMPLEJO
	?libroAbs <- (object (is-a LibroAbs) (complejidadForma ?Forma))
	?inst <- (object (is-a Libro) (complejidad_discurso ?discurso) (complejidad_linguistica ?linguistica))
	(test (and (eq ?Forma COMPLEJO) (> (div (+ ?discurso ?linguistica) 2) 8) (< (div (+ ?discurso ?linguistica) 2) 6)))
	=>
	(printout t "descartando libros por complejidad de forma " crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarComplejidadFormaMUY_COMPLEJO
	?libroAbs <- (object (is-a LibroAbs) (complejidadForma ?Forma))
	?inst <- (object (is-a Libro) (complejidad_discurso ?discurso) (complejidad_linguistica ?linguistica))
	(test (and (eq ?Forma MUY_COMPLEJO) (< (div (+ ?discurso ?linguistica) 2) 9)))
	=>
	(printout t "descartando libros por complejidad de forma " crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarOpinionCONTEMPORANEO
	?libroAbs <- (object (is-a LibroAbs) (isContemporaneo ?Contemporaneo))
	?inst <- (object (is-a Libro) (fecha_salida ?fecha))
	(test (and (eq ?Contemporaneo TRUE) (< ?fecha 2010)))
	=>
	(printout t "descartando libros por contemporaneo" crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarOpinionCLASICO
	?libroAbs <- (object (is-a LibroAbs) (isClasico ?Clasico))
	?inst <- (object (is-a Libro) (fecha_salida ?fecha))
	(test (and (eq ?Clasico TRUE) (> ?fecha 1950)))
	=>
	(printout t "descartando libros por clasico" crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarNivelCriticaREGULAR
	?libroAbs <- (object (is-a LibroAbs) (nivelCritica ?critica))
	?inst <- (object (is-a Libro) (porcentaje_critica ?porcentaje_critica))
	(test (and (eq ?critica REGULAR) (< ?porcentaje_critica 70)))
	=>
	(printout t "descartando libros por critica" crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarNivelCriticaBUENO
	?libroAbs <- (object (is-a LibroAbs) (nivelCritica ?critica))
	?inst <- (object (is-a Libro) (porcentaje_critica ?porcentaje_critica))
	(test (and (eq ?critica BUENO) (< ?porcentaje_critica 90)))
	=>
	(printout t "descartando libros por critica" crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarNivelModaCONOCIDO
	?libroAbs <- (object (is-a LibroAbs) (nivelModa ?moda))
	?inst <- (object (is-a Libro) (ejemplares_vendidos ?ventas))
	(test (and (eq ?moda CONOCIDO) (< ?ventas 10000)))
	=>
	(printout t "descartando libros por moda" crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::sintetizarNivelModaBEST_SELLER
	?libroAbs <- (object (is-a LibroAbs) (nivelModa ?moda))
	?inst <- (object (is-a Libro) (ejemplares_vendidos ?ventas) (best_seller ?bestSeller))
	(test (and (eq ?moda BEST_SELLER) (or (< ?ventas 20000) (eq ?bestSeller FALSE))))
	=>
	(printout t "descartando libros por moda" crlf)
	(send ?inst delete)
)

(defrule SYNTHESIS::switchToRECONSTRUCTION
	(declare (salience -50))
	=>
	(printout t "Pasando a RECONSTRUCTION" crlf)
	(focus RECONSTRUCTION)
)

;;#############################################################################################
;;################################ RECONSTRUCTION #############################################
;;#############################################################################################
 
;;############################### Classes #####################################################

(defclass RECONSTRUCTION::LibroHeur
	(is-a USER)
    (role concrete)
    (pattern-match reactive)
	(slot Heuristic
		(type INTEGER)
		(create-accessor read-write))
	(slot Libro
		(type INSTANCE)
		(create-accessor read-write))
	(slot nombreLibro
		(type SYMBOL)
		(create-accessor read-write))
)

(defclass RECONSTRUCTION::Recomendacion
	(is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot librosRecomendados
        (type INSTANCE)
        (create-accessor read-write))
)

;;############################### Funciones ###################################################

(deffunction RECONSTRUCTION::compareLibroHeur (?libro1 ?libro2)
	(< (send ?libro1 get-Heuristic) (send ?libro2 get-Heuristic))
)

;;################################ Reglas #####################################################

(defrule RECONSTRUCTION::instanciarLibroHeur
	(declare (salience 45))
	?inst <- (object (is-a Libro) (nombre ?nombreLibro))
	=>
	(bind ?libro (find-instance ((?instLibro Libro))(eq ?instLibro:nombre ?nombreLibro)))
	(make-instance (sym-cat RECONSTRUCTION:: ?nombreLibro) of LibroHeur
		(Heuristic 0)
		(Libro ?libro)
		(nombreLibro ?nombreLibro)
	)
)

(defrule RECONSTRUCTION::heuristicoGeneros
	?inst <- (object (is-a Libro) (nombre ?libro) (contieneGenero $?generos))
	?usuario <- (object (is-a Lector) (prefiereGenero $?generosPref))
	=>
	(bind ?libroHeur (symbol-to-instance-name ?libro))
	(printout t "LibroHeur debug " ?libroHeur crlf)
	(bind ?count 0)
	(loop-for-count (?i 1 (length$ $?generos)) do
		(bind ?curr-gen (nth$ ?i $?generos))
		(if (member$ ?curr-gen $?generosPref)
			then
			(bind ?count (+ ?count 1))
		)
	)
	(if (> ?count 0) 
		then 
		(bind ?valueHeur (send ?libroHeur get-Heuristic))
		(bind ?valueHeur (+ ?valueHeur (* ?count 10)))
		(send ?libroHeur put-Heuristic ?valueHeur)
	)
)

(defrule RECONSTRUCTION::heuristicoAutores
	?autor <- (object (is-a Autor) (nombre ?nombreAutor) (haEscrito $?librosEscritos))
	?libro <-  (object (is-a Libro) (nombre ?nombreLibro))
	?usuario <- (object (is-a Lector) (prefiereAutor $?autoresPref))
	(test (and (member$ (symbol-to-instance-name (sym-cat MAIN:: ?nombreAutor)) $?autoresPref) (member$ (symbol-to-instance-name (sym-cat MAIN:: ?nombreLibro)) $?librosEscritos)))
	=>
	(bind ?libroHeur (symbol-to-instance-name ?nombreLibro))
	(bind ?valueHeur (send ?libroHeur get-Heuristic))
	(bind ?valueHeur (+ ?valueHeur 10))
	(send ?libroHeur put-Heuristic ?valueHeur)
)

(defrule RECONSTRUCTION::heuristicoEditoriales
	?editorial <- (object (is-a Editorial) (nombre ?nombreEditorial))
	?libro <- (object (is-a Libro) (nombre ?nombreLibro) (estaEditadoPor ?editorialEditora))
	?usuario <- (object (is-a Lector) (prefiereEditorial $?editorialesPref))
	(test (and (member$ (symbol-to-instance-name (sym-cat MAIN:: ?nombreEditorial)) $?editorialesPref) (eq (symbol-to-instance-name (sym-cat MAIN:: ?nombreEditorial)) ?editorialEditora)))
	=>
	(bind ?libroHeur (symbol-to-instance-name ?nombreLibro))
	(bind ?valueHeur (send ?libroHeur get-Heuristic))
	(bind ?valueHeur (+ ?valueHeur 10))
	(send ?libroHeur put-Heuristic ?valueHeur)
)

(defrule RECONSTRUCTION::crearRecomendacion
	(declare (salience -10))
	=>
	(bind $?librosHeur (find-all-instances ((?inst LibroHeur)) TRUE))
	(printout t "Libros Recomendables: " $?librosHeur crlf)
	(bind $?librosHeurSorted (sort compareLibroHeur $?librosHeur))
	(printout t "Libros sorted: " $?librosHeurSorted crlf)
	(bind $?librosRecomendacion3 (create$))
	(loop-for-count (?index 1 (min 3 (length$ $?librosHeurSorted))) do
		(bind ?aux (nth$ ?index ?librosHeurSorted))
		(bind ?aux (sym-cat RECONSTRUCTION:: ?aux))
		(bind ?aux (symbol-to-instance-name ?aux))
		(bind ?librosRecomendacion3 (insert$ ?librosRecomendacion3 (+ (length$ $?librosRecomendacion3) 1) ?aux))
	)
	(make-instance RecomendacionFinal of Recomendacion
		(librosRecomendados ?librosRecomendacion3)
	)
)

(defrule RECONSTRUCTION::switchToOUTPUT
	(declare (salience -50))
	=>
	(printout t "Pasando a OUTPUT" crlf)
	(focus OUTPUT)
)

;;#############################################################################################
;;##################################### OUTPUT ################################################
;;#############################################################################################

;;############################### Classes #####################################################

;;############################### Funciones ###################################################

(deffunction OUTPUT::printLibro (?libroHeur)

	(bind ?libro (send ?libroHeur get-Libro))
	(bind ?nombre (send ?libro get-nombre))
    (bind ?genero (send ?libro get-contieneGenero))
	(bind ?idioma (send ?libro get-estaEscritoEn))
	
    (printout t "Nombre: " ?nombre crlf)
    (printout t "Genero: " ?genero crlf)
    (printout t "Idioma: " ?idioma crlf)
)

;;################################ Reglas #####################################################

(defrule OUTPUT::printRecomendacion
	?recomendacion <- (object (is-a Recomendacion) (librosRecomendados $?librosRec))
	=>
	(printout t "Recomendaciones: " crlf)
	(loop-for-count (?index 1 (length$ $?librosRec)) do
		(printout t "RECOMENDACION " ?index crlf)
		(bind ?aux (nth$ ?index $?librosRec))
		(printLibro ?aux)
	)
)