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

;; Modulo de reconstruccion resolutiva de libros recomendados
(defmodule SYNTHESIS
	(import MAIN ?ALL)
	(import ASSOCIATION ?ALL)
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

;;############################### Funciones ###################################################

;; Funcion para hacer pregunta categorica dentro de un rango de valores permitidos
(deffunction INPUT::pregunta-choice (?pregunta $?valores_permitidos)
   (bind ?var (lowcase (implode$ $?valores_permitidos)))
   (format t "%s? [%s] " ?pregunta (implode$ ?valores_permitidos))
   (bind ?respuesta (readline))
   (while (not (member$ ?respuesta $?valores_permitidos)) do
      (format t "%s? [%s] " ?pregunta (implode$ ?valores_permitidos))
      (bind ?respuesta (readline))
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
(deffunction INPUT::string-a-instancia ($?lista)
	(printout t "Original List" ?lista crlf)
   (bind ?listaRes (create$))
   (loop-for-count (?index 1 (length ?lista)) do
      (bind ?current_item (nth$ ?index ?lista))
      (bind ?aux (find-instance ((?inst Idioma))(eq (lowcase ?inst:nombre) (lowcase ?current_item))))
        (if ?aux
        then
            (bind ?listaRes (insert$ ?listaRes (+ (length$ $?listaRes) 1) ?aux))
            (printout t "Instance found for: " ?listaRes crlf)

        else
            (printout t "Instance not found for: " ?current_item crlf)
   		)
   )
   ?listaRes
)


(deffunction INPUT::instanciar-lector ()
	(bind ?nombre (pregunta-general "Cual es tu nombre" ))
	(bind ?edad (pregunta-numerica "Cual es tu edad" 1 100))
	(bind $?nacionalidades (pregunta-lista "Cual/es son tus nacionalidades"))
	(bind $?libros (pregunta-lista "Que libros son tus favoritos"))
	(bind $?idiomas (pregunta-lista "Que idiomas hablas/entiendes bien"))
	(bind $?autores (pregunta-lista "Tienes algun/os autores favoritos"))
	(bind $?generos (pregunta-lista "Que generos te suelen gustar"))
	(bind ?frecuencia_lectura (pregunta-numerica "Como de frecuentemente lees en dias a la semana" 0 7))
	(bind ?interes_extranjero (pregunta-binaria "Tienes interes en obras y autores extranjeros"))
	(bind ?lugar_lectura (pregunta-choice "Donde sueles leer" (create$ "Casa" "Cama" "Exteriores" "Transporte_Publico")))
	(bind ?momento_de_lectura (pregunta-choice "Cuando sueles leer" (create$ "Manana" "Tarde" "Noche" "Fin_de_semana")))
	(bind ?susceptible_moda (pregunta-numerica "Cuan de susceptible a la moda te consideras (1 muy poco, 10 mucho)" 1 10))
	(bind ?tiempo_disponible (pregunta-numerica "Cuantas horas a la semana sueles leer" 0 40))
	
	(bind ?idiomasProc (string-a-instancia ?idiomas))
	
	(make-instance Usuario of Lector
		(edad ?edad)
		(nacionalidad ?nacionalidades)
		(nombre ?nombre)
		(haLeido ?libros) ; Assuming Book1 and Book2 are instances of books
		(hablaIdioma ?idiomasProc)
		(prefiereAutor ?autores) ; Assuming Author1 and Author2 are instances of authors
		(prefiereGenero ?generos)
		(frecuencia_lectura ?frecuencia_lectura)
		(interes_extranjero ?interes_extranjero) ; Assuming SI is a symbol indicating interest
		(lugar_lectura ?lugar_lectura)
		(momento_de_lectura ?momento_de_lectura)
		(susceptible_moda ?susceptible_moda) ; Assuming NO is a symbol indicating not susceptible
		(tiempo_disponible ?tiempo_disponible)
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
	(multislot generosInteres
		(type INSTANCE)
		(create-accessor read-write))
	(slot implicacionLector
		(type STRING)
		(create-accessor read-write))
)

;;############################### Funciones ###################################################

;;################################ Reglas #####################################################

(defrule ABSTRACTION::instanciarLectorAbs
   (declare (salience 50))
   (not (LectorAbs))
   =>
   (printout t "Instanciando LectorAbs" crlf)
   (make-instance UsuarioAbs of LectorAbs
		;; aquí añadir las mierdas para single slots
		)
)

(defrule ABSTRACTION::abstraerIdioma
	?usuario <- (object (is-a Lector))
	?usuarioAbs <- (object (is-a LectorAbs))
	=>
   	(printout t "Abstrayendo Idiomas" crlf)
	(bind ?idiomas (send ?usuario get-hablaIdioma))
	(send ?usuarioAbs put-hablaIdioma ?idiomas)
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
	;; añadir slots
)

;;############################### Funciones ###################################################

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
	(bind ?idiomas (send ?usuarioAbs get-hablaIdioma))
	(send ?libroAbs put-estaEscritoEn ?idiomas)
)

;; añadir los 

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
	?libroAbs <- (object (is-a LibroAbs) (estaEscritoEn $?idiomas))
	?inst <- (object (is-a Libro) (estaEscritoEn ?idioma))
	(test (not (member$ ?idioma ?idiomas)))
	=>
   	(printout t "Sintetizando Idiomas" ?idioma ?idiomas crlf)
	(send ?inst delete)
)