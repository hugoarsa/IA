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
	(import MAIN ?ALL)
	;;(import entrada-lector ?ALL)
	(export ?ALL)
)

;; Modulo de inferencia de libros potenciales
(defmodule ASSOCIATION
	(import MAIN ?ALL)
	;;(import abstraccion-lector(export ?ALL))
	(export ?ALL)
)

;; Modulo de reconstruccion resolutiva de libros recomendados
(defmodule SYNTHESIS
	(import MAIN ?ALL)
	;;(import asociacion-libros(export ?ALL))
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
	(focus INPUT)
)


;;#############################################################################################
;;################################ Entrada Lector #############################################
;;#############################################################################################

;; Este modulo se encarga de hacertodas las preguntas del usuario (lector) para obtener toda
;; la informacion sobre sus preferencias, caracteristicas, etc. Para hacer una buena recomendacion

;;############################### Funciones ###################################################

;; Funcion para hacer pregunta categorica dentro de un rango de valores permitidos
(deffunction INPUT::pregunta-choice (?pregunta $?valores_permitidos)
	(progn$
		(bind ?var ?valores_permitidos)
		(lowcase ?var))
	(format t "%s? (%s) " ?pregunta (implode$ ?var))
	(bind ?respuesta (read))
	(while (not (member$ (lowcase ?respuesta) ?var)) do
		(format t "%s? (%s) " ?pregunta (implode$ ?var))
		(bind ?respuesta (read))
	)
	?respuesta
)

;; Funcion para hacer una pregunta numerica dentro de un rango de valores permitidos
(deffunction INPUT::pregunta-numerica (?pregunta ?rangini ?rangfi)
	(format t "%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
	(bind ?respuesta (read))
	(while (not(and(> ?respuesta ?rangini)(< ?respuesta ?rangfi))) do
		(format t "%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
		(bind ?respuesta (read))
	)
	?respuesta
)

;; Funcion para hacer una pregunta binaria
(deffunction INPUT::pregunta-binaria (?pregunta)
    (format t "%s? [Si/No] " ?pregunta)
	(bind ?respuesta (read))
    (while (not (or (eq (lowcase ?respuesta) si)(eq (lowcase ?respuesta) no))) do
        (format t "%s? [Si/No] " ?pregunta)
	    (bind ?respuesta (read))
    )
    (if (eq ?respuesta si)
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

(deffunction INPUT::instanciar-lector ()
	(bind ?nombre (pregunta-general "Cual es tu nombre" ))
	(bind ?edad (pregunta-numerica "Cual es tu edad" 1 100))
	(bind ?nacionalidades (pregunta-lista "Cual/es son tus nacionalidades"))
	(bind ?libros (pregunta-lista "Que libros son tus favoritos"))
	(bind ?idiomas (pregunta-lista "Que idiomas hablas/entiendes bien"))
	(bind ?autores (pregunta-lista "Tienes algun/os autores favoritos"))
	(bind ?generos (pregunta-lista "Que generos te suelen gustar"))
	(bind ?frecuencia_lectura (pregunta-numerica "Como de frecuentemente lees en dias a la semana?" 0 7))
	(bind ?interes_extranjero TRUE);;(pregunta-binaria "Tienes interes en obras y autores extranjeros?"))
	(bind ?lugar_lectura [Cama]);;(pregunta-choice "Donde sueles leer" [Casa] [Cama] [Exteriores] [Transporte_Publico]))
	(bind ?momento_de_lectura [Noche]);;(pregunta-choice "Cuando sueles leer" [Manana] [Tarde] [Noche] [Fin_de_semana]))
	(bind ?susceptible_moda (pregunta-numerica "Cuan de susceptible a la moda te consideras (1 muy poco, 10 mucho)" 1 10))
	(bind ?tiempo_disponible (pregunta-numerica "Cuantas horas a la semana sueles leer" 0 40))
	(make-instance Usuario of Lector
		(edad ?edad)
		(nacionalidad ?nacionalidades)
		(nombre ?nombre)
		(haLeido ?libros) ; Assuming Book1 and Book2 are instances of books
		(hablaIdioma ?idiomas)
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

;;############################### Reglas ###################################################

(defrule INPUT::instanciarLector
	(declare (salience 10))
	=> 
	(printout t "Ahora te haremos unas preguntas para poder generar la rutina que mejor se adapte a ti." crlf crlf)
	(instanciar-lector)
	(focus ABSTRACTION)
)
