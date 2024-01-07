(define (domain book-plan)
  (:requirements :adl :typing)
  (:types
    Book Month)
  
  (:predicates
    (goal ?b - Book)  ;;book ?b is a goal book and MUST be read in order for the plan to be considered correct
    (read ?b - Book)  ;;book ?b has been read
    (readMonth ?b - Book ?m - Month)  ;;book ?b has been read on month ?m. Auxiliary predicate to (read ?b) for temporality
    (predecessor ?b1 - Book ?b2 - Book) ;;book ?b1 precedes book ?b2 and therefore MUST be read BEFORE
    (before ?m1 - Month ?m2 - Month)  ;;month ?m1 goes is earlier than month ?m2 (not necessarily inmediately before)
  )
  
  (:action add-predecessors-mandatory
    :parameters (?b1 - Book ?b2 - Book)
    ;;If book ?b1 is predecessor to ?b2 and ?b2 is a book that MUST be read and has not been read...
    :precondition (and (predecessor ?b1 ?b2) (goal ?b2) (not (read ?b2)))
    ;;...Book ?b1 is a goal book and now MUST be integrated into the plan
    :effect (goal ?b1)
  )

  (:action plan-reading
    :parameters (?b - Book ?m - Month)
    :precondition (and (goal ?b) (not (read ?b)) ;;Book ?b is a book that MUST be read and has not been in order to be put into the plan
            (not (exists (?b2 - Book ?m2 - Month)
              (or
                ;;There MUST NOT exist a book that is predecessor to book ?b and or hasn't been read at all or if it has been read it wasn't on a previous month to ?m
                ;;Note that this check makes it so books that just have the predicate (read ?b2) and not (readMonth ?b2 ?m2) are considered valid books to exist,
                ;;since it would mean they have been generated on the input file and thus have been read before our plan even began
                (and (predecessor ?b2 ?b) (or (not (read ?b2)) (and (readMonth ?b2 ?m2)
                (not (before ?m2 ?m)))))
              )
            )
            )
    )
    ;;The book is read on the month set and the pages for that month are updated appropiately
    :effect (and (readMonth ?b ?m) (read ?b))
  )
)
