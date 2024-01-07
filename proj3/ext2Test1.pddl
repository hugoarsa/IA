(define (problem book-plan-8)
  (:domain book-plan)
  (:objects
    Book0 Book1 Book2 Book3 Book4 Book5 Book6 Book7 - Book ; List all the books
    Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec - Month ; List all the months
  )

  (:init
    (predecessor Book0 Book2)
    (parallel Book0 Book6)
    (parallel Book1 Book2)
    (predecessor Book1 Book5)
    (predecessor Book1 Book6)
    (predecessor Book1 Book7)
    (predecessor Book2 Book5)
    (parallel Book2 Book7)
    (predecessor Book3 Book5)
    (parallel Book3 Book6)
    (parallel Book4 Book6)
    (parallel Book4 Book7)
    (parallel Book5 Book6)
    (parallel Book6 Book7)
    (goal Book5)
    (read Book0)
    (read Book2)
    (before Jan Feb)
    (before Jan Mar)
    (before Jan Apr)
    (before Jan May)
    (before Jan Jun)
    (before Jan Jul)
    (before Jan Aug)
    (before Jan Sep)
    (before Jan Oct)
    (before Jan Nov)
    (before Jan Dec)
    (before Feb Mar)
    (before Feb Apr)
    (before Feb May)
    (before Feb Jun)
    (before Feb Jul)
    (before Feb Aug)
    (before Feb Sep)
    (before Feb Oct)
    (before Feb Nov)
    (before Feb Dec)
    (before Mar Apr)
    (before Mar May)
    (before Mar Jun)
    (before Mar Jul)
    (before Mar Aug)
    (before Mar Sep)
    (before Mar Oct)
    (before Mar Nov)
    (before Mar Dec)
    (before Apr May)
    (before Apr Jun)
    (before Apr Jul)
    (before Apr Aug)
    (before Apr Sep)
    (before Apr Oct)
    (before Apr Nov)
    (before Apr Dec)
    (before May Jun)
    (before May Jul)
    (before May Aug)
    (before May Sep)
    (before May Oct)
    (before May Nov)
    (before May Dec)
    (before Jun Jul)
    (before Jun Aug)
    (before Jun Sep)
    (before Jun Oct)
    (before Jun Nov)
    (before Jun Dec)
    (before Jul Aug)
    (before Jul Sep)
    (before Jul Oct)
    (before Jul Nov)
    (before Jul Dec)
    (before Aug Sep)
    (before Aug Oct)
    (before Aug Nov)
    (before Aug Dec)
    (before Sep Oct)
    (before Sep Nov)
    (before Sep Dec)
    (before Oct Nov)
    (before Oct Dec)
    (before Nov Dec)
    (sameTime Jan Jan)
    (sameTime Feb Feb)
    (sameTime Mar Mar)
    (sameTime Apr Apr)
    (sameTime May May)
    (sameTime Jun Jun)
    (sameTime Jul Jul)
    (sameTime Aug Aug)
    (sameTime Sep Sep)
    (sameTime Oct Oct)
    (sameTime Nov Nov)
    (sameTime Dec Dec)
  )

  (:goal
    (not (exists (?b - Book) 
      (and (goal ?b) (not (read ?b)))))
  )
)
