(define (problem book-plan-20)
  (:domain book-plan)
  (:objects
    Book0 Book1 Book2 Book3 Book4 Book5 Book6 Book7 Book8 Book9 Book10 Book11 Book12 Book13 Book14 Book15 Book16 Book17 Book18 Book19 - Book ; List all the books
    Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec - Month ; List all the months
  )

  (:init
    (parallel Book0 Book2)
    (parallel Book0 Book3)
    (parallel Book0 Book11)
    (parallel Book0 Book13)
    (parallel Book0 Book19)
    (predecessor Book1 Book2)
    (parallel Book1 Book4)
    (predecessor Book1 Book5)
    (predecessor Book1 Book7)
    (predecessor Book1 Book19)
    (parallel Book2 Book4)
    (parallel Book2 Book5)
    (predecessor Book2 Book9)
    (predecessor Book2 Book10)
    (parallel Book2 Book12)
    (predecessor Book3 Book6)
    (predecessor Book3 Book11)
    (predecessor Book3 Book12)
    (predecessor Book3 Book16)
    (predecessor Book3 Book17)
    (parallel Book3 Book19)
    (parallel Book5 Book10)
    (parallel Book5 Book11)
    (parallel Book5 Book12)
    (parallel Book5 Book13)
    (predecessor Book5 Book19)
    (predecessor Book6 Book7)
    (predecessor Book6 Book8)
    (predecessor Book6 Book12)
    (parallel Book6 Book16)
    (predecessor Book7 Book9)
    (predecessor Book7 Book12)
    (parallel Book7 Book18)
    (parallel Book8 Book9)
    (predecessor Book8 Book11)
    (predecessor Book8 Book12)
    (predecessor Book8 Book13)
    (predecessor Book8 Book14)
    (predecessor Book9 Book11)
    (predecessor Book9 Book14)
    (predecessor Book9 Book15)
    (predecessor Book9 Book16)
    (predecessor Book9 Book17)
    (predecessor Book9 Book19)
    (predecessor Book10 Book11)
    (predecessor Book10 Book19)
    (predecessor Book11 Book12)
    (predecessor Book11 Book15)
    (predecessor Book12 Book15)
    (parallel Book12 Book16)
    (predecessor Book13 Book16)
    (predecessor Book13 Book17)
    (parallel Book13 Book18)
    (parallel Book13 Book19)
    (parallel Book14 Book18)
    (predecessor Book15 Book17)
    (predecessor Book15 Book18)
    (goal Book10)
    (goal Book13)
    (goal Book16)
    (goal Book19)
    (read Book5)
    (read Book7)
    (read Book14)
    (read Book19)
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