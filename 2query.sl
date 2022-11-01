	(set-logic LIA)

	;; Name and signature of the function to be synthesized
	(synth-fun function (( y_target Int )) Int

	;; Declare the nonterminals used in the grammar
	((y Int))
	  (
	    (y Int
	      (y_target (+ y 1))
	    )
	  )
	)
	(constraint
	  (forall ((y Int))
	    (=>
	      (= y 1)
	      (= (function y) 1)
	    )
	  )
	)
	(check-synth)
