(ns scheme-interpreter.core-test
	(:require [clojure.test :refer :all]
						[scheme-interpreter.core :refer :all]))

(deftest leer-entrada-test
	(testing "TEST leer-entrada"
		(println "TEST de leer-entrada:\n")

		(println "Ingresar: (hola mundo)\n")
		(is (= (leer-entrada) "(hola mundo)"))

		(println "Ingresar: 123\n")
		(is (= (leer-entrada) "123"))
	)
)

(deftest verificar-parentesis-test
	(testing "TEST verificar-parentesis"
		(let [res1 (verificar-parentesis "(hola 'mundo"),
			res2 (verificar-parentesis "(hola '(mundo)))"),
			res3 (verificar-parentesis "(hola '(mundo) () 6) 7)"),
			res4 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)"),
			res5 (verificar-parentesis "(hola '(mundo) )")]

			(is (= res1 1))
			(is (= res2 -1))
			(is (= res3 -1))
			(is (= res4 -1))
			(is (= res5 0))
		)
	)
)

(deftest actualizar-amb-test
	(testing "TEST actualizar-amb"
		(let [res1 (actualizar-amb '(a 1 b 2 c 3) 'd 4),
			res2 (actualizar-amb '(a 1 b 2 c 3) 'b 4),
			error (first (generar-mensaje-error :unbound-variable 'f)),
			res3 (actualizar-amb '(a 1 b 2 c 3) 'b (list error 'mal 'hecho)),
			res4 (actualizar-amb '() 'b 7)]

			(is (= res1 (list 'a 1 'b 2 'c 3 'd 4)))
			(is (= res2 (list 'a 1 'b 4 'c 3)))
			(is (= res3 (list 'a '1 'b 2 'c '3)))
			(is (= res4 (list 'b 7)))
		)
	)
)

(deftest buscar-test
	(testing "TEST buscar"
		(let [res1 (buscar 'c '(a 1 b 2 c 3 d 4 e 5)),
			res2 (buscar 'f '(a 1 b 2 c 3 d 4 e 5))]

			(is (= res1 3))
			(is (= res2 (generar-mensaje-error :unbound-variable 'f)))
		)
	)
)

(deftest error?-test
	(testing "TEST error?"
		(let [res1 (error? (list (symbol ";ERROR:") 'mal 'hecho)),
			res2 (error? (list 'mal 'hecho)),
			res3 (error? (list (symbol ";WARNING:") 'mal 'hecho))]

			(is res1)
			(is (not res2))
			(is res3)
		)
	)
)

(deftest proteger-bool-en-str-test
	(testing "TEST proteger-bool-en-str"
		(let [res1 (proteger-bool-en-str "(or #F #f #t #T)"),
			res2 (proteger-bool-en-str "(and (or #F #f #t #T) #T)"),
			res3 (proteger-bool-en-str "")]

			(is (= res1 "(or %F %f %t %T)"))
			(is (= res2 "(and (or %F %f %t %T) %T)"))
			(is (= res3 ""))
		)
	)
)

(deftest restaurar-bool-test
	(testing "TEST restaurar-bool"
		(let [res1 (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))),
			res2 (restaurar-bool (read-string "(and (or %F %f %t %T) %T)")),
			res1-expected (list (symbol "and") (list (symbol "or") (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")) (symbol "#t")),
			res2-expected (list (symbol "and") (list (symbol "or") (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")) (symbol "#t"))]

			(is (= res1 res1-expected))
			(is (= res2 res2-expected))
		)
	)
)

(deftest igual?-test
	(testing "TEST igual?"
		(let [res1 (igual? 'if 'IF),
			res2 (igual? 'if 'if),
			res3 (igual? 'IF 'IF),
			res4 (igual? 'IF "IF"),
			res5 (igual? 6 "6")]

			(is res1)
			(is res2)
			(is res3)
			(is (not res4))
			(is (not res5))
		)
	)
)

(deftest fnc-append-test
	(testing "TEST fnc-append"
		(let [res1 (fnc-append '( (1 2) (3) (4 5) (6 7))),
			res2 (fnc-append '( (1 2) 3 (4 5) (6 7))),
			res3 (fnc-append '( (1 2) A (4 5) (6 7)))]

			(is (= res1 '(1 2 3 4 5 6 7)))
			(is (= res2 (generar-mensaje-error :wrong-type-arg 'append 3)))
			(is (= res3 (generar-mensaje-error :wrong-type-arg 'append 'A)))
		)
	)
)

(deftest fnc-equal?-test
	(testing "TEST fnc-equal?"
		(let [res1 (fnc-equal? ()),
			res2 (fnc-equal? '(A)),
			res3 (fnc-equal? '(A a)),
			res4 (fnc-equal? '(A a A)),
			res5 (fnc-equal? '(A a A a)),
			res6 (fnc-equal? '(A a A B)),
			res7 (fnc-equal? '(1 1 1 1)),
			res8 (fnc-equal? '(1 1 2 1)),
			t (symbol "#t"),
			f (symbol "#f")]

			(is (= res1 t))
			(is (= res2 t))
			(is (= res3 t))
			(is (= res4 t))
			(is (= res5 t))
			(is (= res6 f))
			(is (= res7 t))
			(is (= res8 f))
		)
	)
)

(deftest fnc-read-test
	(testing "TEST fnc-read"
		(let [error-ports (generar-mensaje-error :io-ports-not-implemented 'read),
			error-number-args (generar-mensaje-error :wrong-number-args-oper 'read)]
			(println "TEST de read:\n")

			(println "Ingresar: (hola mundo)\n")
			(is (= (fnc-read ()) '(hola mundo)))

			(is (= (fnc-read '(1)) error-ports))
			(is (= (fnc-read '(1 2)) error-number-args))
			(is (= (fnc-read '(1 2 3)) error-number-args))
		)
	)
)

(deftest fnc-sumar-test
	(testing "TEST fnc-sumar"
		(let [res1 (fnc-sumar ()),
			res2 (fnc-sumar '(3)),
			res3 (fnc-sumar '(3 4)),
			res4 (fnc-sumar '(3 4 5)),
			res5 (fnc-sumar '(3 4 5 6)),
			res6 (fnc-sumar '(A 4 5 6)),
			res7 (fnc-sumar '(3 A 5 6)),
			res8 (fnc-sumar '(3 4 A 6)),
			error1A (generar-mensaje-error :wrong-type-arg1 '+ 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 '+ 'A)]

			(is (= res1 0))
			(is (= res2 3))
			(is (= res3 7))
			(is (= res4 12))
			(is (= res5 18))
			(is (= res6 error1A))
			(is (= res7 error2A))
			(is (= res8 error2A))
		)
	)
)

(deftest fnc-multiplicar-test
	(testing "TEST fnc-multiplicar"
		(let [res1 (fnc-multiplicar ()),
			res2 (fnc-multiplicar '(3)),
			res3 (fnc-multiplicar '(3 4)),
			res4 (fnc-multiplicar '(3 4 5)),
			res5 (fnc-multiplicar '(3 4 5 6)),
			res6 (fnc-multiplicar '(A 4 5 6)),
			res7 (fnc-multiplicar '(3 A 5 6)),
			res8 (fnc-multiplicar '(3 4 A 6)),
			error1A (generar-mensaje-error :wrong-type-arg1 '* 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 '* 'A)]

			(is (= res1 1))
			(is (= res2 3))
			(is (= res3 12))
			(is (= res4 60))
			(is (= res5 360))
			(is (= res6 error1A))
			(is (= res7 error2A))
			(is (= res8 error2A))
		)
	)
)

(deftest fnc-dividir-test
	(testing "TEST fnc-multiplicar"
		(let [res1 (fnc-dividir ()),
			res2 (fnc-dividir '(3)),
			res3 (fnc-dividir '(2 4)),
			res4 (fnc-dividir '(4 2 2)),
			res5 (fnc-dividir '(A 4 5 6)),
			res6 (fnc-dividir '(3 A 5 6)),
			errorNoArgs (generar-mensaje-error :wrong-number-args-oper '/),
			error1A (generar-mensaje-error :wrong-type-arg1 '/ 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 '/ 'A)]

			(is (= res1 errorNoArgs))
			(is (= res2 (/ 1 3)))
			(is (= res3 (/ 2 4)))
			(is (= res4 (/ 4 2 2)))
			(is (= res5 error1A))
			(is (= res6 error2A))
		)
	)
)

(deftest fnc-quotient-test
	(testing "TEST fnc-quotient"
		(let [res1 (fnc-quotient ()),
			res2 (fnc-quotient '(3)),
			res3 (fnc-quotient '(3 4 5)),
			res4 (fnc-quotient '(5 2)),
			res5 (fnc-quotient '(4 2)),
			res6 (fnc-quotient '(3 2)),
			res7 (fnc-quotient '(A 4)),
			res8 (fnc-quotient '(3 A)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'quotient)
			error1A (generar-mensaje-error :wrong-type-arg1 'quotient 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 'quotient 'A)]

			(is (= res1 errorArgs))
			(is (= res2 errorArgs))
			(is (= res3 errorArgs))
			(is (= res4 2))
			(is (= res5 2))
			(is (= res6 1))
			(is (= res7 error1A))
			(is (= res8 error2A))
		)
	)
)

(deftest fnc-remainder-test
	(testing "TEST fnc-remainder"
		(let [res1 (fnc-remainder ()),
			res2 (fnc-remainder '(3)),
			res3 (fnc-remainder '(1 2 3)),
			res4 (fnc-remainder '(25 6)),
			res5 (fnc-remainder '(8 3)),
			res6 (fnc-remainder '(3 3)),
			res7 (fnc-remainder '(A 4)),
			res8 (fnc-remainder '(3 A)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'remainder)
			error1A (generar-mensaje-error :wrong-type-arg1 'remainder 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 'remainder 'A)]

			(is (= res1 errorArgs))
			(is (= res2 errorArgs))
			(is (= res3 errorArgs))
			(is (= res4 1))
			(is (= res5 2))
			(is (= res6 0))
			(is (= res7 error1A))
			(is (= res8 error2A))
		)
	)
)

(deftest fnc-abs-test
	(testing "TEST fnc-abs"
		(let [res1 (fnc-abs ()),
			res2 (fnc-abs '(3 4)),
			res3 (fnc-abs '(2)),
			res4 (fnc-abs '(-2))
			res5 (fnc-abs '(A)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'abs),
			error1A (generar-mensaje-error :wrong-type-arg1 'abs 'A)]

			(is (= res1 errorArgs))
			(is (= res2 errorArgs))
			(is (= res3 2))
			(is (= res4 2))
			(is (= res5 error1A))
		)
	)
)

(deftest fnc-sqrt-test
	(testing "TEST fnc-sqrt"
		(let [res1 (fnc-sqrt ()),
			res2 (fnc-sqrt '(3 4)),
			res3 (fnc-sqrt '(4)),
			res4 (fnc-sqrt '(16))
			res5 (fnc-sqrt '(A)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'sqrt),
			error1A (generar-mensaje-error :wrong-type-arg1 'sqrt 'A)]

			(is (= res1 errorArgs))
			(is (= res2 errorArgs))
			(is (= res3 2.0))
			(is (= res4 4.0))
			(is (= res5 error1A))
		)
	)
)

(deftest fnc-even?-test
	(testing "TEST fnc-even?"
		(let [res1 (fnc-even? ()),
			res2 (fnc-even? '(3 4)),
			res3 (fnc-even? '(2)),
			res4 (fnc-even? '(3))
			res5 (fnc-even? '(A)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'even?),
			error1A (generar-mensaje-error :wrong-type-arg1 'even? 'A),
			t (symbol "#t"),
			f (symbol "#f")]

			(is (= res1 errorArgs))
			(is (= res2 errorArgs))
			(is (= res3 t))
			(is (= res4 f))
			(is (= res5 error1A))
		)
	)
)

(deftest fnc-odd?-test
	(testing "TEST fnc-odd?"
		(let [res1 (fnc-odd? ()),
			res2 (fnc-odd? '(3 4)),
			res3 (fnc-odd? '(2)),
			res4 (fnc-odd? '(3))
			res5 (fnc-odd? '(A)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'odd?),
			error1A (generar-mensaje-error :wrong-type-arg1 'odd? 'A),
			t (symbol "#t"),
			f (symbol "#f")]

			(is (= res1 errorArgs))
			(is (= res2 errorArgs))
			(is (= res3 f))
			(is (= res4 t))
			(is (= res5 error1A))
		)
	)
)

(deftest fnc-zero?-test
	(testing "TEST fnc-zero?"
		(let [res1 (fnc-zero? ()),
			res2 (fnc-zero? '(3 4)),
			res3 (fnc-zero? '(2)),
			res4 (fnc-zero? '(0))
			res5 (fnc-zero? '(A)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'zero?),
			error1A (generar-mensaje-error :wrong-type-arg1 'zero? 'A),
			t (symbol "#t"),
			f (symbol "#f")]

			(is (= res1 errorArgs))
			(is (= res2 errorArgs))
			(is (= res3 f))
			(is (= res4 t))
			(is (= res5 error1A))
		)
	)
)

(deftest fnc-min-test
	(testing "TEST fnc-min"
		(let [res1 (fnc-min ()),
			res2 (fnc-min '(3)),
			res3 (fnc-min '(3 4)),
			res4 (fnc-min '(3 5 4)),
			res5 (fnc-min '(A 4 5 6)),
			res6 (fnc-min '(3 A 5 6)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'min),
			error1A (generar-mensaje-error :wrong-type-arg1 'min 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 'min 'A)]

			(is (= res1 errorArgs))
			(is (= res2 3))
			(is (= res3 3))
			(is (= res4 3))
			(is (= res5 error1A))
			(is (= res6 error2A))
		)
	)
)

(deftest fnc-max-test
	(testing "TEST fnc-max"
		(let [res1 (fnc-max ()),
			res2 (fnc-max '(3)),
			res3 (fnc-max '(3 4)),
			res4 (fnc-max '(3 5 4)),
			res5 (fnc-max '(A 4 5 6)),
			res6 (fnc-max '(3 A 5 6)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'max),
			error1A (generar-mensaje-error :wrong-type-arg1 'max 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 'max 'A)]

			(is (= res1 errorArgs))
			(is (= res2 3))
			(is (= res3 4))
			(is (= res4 5))
			(is (= res5 error1A))
			(is (= res6 error2A))
		)
	)
)

(deftest fnc-expt-test
	(testing "TEST fnc-expt"
		(let [res1 (fnc-expt ()),
			res2 (fnc-expt '(3)),
			res3 (fnc-expt '(3 4 5)),
			res4 (fnc-expt '(5 2)),
			res5 (fnc-expt '(4 2)),
			res6 (fnc-expt '(3 3)),
			res7 (fnc-expt '(A 4)),
			res8 (fnc-expt '(3 A)),
			errorArgs (generar-mensaje-error :wrong-number-args-prim-proc 'expt)
			error1A (generar-mensaje-error :wrong-type-arg1 'expt 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 'expt 'A)]

			(is (= res1 errorArgs))
			(is (= res2 errorArgs))
			(is (= res3 errorArgs))
			(is (= res4 25))
			(is (= res5 16))
			(is (= res6 27))
			(is (= res7 error1A))
			(is (= res8 error2A))
		)
	)
)

(deftest fnc-restar-test
	(testing "TEST fnc-restar"
		(let [res1 (fnc-restar ()),
			res2 (fnc-restar '(3)),
			res3 (fnc-restar '(3 4)),
			res4 (fnc-restar '(3 4 5)),
			res5 (fnc-restar '(3 4 5 6)),
			res6 (fnc-restar '(A 4 5 6)),
			res7 (fnc-restar '(3 A 5 6)),
			res8 (fnc-restar '(3 4 A 6)),
			errorNoArgs (generar-mensaje-error :wrong-number-args-oper "-")
			error1A (generar-mensaje-error :wrong-type-arg1 '- 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 '- 'A)]

			(is (= res1 errorNoArgs))
			(is (= res2 -3))
			(is (= res3 -1))
			(is (= res4 -6))
			(is (= res5 -12))
			(is (= res6 error1A))
			(is (= res7 error2A))
			(is (= res8 error2A))
		)
	)
)

(deftest fnc-menor-test
	(testing "TEST fnc-menor"
		(let [res1 (fnc-menor ()),
			res2 (fnc-menor '(1)),
			res3 (fnc-menor '(1 2)),
			res4 (fnc-menor '(1 2 3)),
			res5 (fnc-menor '(1 2 3 4)),
			res6 (fnc-menor '(1 2 2 4)),
			res7 (fnc-menor '(A 1 2 4)),
			res8 (fnc-menor '(1 A 1 4)),
			res9 (fnc-menor '(1 2 A 4)),
			error1A (generar-mensaje-error :wrong-type-arg1 '< 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 '< 'A),
			t (symbol "#t"),
			f (symbol "#f")]

			(is (= res1 t))
			(is (= res2 t))
			(is (= res3 t))
			(is (= res4 t))
			(is (= res5 t))
			(is (= res6 f))
			(is (= res7 error1A))
			(is (= res8 error2A))
			(is (= res9 error2A))
		)
	)
)

(deftest fnc-mayor-test
	(testing "TEST fnc-mayor"
		(let [res1 (fnc-mayor ()),
			res2 (fnc-mayor '(1)),
			res3 (fnc-mayor '(2 1)),
			res4 (fnc-mayor '(3 2 1)),
			res5 (fnc-mayor '(4 3 2 1)),
			res6 (fnc-mayor '(4 2 2 1)),
			res7 (fnc-mayor '(4 2 1 4)),
			res8 (fnc-mayor '(A 3 2 1)),
			res9 (fnc-mayor '(3 A 2 1)),
			res10 (fnc-mayor '(3 2 A 1)),
			error1A (generar-mensaje-error :wrong-type-arg1 '< 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 '< 'A),
			t (symbol "#t"),
			f (symbol "#f")]

			(is (= res1 t))
			(is (= res2 t))
			(is (= res3 t))
			(is (= res4 t))
			(is (= res5 t))
			(is (= res6 f))
			(is (= res7 f))
			(is (= res8 error1A))
			(is (= res9 error2A))
			(is (= res10 error2A))
		)
	)
)

(deftest fnc-mayor-o-igual-test
	(testing "TEST fnc-mayor-o-igual"
		(let [res1 (fnc-mayor-o-igual ()),
			res2 (fnc-mayor-o-igual '(1)),
			res3 (fnc-mayor-o-igual '(2 1)),
			res4 (fnc-mayor-o-igual '(3 2 1)),
			res5 (fnc-mayor-o-igual '(4 3 2 1)),
			res6 (fnc-mayor-o-igual '(4 2 2 1)),
			res7 (fnc-mayor-o-igual '(4 2 1 4)),
			res8 (fnc-mayor-o-igual '(A 3 2 1)),
			res9 (fnc-mayor-o-igual '(3 A 2 1)),
			res10 (fnc-mayor-o-igual '(3 2 A 1)),
			error1A (generar-mensaje-error :wrong-type-arg1 '< 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 '< 'A),
			t (symbol "#t"),
			f (symbol "#f")]

			(is (= res1 t))
			(is (= res2 t))
			(is (= res3 t))
			(is (= res4 t))
			(is (= res5 t))
			(is (= res6 t))
			(is (= res7 f))
			(is (= res8 error1A))
			(is (= res9 error2A))
			(is (= res10 error2A))
		)
	)
)

(deftest fnc-menor-o-igual-test
	(testing "TEST fnc-menor-o-igual"
		(let [res1 (fnc-menor-o-igual ()),
			res2 (fnc-menor-o-igual '(1)),
			res3 (fnc-menor-o-igual '(2 1)),
			res4 (fnc-menor-o-igual '(1 2 3)),
			res5 (fnc-menor-o-igual '(4 3 2 1)),
			res6 (fnc-menor-o-igual '(1 2 2 3)),
			res7 (fnc-menor-o-igual '(4 2 1 4)),
			res8 (fnc-menor-o-igual '(A 3 2 1)),
			res9 (fnc-menor-o-igual '(3 A 2 1)),
			res10 (fnc-menor-o-igual '(3 2 A 1)),
			error1A (generar-mensaje-error :wrong-type-arg1 '< 'A),
			error2A (generar-mensaje-error :wrong-type-arg2 '< 'A),
			t (symbol "#t"),
			f (symbol "#f")]

			(is (= res1 t))
			(is (= res2 t))
			(is (= res3 f))
			(is (= res4 t))
			(is (= res5 f))
			(is (= res6 t))
			(is (= res7 f))
			(is (= res8 error1A))
			(is (= res9 error2A))
			(is (= res10 error2A))
		)
	)
)

(deftest evaluar-escalar-test
	(testing "TEST evaluar-escalar"
		(let [res1 (evaluar-escalar 32 '(x 6 y 11 z "hola")),
			res2 (evaluar-escalar "hola" '(x 6 y 11 z "hola")),
			res3 (evaluar-escalar 'y '(x 6 y 11 z "hola")),
			res4 (evaluar-escalar 'z '(x 6 y 11 z "hola")),
			res5 (evaluar-escalar 'n '(x 6 y 11 z "hola")),
			error-unbound (generar-mensaje-error :unbound-variable 'n)]

			(is (= res1 '(32 (x 6 y 11 z "hola"))))
			(is (= res2 '("hola" (x 6 y 11 z "hola"))))
			(is (= res3 '(11 (x 6 y 11 z "hola"))))
			(is (= res4 '("hola" (x 6 y 11 z "hola"))))
			(is (= res5 (list error-unbound '(x 6 y 11 z "hola"))))
		)
	)
)

(deftest evaluar-define-test
	(testing "TEST evaluar-define"
		(let [res1 (evaluar-define '(define x 2) '(x 1)),
			res2 (evaluar-define '(define (f x) (+ x 1)) '(x 1)),
			res3 (evaluar-define '(define) '(x 1)),
			error-missing3 (generar-mensaje-error :missing-or-extra 'define '(define)),
			res4 (evaluar-define '(define x) '(x 1)),
			error-missing4 (generar-mensaje-error :missing-or-extra 'define '(define x)),
			res5 (evaluar-define '(define x 2 3) '(x 1)),
			error-missing5 (generar-mensaje-error :missing-or-extra 'define '(define x 2 3)),
			res6 (evaluar-define '(define ()) '(x 1)),
			error-missing6 (generar-mensaje-error :missing-or-extra 'define '(define ())),
			res7 (evaluar-define '(define () 2) '(x 1)),
			error-badvariable7 (generar-mensaje-error :bad-variable 'define '(define () 2)),
			res8 (evaluar-define '(define 2 x) '(x 1)),
			error-badvariable8 (generar-mensaje-error :bad-variable 'define '(define 2 x)),
			unspecified (symbol "#<unspecified>")]

			(is (= res1 (list unspecified '(x 2))))
			(is (= res2 (list unspecified '(x 1 f (lambda (x) (+ x 1))))))
			(is (= res3 (list error-missing3 '(x 1))))
			(is (= res4 (list error-missing4 '(x 1))))
			(is (= res5 (list error-missing5 '(x 1))))
			(is (= res6 (list error-missing6 '(x 1))))
			(is (= res7 (list error-badvariable7 '(x 1))))
			(is (= res8 (list error-badvariable8 '(x 1))))
		)
	)
)

(deftest evaluar-if-test
	(testing "TEST evaluar-if"
		(let [res1 (evaluar-if '(if 1 2) '(n 7)),
			res2 (evaluar-if '(if 1 n) '(n 7)),
			res3 (evaluar-if '(if 1 n 8) '(n 7)),
			res4 (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f"))),
			res5 (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f"))),
			res6 (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f"))),
			res7 (evaluar-if '(if) '(n 7)),
			error-missing7 (generar-mensaje-error :missing-or-extra 'if '(if)),
			res8 (evaluar-if '(if 1) '(n 7)),
			error-missing8 (generar-mensaje-error :missing-or-extra 'if '(if 1)),
			unspecified (symbol "#<unspecified>")]

			(is (= res1 (list 2 '(n 7))))
			(is (= res2 (list 7 '(n 7))))
			(is (= res3 (list 7 '(n 7))))
			(is (= res4 (list unspecified (list 'n 7 (symbol "#f") (symbol "#f")))))
			(is (= res5 (list 8 (list 'n 7 (symbol "#f") (symbol "#f")))))
			(is (= res6 (list unspecified (list 'n 9 (symbol "#f") (symbol "#f")))))
			(is (= res7 (list error-missing7 '(n 7))))
			(is (= res8 (list error-missing8 '(n 7))))

		)
	)
)

(deftest evaluar-or-test
	(testing "TEST evaluar-or"
		(let [res1 (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))),
			res2 (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))),
			res3 (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))),
			res4 (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))),
			res5 (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))),
			t (symbol "#t"),
			f (symbol "#f"),
			res1-expected (list f (list f f t t)),
			res2-expected (list t (list f f t t)),
			res3-expected (list 7 (list f f t t)),
			res4-expected (list 5 (list f f t t)),
			res5-expected (list f (list f f t t))]

			(is (= res1 res1-expected))
			(is (= res2 res2-expected))
			(is (= res3 res3-expected))
			(is (= res4 res4-expected))
			(is (= res5 res5-expected))
		)
	)
)

(deftest evaluar-set!-test
	(testing "TEST evaluar-set!"
		(let [res1 (evaluar-set! '(set! x 1) '(x 0)),
			res2 (evaluar-set! '(set! x 1) '()),
			res3 (evaluar-set! '(set! x) '(x 0)),
			res4 (evaluar-set! '(set! x 1 2) '(x 0)),
			res5 (evaluar-set! '(set! 1 2) '(x 0)),
			unspecified (symbol "#<unspecified>"),
			error-unbound (generar-mensaje-error :unbound-variable 'x),
			error-missing3 (generar-mensaje-error :missing-or-extra 'set! '(set! x)),
			error-missing4 (generar-mensaje-error :missing-or-extra 'set! '(set! x 1 2)),
			error-badvariable (generar-mensaje-error :bad-variable 'set! 1),
			res1-expected (list unspecified '(x 1)),
			res2-expected (list error-unbound '()),
			res3-expected (list error-missing3 '(x 0)),
			res4-expected (list error-missing4 '(x 0)),
			res5-expected (list error-badvariable '(x 0))]

			(is (= res1 res1-expected))
			(is (= res2 res2-expected))
			(is (= res3 res3-expected))
			(is (= res4 res4-expected))
			(is (= res5 res5-expected))
		)
	)
)