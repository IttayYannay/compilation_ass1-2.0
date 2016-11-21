(load "pc.scm")
(load "compiler.scm")
(load "parser.so")

(define s1 "Yui")
(define test_string "(+ 1 a (- 3 4 5))")

(newline)

(display "Mayer     :")
(display (test-string <sexpr> test_string))

(newline)

(display "Our_Parser:")
(display (test-string <Sexpr> test_string))