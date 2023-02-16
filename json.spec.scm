(module json (get-json json->string match2 match match-clause match-clause* match-array match-object)
  (include "json.impl.scm"))

(import (prefix json json:))

(define-syntax assert-with
  (syntax-rules ()
    [(_ comparison a b)
      (let ([actual a]
            [expected b])
        (guard (e [else (format #t "~a, namely the expression: (~a ~s ~s)\n"
                                   (condition-message e)
                                   'comparison
                                   actual
                                   expected)
                        (exit 1)])
          (assert (comparison actual expected))))]))

(display "Running tests...\n")

(define t0 (current-time))

(let ()
  (import (json))
  (assert-with equal? (library-exports '(json))
    '(json-boolean?
      json-false?
      json-true?
      json-null?
      json-empty?
      json-array?
      json-object?
      json->string
      match
      get-json)))

(let ([json-document ""])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "5 false"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with = (json:get-json bip) 5)
      (assert-with symbol=? (json:get-json bip) 'json-false)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "123e5"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip) (assert-with = (json:get-json bip) 12300000.0))))

(let ([json-document "  123e5 "])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip) (assert-with = (json:get-json bip) 12300000.0))))

(let ([json-document "  1 23e5 "])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with = (json:get-json bip) 1)
      (assert-with = (json:get-json bip) 2300000.0)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "\"123e\\n5\" true"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with string=? (json:get-json bip) "123e\\n5")
      (assert-with symbol=? (json:get-json bip) 'json-true)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "  \"123e\\\"5\" "])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with string=? (json:get-json bip) "123e\\\"5")
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "  \"knatte\" 23e5 "])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with string=? (json:get-json bip) "knatte")
      (assert-with = (json:get-json bip) 2300000.0)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[]"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[1,2,3]"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array 1 2 3))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[[\"hello\"]]"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array #(json-array "hello")))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[1,2,[], [\"hello\"]]"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array 1 2 #(json-array) #(json-array "hello")))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[1,2,[], [\"hello\"]]  7"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array 1 2 #(json-array) #(json-array "hello")))
      (assert-with = (json:get-json bip) 7)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "  [   1 ,2 [], [\"hello\"]]  "])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip) (assert-with boolean=? (json:get-json bip) #f))))

(let ([json-document "  [   1 ,2, [], [\"hello\"]  "])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip) (assert-with boolean=? (json:get-json bip) #f))))

(let ([json-document "{}"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "{\"hey\": 5}"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object ("hey" . 5)))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "     {\"hey\" :   5}   \n\n \n"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object ("hey" . 5)))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "     {\"hey\" :   5}   \n\n 5\n"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object ("hey" . 5)))
      (assert-with = (json:get-json bip) 5)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "{\"hey\":{}}"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object ("hey" . #(json-object))))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "{\"hey\":{}{}}"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip) (assert-with boolean=? (json:get-json bip) #f))))

(let ([json-document "{\"hey\":{\"x\":[1,null,3]},\"yo\":{}}"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object ("hey" . #(json-object ("x" . #(json-array 1 json-null 3))))
                                         ("yo" . #(json-object))))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document (json:json->string '#(json-array))])
  (assert-with string=? json-document "[]"))

(let ([json-document (json:json->string '#(json-array 42))])
  (assert-with string=? json-document "[42]"))

(let ([json-document (json:json->string '#(json-array 1 #(json-array 2 3) json-null))])
  (assert-with string=? json-document "[1,[2,3],null]"))

(let ([json-expression 'json-null])
  (assert-with = 1337
    (json:match json-expression
      [5 42]
      [null 1337]))
  
  (assert-with = 1337
    (json:match json-expression
      [5 42]
      ['json-null 1337])))

(let ([json-expression 'json-true])
  (assert-with = 1337
    (json:match json-expression
      [false 42]
      [true 1337]))
  
  (assert-with = 1337
    (json:match json-expression
      [false 42]
      ['json-true 1337])))

(let ([json-expression 'json-false])
  (assert-with = 1337
    (json:match json-expression
      [true 42]
      [false 1337]))
  
  (assert-with = 1337
    (json:match json-expression
      [true 42]
      ['json-false 1337])))

(let ([json-expression 5])
  (assert-with = 1337
    (json:match json-expression
      [4 42]
      [5 1337])))

(let ([json-expression "huey"])
  (assert-with = 1337
    (json:match json-expression
      ["dewey" 42]
      ["huey" 1337]
      ["louie" 13])))

(assert-with equal?
  (json:match '#(json-array 1 2 3)
    [(array _ ... rest) rest])
  '#(json-array 2 3))

(assert-with =
  (json:match '#(json-array 1 2 3)
    [(array _ (? odd? _) ... (-> vector-length l)) (+ 1000 l)]
    [(array _ (? even? _) ... (-> vector-length l)) (+ 2000 l)])
  2002)

(assert-with =
  (json:match '#(json-object ("huey" . 1337) ("dewey" . 42) ("louie" . 3.14))
    [(object ("louie" . x) huey ... _) (+ huey x)])
  (+ 1337 3.14))

(assert-with equal?
  (json:match '#(json-object ("huey" . #(json-array json-true 4 2)))
    [(object ("huey" . (array false ... (@ xs (-> vector-length l))))
             ("dewey" . (array true ... (@ xs (-> vector-length l))))
             ("louie" . (array null ... (@ xs (-> vector-length l))))) #f]
    [(object ("huey" . (array false ... (@ xs (-> vector-length l))))) #f]
    [(object ("huey" . (array true ... (@ xs (-> vector-length (? even? l)))))) #f]
    [(object ("huey" . (array true ... (@ xs (-> vector-length (? odd? l)))))) (? #t) (cons l xs)])
  (cons 3 '#(json-array 4 2)))

(assert-with eq?
  (json:match2 2023)
  (void))

(assert-with symbol=?
  (guard
    (e [(syntax-violation? e)
          (assert-with string=?
            (condition-message e)
            "Expected (json:match2 value) but got")
          'error])
    (expand '(json:match2)))
  'error)

(assert-with symbol=?
  (guard
    (e [(syntax-violation? e)
          (assert-with string=?
            (condition-message e)
            "Unexpected clause 5, expected (json:match2 value [pattern expressions ...] clauses ...) but got")
          'error])
    (expand '(json:match2 2023 5)))
  'error)

(assert-with =
  (json:match2 13 [x (sub1 x)])
  12)

(assert-with =
  (json:match2 48 [x (? #t) (add1 x)])
  49)

(assert-with eq?
  (json:match2 55 [x (? #f) x])
  (void))

(assert-with symbol=?
  (json:match2 13 [_ 'success])
  'success)

(assert-with eq?
  (json:match2 14 [14 'success])
  'success)

(assert-with eq?
  (json:match2 14 [13 'success])
  (void))

(assert-with eq?
  (json:match2 "huey" ["huey" 'success])
  'success)

(assert-with eq?
  (json:match2 'json-null ['json-null 'success])
  'success)

(assert-with eq?
  (json:match2 1400 [null 'success])
  (void))

(assert-with eq?
  (json:match2 'json-null [null 'success])
  'success)

(assert-with eq?
  (json:match2 1401 [true 'success])
  (void))

(assert-with eq?
  (json:match2 'json-true [true 'success])
  'success)

(assert-with eq?
  (json:match2 1402 [false 'success])
  (void))

(assert-with symbol=?
  (guard
    (e [else
          (assert-with string=?
            (format (condition-message e) (condition-irritants e))
            "variable (null) is not bound")
          'error])
    (json:match2 'json-null [null null]))
  'error)

(assert-with symbol=?
  (guard
    (e [else
          (assert-with string=?
            (format (condition-message e) (condition-irritants e))
            "variable (true) is not bound")
          'error])
    (json:match2 'json-true [true true]))
  'error)

(assert-with symbol=?
  (guard
    (e [else
          (assert-with string=?
            (format (condition-message e) (condition-irritants e))
            "variable (false) is not bound")
          'error])
    (json:match2 'json-false [false false]))
  'error)

(assert-with eq?
  (json:match2 'json-false [false 'success])
  'success)

(assert-with symbol=?
  (guard
    (e [(syntax-violation? e)
          (assert-with string=?
            (condition-message e)
            "Unexpected pattern (?), expected (? predicate pattern) in")
          'error])
    (expand '(json:match2 1399 [(?) 'success])))
  'error)

(assert-with symbol=?
  (guard
    (e [(syntax-violation? e)
          (assert-with string=?
            (condition-message e)
            "Unexpected pattern (? odd?), expected (? predicate pattern) in")
          'error])
    (expand '(json:match2 1399 [(? odd?) 'success])))
  'error)

(assert-with eq?
  (json:match2 1403 [(? odd? x) (add1 x)])
  1404)

(assert-with eq?
  (json:match2 1405 [(? even? x) (add1 x)])
  (void))

(assert-with symbol=?
  (guard
    (e [(syntax-violation? e)
          (assert-with string=?
            (condition-message e)
            "Unexpected pattern (\\x40; x), expected (@ pattern pattern pattern ...) in")
          'error])
    (expand '(json:match2 1399 [(@ x) 'success])))
  'error)

(assert-with eq?
  (json:match2 1397 [(@ x y) (+ x y)])
  (* 2 1397))

(assert-with eq?
  (json:match2 1396 [(@ x y z) (+ x y z)])
  (* 3 1396))

(assert-with eq?
  (json:match2 1396 [(@ x y z) (? #f) (+ x y z)]
                    [(@ x y z) (add1 (+ x y z))])
  (add1 (* 3 1396)))

(assert-with eq?
  (json:match2 "hello" [(@ x y) (eq? x y)])
  #t)

(assert-with symbol=?
  (guard
    (e [(syntax-violation? e)
          (assert-with string=?
            (condition-message e)
            "Unexpected pattern (-> x), expected (-> procedure pattern) in")
          'error])
    (expand '(json:match2 1393 [(-> x) 'success])))
  'error)

(assert-with symbol=?
  (guard
    (e [(syntax-violation? e)
          (assert-with string=?
            (condition-message e)
            "Unexpected pattern (-> a b c), expected (-> procedure pattern) in")
          'error])
    (expand '(json:match2 1393 [(-> a b c) 'success])))
  'error)

(assert-with eq?
  (json:match2 1395 [(-> add1 x) x])
  1396)

(assert-with eq?
  (json:match2 1395 [(@ (-> add1 x) y) (+ x y)])
  (+ 1395 1395 1))

(assert-with eq?
  (json:match2 1395 [(@ (-> add1 x) y) (? #f) (+ x y)]
                    [(-> sub1 x) x])
  1394)

(assert-with eq?
  (json:match2 '#(json-array)
    [(array) 1380])
  1380)

(assert-with eq?
  (json:match2 '#()
    [(array) 1379])
  (void))

(assert-with eq?
  (json:match2 '#(4 5)
    [(array x) x])
  (void))

(assert-with eq?
  (json:match2 4
    [(array x) x])
  (void))

(assert-with eq?
  (json:match2 '#(json-array)
    [(array x) x])
  (void))

(assert-with eq?
  (json:match2 '#(json-array 1 2 3)
    [(array x) x])
  (void))

(assert-with eq?
  (json:match2 '#(json-array 2 3 5)
    [(array x y z) (* x y z)])
  30)

(assert-with eq?
  (json:match2 '#(json-array 2 3 5)
    [(array x ... (-> vector-length l)) (* x l)])
  6)

(assert-with equal?
  (json:match2 '#(json-array 2 3 5)
    [(array _ ... rest) rest])
  '#(json-array 3 5))

(define t1 (current-time))

(display "All tests passed!\n")
(format #t "~s\n" (time-difference t1 t0))
