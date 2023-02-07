(module json (string->json
              make-json-array
              make-json-object)
  (include "json.impl.scm"))

(import (prefix json json:))

(define-syntax assert-with
  (syntax-rules ()
    [(_ comparison a b)
      (let ([actual a]
            [expected b])
        (guard (e [else (format #t "~a, namely: (~a ~s ~s)\n" (condition-message e) 'comparison actual expected)
                        (exit 1)])
          (assert (comparison actual expected))))]))

(display "Running tests...\n")

(assert-with equal? (json:string->json "123e5") 12300000.0)
(assert-with equal? (json:string->json "  123e5 ") 12300000.0)
(assert-with equal? (json:string->json "  1 23e5 ") 1)

(assert-with equal? (json:string->json "\"123e\\n5\"") "123e\\n5")
(assert-with equal? (json:string->json "  \"123e\\\"5\" ") "123e\\\"5")
(assert-with equal? (json:string->json "  \"knatte\" 23e5 ") "knatte")

(assert-with equal? (json:string->json "[]") (json:make-json-array (list)))
(assert-with equal?
  (json:string->json "[1,2,3]")
  (json:make-json-array (list 1 2 3)))
(assert-with equal?
  (json:string->json "[[\"hello\"]]")
  (json:make-json-array (list (json:make-json-array '("hello")))))
(assert-with equal?
  (json:string->json "[1,2,[], [\"hello\"]]")
  (json:make-json-array
    (list
      1
      2
      (json:make-json-array '())
      (json:make-json-array '("hello")))))
(assert-with equal? (json:string->json "[1,2,[], [\"hello\"]]  7") (json:make-json-array `(1 2 ,(json:make-json-array '()) ,(json:make-json-array '("hello")))))
(assert-with equal?
  (json:string->json "  [   1 ,2 [], [\"hello\"]]  ")
  #f)
(assert-with equal?
  (json:string->json "  [   1 ,2, [], [\"hello\"]  ")
  #f)

(assert-with equal? (json:string->json "{}") (json:make-json-object (list)))
(assert-with equal?
  (json:string->json "{\"hey\": 5}")
  (json:make-json-object (list (cons "hey" 5))))
(assert-with equal?
  (json:string->json "     {\"hey\" :   5}   \n\n \n")
  (json:make-json-object (list (cons "hey" 5))))
(assert-with equal?
  (json:string->json "     {\"hey\" :   5}   \n\n 5\n")
  (json:make-json-object '(("hey" . 5))))
(assert-with equal?
  (json:string->json "{\"hey\":{}}")
  (json:make-json-object
    (list (cons "hey" (json:make-json-object (list))))))
(assert-with equal? (json:string->json "{\"hey\":{}{}}") #f)
(assert-with equal?
  (json:string->json "{\"hey\":{\"x\":[1,null,3]},\"yo\":{}}")
  (json:make-json-object
    (list
      (cons
        "hey"
        (json:make-json-object
          (list (cons "x" (json:make-json-array (list 1 'json-null 3))))))
      (cons "yo" (json:make-json-object (list))))))
(assert-with equal?
  (json:string->json "{\"hey\":{\"x\":[1,null,3]},\"yo\":{}}")
  (json:make-json-object
    `(("hey"
        .
        ,(json:make-json-object
            `(("x" . ,(json:make-json-array (list 1 'json-null 3))))))
        ("yo" . ,(json:make-json-object '())))))

(display "All tests passed!\n")
