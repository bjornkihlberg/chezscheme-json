(import (json))

(define (assert-equal a b)
  (unless (equal? a b)
    (assertion-violationf 'assert-equal "~s =/= ~s" a b)))

(define-syntax describe
  (syntax-rules ()
    [(_ s body ...)
     (begin
       (format #t "  Tests: ~a ...\n" s)
       body
       ...
       (format #t "  ~a tests done!\n" s))]))

(display "Running tests...\n")

(describe
  "parsing numbers"
  (assert-equal (string->json "123e5") 12300000.0)
  (assert-equal (string->json "  123e5 ") 12300000.0)
  (assert-equal (string->json "  1 23e5 ") #f))

(describe
  "parsing strings"
  (assert-equal (string->json "\"123e\\n5\"") "123e\\n5")
  (assert-equal (string->json "  \"123e\\\"5\" ") "123e\\\"5")
  (assert-equal (string->json "  \"knatte\" 23e5 ") #f))

(describe "parsing lists"
  (assert-equal (string->json "[]") (make-json-array (list)))
  (assert-equal
    (string->json "[1,2,3]")
    (make-json-array (list 1 2 3)))
  (assert-equal
    (string->json "[[\"hello\"]]")
    (make-json-array (list (make-json-array '("hello")))))
  (assert-equal
    (string->json "[1,2,[], [\"hello\"]]")
    (make-json-array
      (list
        1
        2
        (make-json-array '())
        (make-json-array '("hello")))))
  (assert-equal (string->json "[1,2,[], [\"hello\"]]  7") #f)
  (assert-equal
    (string->json "  [   1 ,2 [], [\"hello\"]]  ")
    #f)
  (assert-equal
    (string->json "  [   1 ,2, [], [\"hello\"]  ")
    #f))

(describe "parsing objects"
  (assert-equal (string->json "{}") (make-json-object (list)))
  (assert-equal
    (string->json "{\"hey\": 5}")
    (make-json-object (list (cons "hey" 5))))
  (assert-equal
    (string->json "     {\"hey\" :   5}   \n\n \n")
    (make-json-object (list (cons "hey" 5))))
  (assert-equal
    (string->json "     {\"hey\" :   5}   \n\n 5\n")
    #f)
  (assert-equal
    (string->json "{\"hey\":{}}")
    (make-json-object
      (list (cons "hey" (make-json-object (list))))))
  (assert-equal (string->json "{\"hey\":{}{}}") #f)
  (assert-equal
    (string->json "{\"hey\":{\"x\":[1,null,3]},\"yo\":{}}")
    (make-json-object
      (list
        (cons
          "hey"
          (make-json-object
            (list (cons "x" (make-json-array (list 1 'json-null 3))))))
        (cons "yo" (make-json-object (list))))))
  (assert-equal
    (string->json "{\"hey\":{\"x\":[1,null,3]},\"yo\":{}}")
    (make-json-object
      `(("hey"
          .
          ,(make-json-object
             `(("x" . ,(make-json-array (list 1 'json-null 3))))))
         ("yo" . ,(make-json-object '()))))))

(display "All tests passed!\n")
