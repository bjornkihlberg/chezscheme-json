(module json (get-json)
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

(let ([json-document ""])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "5"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with = (json:get-json bip) 5)
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

(let ([json-document "\"123e\\n5\""])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with string=? (json:get-json bip) "123e\\n5")
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
                          '#(json-array ()))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[1,2,3]"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array (1 2 3)))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[[\"hello\"]]"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array (#(json-array ("hello")))))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[1,2,[], [\"hello\"]]"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array (1 2 #(json-array ()) #(json-array ("hello")))))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "[1,2,[], [\"hello\"]]  7"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-array (1 2 #(json-array ()) #(json-array ("hello")))))
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
                          '#(json-object ()))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "{\"hey\": 5}"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object (("hey" . 5))))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "     {\"hey\" :   5}   \n\n \n"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object (("hey" . 5))))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "     {\"hey\" :   5}   \n\n 5\n"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object (("hey" . 5))))
      (assert-with = (json:get-json bip) 5)
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(let ([json-document "{\"hey\":{}}"])
  (call-with-port
    (open-bytevector-input-port (string->utf8 json-document))
    (lambda (bip)
      (assert-with equal? (json:get-json bip)
                          '#(json-object (("hey" . #(json-object ())))))
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
                          '#(json-object (("hey" . #(json-object (("x" . #(json-array (1 json-null 3))))))
                                          ("yo" . #(json-object ())))))
      (assert-with symbol=? (json:get-json bip) 'json-empty))))

(display "All tests passed!\n")
