(define-structure (json-object data))
(define-structure (json-array data))
(define (json-empty? x) (eq? x 'json-empty))
(define (json-null? x) (eq? x 'json-null))
(define (json-true? x) (eq? x 'json-true))
(define (json-false? x) (eq? x 'json-false))

(define (parse-empty bip)
  (and (eof-object? (lookahead-u8 bip))
       (get-u8 bip)
       'json-empty))

(define-syntax |char n| (identifier-syntax 110))
(define-syntax |char u| (identifier-syntax 117))
(define-syntax |char l| (identifier-syntax 108))
(define-syntax |char f| (identifier-syntax 102))
(define-syntax |char a| (identifier-syntax 97))
(define-syntax |char s| (identifier-syntax 115))
(define-syntax |char e| (identifier-syntax 101))
(define-syntax |char t| (identifier-syntax 116))
(define-syntax |char r| (identifier-syntax 114))
(define-syntax |char ,| (identifier-syntax 44))
(define-syntax |char :| (identifier-syntax 58))
(define-syntax |char newline| (identifier-syntax 10))
(define-syntax |char return| (identifier-syntax 13))
(define-syntax |char space| (identifier-syntax 32))

(define (parse-null bip)
  (and (eq? |char n| (lookahead-u8 bip))
       (get-u8 bip)
       (eq? |char u| (get-u8 bip))
       (eq? |char l| (get-u8 bip))
       (eq? |char l| (get-u8 bip))
       'json-null))

(define (parse-false bip)
  (and (eq? |char f| (lookahead-u8 bip))
       (get-u8 bip)
       (eq? |char a| (get-u8 bip))
       (eq? |char l| (get-u8 bip))
       (eq? |char s| (get-u8 bip))
       (eq? |char e| (get-u8 bip))
       'json-false))

(define (parse-true bip)
  (and (eq? |char t| (lookahead-u8 bip))
       (get-u8 bip)
       (eq? |char r| (get-u8 bip))
       (eq? |char u| (get-u8 bip))
       (eq? |char e| (get-u8 bip))
       'json-true))

(define (parse-comma bip)
  (and (eq? |char ,| (lookahead-u8 bip))
       (get-u8 bip)
       #t))

(define (parse-array-start bip)
  (and (eq? 91 (lookahead-u8 bip))
       (get-u8 bip)
       #t))

(define (parse-array-end bip)
  (and (eq? 93 (lookahead-u8 bip))
       (get-u8 bip)
       #t))

(define (parse-object-start bip)
  (and (eq? 123 (lookahead-u8 bip))
       (get-u8 bip)
       #t))

(define (parse-object-end bip)
  (and (eq? 125 (lookahead-u8 bip))
       (get-u8 bip)
       #t))

(define (parse-colon bip)
  (and (eq? |char :| (lookahead-u8 bip))
       (get-u8 bip)
       #t))

(define (parse-padding* bip)
  (define (parse-padding bip)
    (let ([c (lookahead-u8 bip)])
      (and (or (eq? c |char newline|)
               (eq? c |char return|)
               (eq? c |char space|))
           (get-u8 bip)
           #t)))

  (let loop () (and (parse-padding bip) (loop))))

(define (parse-string bip)
  (and (eq? 34 (lookahead-u8 bip))
       (eq? 34 (get-u8 bip))
       (call/1cc
         (lambda (k)
           (utf8->string
             (call-with-bytevector-output-port
               (lambda (bop)
                 (let loop ([escaped #f])
                   (let ([x (get-u8 bip)])
                     (cond
                       [(and (eq? 92 x) (not escaped)) (put-u8 bop x) (loop #t)]
                       [(eq? 10 x) (k #f)]
                       [(and (eq? 34 x) (not escaped))]
                       [else (put-u8 bop x) (loop #f)]))))))))))

(define (parse-number bip)
  (and (case (lookahead-u8 bip)
         [(45 48 49 50 51 52 53 54 55 56 57) #t]
         [else #f])
       (string->number
         (utf8->string
           (call-with-bytevector-output-port
             (lambda (bop)
               (let loop ()
                 (let ([x (get-u8 bip)])
                   (unless (eof-object? x)
                     (put-u8 bop x)
                     (case (lookahead-u8 bip)
                       [(10 13 32 44 58 93 125) #t]
                       [else (loop)]))))))))))

(define (parse-array bip)
  (let ([result (and (parse-array-start bip)
                     (let ([x (parse-json-term bip)])
                       (if x
                           (let loop ([xs (list x)])
                             (parse-padding* bip)
                             (if (parse-comma bip)
                                 (let ([x (parse-json-term bip)])
                                   (and x (loop (cons x xs))))
                                 (and (parse-array-end bip) xs)))
                           (and (parse-array-end bip) '()))))])
    (and result
         (make-json-array (reverse result)))))

(define (parse-key-value-pair bip)
  (call/1cc
    (lambda (k)
      (let ([key (parse-string bip)])
        (unless key (k #f))
        (parse-padding* bip)
        (unless (parse-colon bip) (k #f))
        (parse-padding* bip)
        (let ([value (parse-json-term bip)])
          (and value
               (cons key value)))))))

(define (parse-object bip)
  (let ([result (call/1cc
                  (lambda (k)
                    (and (parse-object-start bip)
                         (let loop ([result '()])
                           (parse-padding* bip)
                           (let ([k/v (parse-key-value-pair bip)])
                             (parse-padding* bip)
                             (cond
                               [(parse-object-end bip)
                               (if k/v (cons k/v result) result)]
                               [(and k/v (parse-comma bip))
                               (loop (cons k/v result))]
                               [else (k #f)]))))))])
    (and result
         (make-json-object (reverse result)))))

(define (parse-json-term bip)
  (parse-padding* bip)
  (or (parse-number bip)
      (parse-string bip)
      (parse-empty bip)
      (parse-null bip)
      (parse-false bip)
      (parse-true bip)
      (parse-array bip)
      (parse-object bip)))

(define get-json parse-json-term)

(define (json-key-value-pair->scheme key/value)
  (let ([key (car key/value)] [value (cdr key/value)])
    `(,(string->symbol key) . ,(json->scheme value))))

(define (json->scheme x)
  (cond
    [(number? x) x]
    [(string? x) x]
    [(json-empty? x) '()]
    [(json-null? x) '()]
    [(json-true? x) #t]
    [(json-false? x) #f]
    [(json-array? x) (map json->scheme (json-array-data x))]
    [(json-object? x) (map json-key-value-pair->scheme (json-object-data x))]
    [else (error 'json->scheme "Invalid JSON structure")]))

(define (json-number->string n)
  (and (number? n)
       (number->string n)))

(define (json-string->string x)
  (and (string? x)
       (format #f "~s" x)))

(define (json-null->string x)
  (and (json-null? x)
       "null"))

(define (json-true->string x)
  (and (json-true? x)
       "true"))

(define (json-false->string x)
  (and (json-false? x)
       "false"))

(define (json-empty->string x)
  (and (json-empty? x)
       ""))

(define (json-array->string x)
  (and (json-array? x)
        (let ([x (json-array-data x)])
          (if (null? x)
              "[]"
              (apply
                string-append
                `("[" ,(json->string (car x))
                      ,@(map (lambda (x) (string-append "," (json->string x)))
                            (cdr x))
                      "]"))))))

(define (json-key/value->string x)
  (string-append "\"" (car x) "\":" (json->string (cdr x))))

(define (json-object->string x)
  (and (json-object? x)
       (let ([x (json-object-data x)])
         (if (null? x)
             "{}"
             (apply
               string-append
               `("{" ,(json-key/value->string (car x))
                     ,@(map (lambda (x)
                             (string-append "," (json-key/value->string x)))
                           (cdr x))
                     "}"))))))

(define (json->string x)
  (or (json-number->string x)
      (json-string->string x)
      (json-null->string x)
      (json-true->string x)
      (json-false->string x)
      (json-empty->string x)
      (json-array->string x)
      (json-object->string x)))

(define (json-key/value? x)
  (and (pair? x)
       (string? (car x))
       (json? (cdr x))))

(define (json? x)
  (cond
    [(json-array? x) (for-all json? (json-array-data x))]
    [(json-object? x) (for-all json-key/value? (json-object-data x))]
    [else
      (or (number? x)
          (string? x)
          (json-true? x)
          (json-false? x)
          (json-null? x))]))
