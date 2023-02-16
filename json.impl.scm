(define (json-array? x)
  (and (vector? x)
       (positive? (vector-length x))
       (eq? (vector-ref x 0) 'json-array)))

(define (json-object? x)
  (and (vector? x)
       (positive? (vector-length x))
       (eq? (vector-ref x 0) 'json-object)))

(define (json-empty? x) (eq? x 'json-empty))
(define (json-null? x) (eq? x 'json-null))
(define (json-true? x) (eq? x 'json-true))
(define (json-false? x) (eq? x 'json-false))
(define (json-boolean? x) (or (eq? x 'json-true)
                              (eq? x 'json-false)))

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
  (define (make-json-array items)
    (define v (make-vector (add1 (length items))))
    (vector-set! v 0 'json-array)
    (do ([i (sub1 (vector-length v)) (sub1 i)]
         [items items (cdr items)])
        ((zero? i) v)
      (vector-set! v i (car items))))

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
    (and result (make-json-array result))))

(define (parse-key-value-pair bip)
  (call/1cc (lambda (k)
    (let ([key (parse-string bip)])
      (unless key (k #f))
      (parse-padding* bip)
      (unless (parse-colon bip) (k #f))
      (parse-padding* bip)
      (let ([value (parse-json-term bip)])
        (and value
             (cons key value)))))))

(define (parse-object bip)
  (define (make-json-object items)
    (define v (make-vector (add1 (length items))))
    (vector-set! v 0 'json-object)
    (do ([i (sub1 (vector-length v)) (sub1 i)]
         [items items (cdr items)])
        ((zero? i) v)
      (vector-set! v i (car items))))

  (let ([result (call/1cc (lambda (k)
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
    (and result (make-json-object result))))

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
       (if (= (vector-length x) 1)
           "[]"
           (call-with-string-output-port (lambda (top)
             (put-string top "[")
             (put-string top (json->string (vector-ref x 1)))
             (do ([i 2 (add1 i)])
                 ((>= i (vector-length x)))
               (put-char top #\,)
               (put-string top (json->string (vector-ref x i))))
             (put-string top "]"))))))

(define (json-key/value->string x)
  (string-append "\"" (car x) "\":" (json->string (cdr x))))

(define (json-object->string x)
  (and (json-object? x)
       (if (= (vector-length x) 1)
            "{}"
           (call-with-string-output-port (lambda (top)
             (put-string top "{")
             (put-string top (json-key/value->string (vector-ref x 1)))
             (do ([i 2 (add1 i)])
                 ((>= i (vector-length x)))
               (put-char top #\,)
               (put-string top (json-key/value->string (vector-ref x i))))
             (put-string top "}"))))))

(define (json->string x)
  (or (json-number->string x)
      (json-string->string x)
      (json-null->string x)
      (json-true->string x)
      (json-false->string x)
      (json-empty->string x)
      (json-array->string x)
      (json-object->string x)))

(define-syntax match-array
  (syntax-rules ()

    [(_ val patterns ks kf)
      (match-array val 1 patterns ks kf)]

    [(_ val offset () ks kf)
      (if (zero? (- offset (vector-length val))) ks kf)]

    [(_ val 1 (keyword pattern) ks kf)
      (eq? (syntax->datum #'keyword) '...)
      (match-clause val pattern ks kf)]

    [(_ val offset (keyword pattern) ks kf)
      (eq? (syntax->datum #'keyword) '...)
      (let ([v (make-vector (add1 (- (vector-length val) offset)))])
          (vector-set! v 0 'json-array)
          (do ([val-i (sub1 (vector-length val)) (sub1 val-i)]
               [v-i (sub1 (vector-length v)) (sub1 v-i)])
              ((zero? v-i)
              (match-clause v pattern ks kf))
            (vector-set! v v-i (vector-ref val val-i))))]

    [(_ val offset (pattern pattern* ...) ks kf)
      (let ([kff (lambda () kf)]) (if (positive? (- (vector-length val) offset))
          (match-clause (vector-ref val offset) pattern
            (match-array val (add1 offset) (pattern* ...) ks (kff))
            (kff))
          (kff)))]))

(define-syntax (match-object code)
  (syntax-case code ()

    [(_ val () ks kf)
      #'(if (= (vector-length val) 1) ks kf)]

    [(_ val (keyword pattern) ks kf)
      (eq? (syntax->datum #'keyword) '...)
      #'(match-clause val pattern ks kf)]

    [(_ val ((key . pattern) pattern* ...) ks kf)
      #'(let ([result (do ([i (sub1 (vector-length val)) (sub1 i)])
                          ((or (zero? i)
                               (string=? key (car (vector-ref val i))))
                          (and (positive? i)
                               (cons i (cdr (vector-ref val i))))))]
              [kff (lambda () kf)])
          (if result
              (let ([x (cdr result)])
                (match-clause x pattern
                  (let ([v (make-vector (sub1 (vector-length val)))])
                    (do ([i (sub1 (vector-length val)) (sub1 i)])
                        ((= i (car result)))
                      (vector-set! v (sub1 i) (vector-ref val i)))
                    (do ([i (sub1 (car result)) (sub1 i)])
                        ((negative? i))
                      (vector-set! v i (vector-ref val i)))
                    (match-object v (pattern* ...) ks (kff)))
                  (kff)))
              (kff)))]

    [(_ val (key pattern* ...) ks kf)
      (let ([key-string (symbol->string (syntax->datum #'key))])
        #`(match-object val ((#,key-string . key) pattern* ...) ks kf))]))

(define-syntax match-clause
  (syntax-rules ()

    [(_ val wildcard ks kf)
      (let ([wildcard (syntax->datum #'wildcard)])
        (or (eq? wildcard 'else)
            (eq? wildcard '_)
            (eq? wildcard '=>)))
      ks]

    [(_ val (keyword pattern* ...) ks kf)
      (eq? (syntax->datum #'keyword) 'object)
      (let ([kff (lambda () kf)]) (if (and (vector? val) (symbol=? (vector-ref val 0) 'json-object))
          (match-object val (pattern* ...) ks (kff))
          (kff)))]

    [(_ val (keyword pattern* ...) ks kf)
      (eq? (syntax->datum #'keyword) 'array)
      (let ([kff (lambda () kf)]) (if (and (vector? val) (symbol=? (vector-ref val 0) 'json-array))
          (match-array val (pattern* ...) ks (kff))
          (kff)))]

    [(_ val (keyword expr pattern) ks kf)
      (eq? (syntax->datum #'keyword) '->)
      (let ([x (expr val)]) (match-clause x pattern ks kf))]

    [(_ val (keyword var pattern) ks kf)
      (eq? (syntax->datum #'keyword) '@)
      (let ([kff (lambda () kf)]) (match-clause val var
          (match-clause val pattern ks (kff))
          (kff)))]

    [(_ val (keyword pred pattern) ks kf)
      (eq? (syntax->datum #'keyword) '?)
      (let ([kff (lambda () kf)]) (if (pred val)
          (match-clause val pattern ks (kff))
          (kff)))]

    [(_ val json-lit ks kf)
      (eq? (syntax->datum #'json-lit) 'null)
      (if (eq? val 'json-null) ks kf)]

    [(_ val json-lit ks kf)
      (eq? (syntax->datum #'json-lit) 'true)
      (if (eq? val 'json-true) ks kf)]

    [(_ val json-lit ks kf)
      (eq? (syntax->datum #'json-lit) 'false)
      (if (eq? val 'json-false) ks kf)]

    [(_ val lit ks kf)
      (not (symbol? (syntax->datum #'lit)))
      (if (equal? val lit) ks kf)]

    [(_ val var ks kf)
      (let ([var val]) ks)]))

(define-syntax match-clause*
  (syntax-rules ()
    [(_ val)
      (void)]

    [(_ val (pattern (keyword test) e ...) clause* ...)
      (eq? (syntax->datum #'keyword) '?)
      (let ([kff (lambda () (match-clause* val clause* ...))])
          (match-clause val pattern
            (if test (begin e ...) (kff))
            (kff)))]

    [(_ val (pattern e ...) clause* ...)
      (match-clause val pattern
          (begin e ...)
          (match-clause* val clause* ...))]))

(define-syntax match
  (syntax-rules ()
    [(_ val)
      (match-clause* val)]

    [(_ val clause* ...)
      (let ([x val]) (match-clause* x clause* ...))]))

(define-syntax (match2 code)
  (define macro (syntax->datum code))
  (define macro-name (car macro))
  (define macro-args (cdr macro))

  (define-syntax match-error
    (syntax-rules () [(_ args ...) (syntax-violation macro-name (format args ...) code)]))

  (define (match-clause-transformer val-name pattern on-match on-mismatch)
    (cond [(symbol? pattern)
            (case pattern
              [_ on-match]

              [null `(if (eq? ,val-name 'json-null) ,on-match ,on-mismatch)]
              [true `(if (eq? ,val-name 'json-true) ,on-match ,on-mismatch)]
              [false `(if (eq? ,val-name 'json-false) ,on-match ,on-mismatch)]

              [else `(let ([,pattern ,val-name]) ,on-match)])]

          [(pair? pattern)
            (case (car pattern)
              [quote `(if (equal? ,val-name ,pattern) ,on-match ,on-mismatch)]

              [? (let ([pred/pattern (cdr pattern)])
                  (unless (= (length pred/pattern) 2)
                    (match-error "Unexpected pattern ~s, expected (? predicate pattern) in" pattern))

                  `(let ([on-mismatch-lambda (lambda () ,on-mismatch)])
                    (if (,(car pred/pattern) ,val-name)
                        ,(match-clause-transformer val-name (cadr pred/pattern)
                          on-match
                          '(on-mismatch-lambda))
                        (on-mismatch-lambda))))]

              [@ (let ([pattern* (cdr pattern)])
                  (unless (> (length pattern*) 1)
                    (match-error "Unexpected pattern ~s, expected (@ pattern pattern pattern ...) in" pattern))

                  `(let ([on-mismatch-lambda (lambda () ,on-mismatch)])
                    ,(fold-right
                      (lambda (pattern on-match)
                        (match-clause-transformer val-name pattern on-match '(on-mismatch-lambda)))
                      on-match pattern*)))]

              [-> (let ([proc/pattern (cdr pattern)])
                    (unless (= (length proc/pattern) 2)
                      (match-error "Unexpected pattern ~s, expected (-> procedure pattern) in" pattern))

                    (let ([new-val-name (gensym "val-arg")])
                      `(let ([,new-val-name (,(car proc/pattern) ,val-name)])
                        ,(match-clause-transformer new-val-name (cadr proc/pattern)
                          on-match
                          on-mismatch))))]

              [object (match-error "Unimplemented pattern (object (key . pattern) ...)")]
              [array (match-error "Unimplemented pattern (array pattern ...)")]
              [else (match-error "Unknown keyword ~s in pattern ~s" (car pattern) pattern)])]

          [(atom? pattern)
            `(if (equal? ,val-name ,pattern) ,on-match ,on-mismatch)]

          [else (match-error "Unknown pattern ~s" pattern)]))

  (define match-clause*-transformer
    (case-lambda
      [() '(void)]

      [(clause . clause*)
        (unless (pair? clause)
          (match-error
            "Unexpected clause ~s, expected (~s value [pattern expressions ...] clauses ...) but got"
            clause
            macro-name))

        (let ([pattern (car clause)]
              [?/expressions* (cdr clause)])
          (if (and (pair? ?/expressions*)
                   (pair? (car ?/expressions*))
                   (eq? (caar ?/expressions*) '?))

              `(let ([on-mismatch-lambda (lambda () ,(apply match-clause*-transformer clause*))])
                ,(match-clause-transformer 'val-arg pattern
                  `(if ,(cadar ?/expressions*)
                       (begin ,@(cdr ?/expressions*))
                       (on-mismatch-lambda))
                  '(on-mismatch-lambda)))

              (match-clause-transformer 'val-arg pattern
                `(begin ,@?/expressions*)
                (apply match-clause*-transformer clause*))))]))

  (define match-transformer
    (case-lambda
      [() (match-error "Expected (~s value) but got" macro-name)]

      [(val) '(void)]

      [(val . clause*)
        `(let ([val-arg ,val]) ,(apply match-clause*-transformer clause*))]))

  (datum->syntax #'code (apply match-transformer macro-args)))
