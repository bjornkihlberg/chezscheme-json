#!chezscheme

(library (stream)
  (export
    stream
    stream-cons*
    stream-pair?
    stream-null?
    stream-car
    stream-cdr
    stream-snoc
    stream-map
    stream-for
    stream-for-each
    stream-fold-left
    generator->stream
    when-match/stream
    match/stream
    match-lambda/stream
    match-system/stream
    match-system-lambda/stream
    @ ? ->)

  (import (chezscheme))

  (define-syntax stream
    (syntax-rules ()
      [(_) '()]
      [(_ e e* ...) (delay (cons e (stream e* ...)))]))

  (define-syntax stream-cons*
    (syntax-rules ()
      [(_ e) e]
      [(_ e e* ...) (delay (cons e (stream-cons* e* ...)))]))

  (define-syntax stream-for
    (syntax-rules ()
      [(_ xs x body ...) (stream-for-each (lambda (x) body ...) xs)]))

  (define (force-stream ss) (if (procedure? ss) (force ss) ss))
  (define (stream-pair? ss) (pair? (force-stream ss)))
  (define (stream-null? ss) (null? (force-stream ss)))
  (define (stream-car ss) (car (force-stream ss)))
  (define (stream-cdr ss) (cdr (force-stream ss)))

  (define (stream-snoc ss)
    (and (stream-pair? ss)
         (values (stream-car ss)
                 (stream-cdr ss))))

  (define (stream-map f ss)
    (let loop ([ss ss])
      (delay
        (if (stream-null? ss) '()
            `(,(f (stream-car ss)) . ,(loop (stream-cdr ss)))))))

  (define (stream-fold-left f x ss)
    (let loop ([x x] [ss ss])
      (if (stream-null? ss) x (loop (f x (stream-car ss)) (stream-cdr ss)))))

  (define (stream-for-each f ss)
    (let loop ([ss ss])
      (unless (stream-null? ss)
        (f (stream-car ss))
        (loop (stream-cdr ss)))))

  (define (generator->stream generator)
    (delay
      (begin
        (define return #f)
        (call/1cc (lambda (k1)
          (set! return k1)
          (generator (lambda (q)
            (call/1cc (lambda (k0)
              (return (cons q (delay (call/1cc (lambda (k2) (set! return k2) (k0 (void)))))))))))
          (return '()))))))

  (define-syntax @ (lambda (code)
    (syntax-error code "invalid context")))

  (define-syntax ? (lambda (code)
    (syntax-error code "invalid context")))

  (define-syntax -> (lambda (code)
    (syntax-error code "invalid context")))

  (define-syntax when-match/stream
    (syntax-rules (@ ? -> quasiquote unquote unquote-splicing)
      [(_ v x e* ...) (eq? (datum x) '_)
        (begin e* ...)]
      
      [(_ v x e* ...) (identifier? #'x)
        (let ([x v]) e* ...)]

      [(_ v0 (-> x e ys ...) e* ...)
        (let ([x v0]) (call-with-values (lambda () e)
            (lambda v1 (when-match/stream v1 `(,ys ...) e* ...))))]

      [(_ v (@ x0 x1) e* ...)
        (when-match/stream v x0 (when-match/stream v x1 e* ...))]

      [(_ v (@ x0 x1 x* ...) e* ...)
        (when-match/stream v x0 (when-match/stream v (@ x1 x* ...) e* ...))]

      [(_ v (? x p p* ...) e* ...)
        (when-match/stream v x (when (and p p* ...) e* ...))]

      [(_ v `,x e* ...)
        (when-match/stream v x e* ...)]

      [(_ v0 `(,@x) e* ...)
        (let ([v (if (procedure? v0) (force v0) v0)])
            (when (or (pair? v) (null? v)) (when-match/stream v x e* ...)))]

      [(_ v0 `(x x* ...) e* ...)
        (let ([v (if (procedure? v0) (force v0) v0)])
            (when (pair? v)
              (let ([v1 (car v)]
                    [v2 (cdr v)])
                (when-match/stream v1 `x (when-match/stream v2 `(x* ...) e* ...)))))]

      [(_ v0 x e* ...)
        (let ([v (if (procedure? v0) (force v0) v0)])
            (when (equal? v x) e* ...))]))

  (define-syntax match/stream
    (syntax-rules ()
      [(_ v [p e* ...] ...)
        (call/1cc (lambda (k)
          (when-match/stream v p (call-with-values (lambda () e* ...) k)) ...))]))

  (define-syntax match-lambda/stream
    (syntax-rules ()
      [(_ clause* ...) (lambda (v) (match/stream v clause* ...))]))

  (define-syntax match-system-lambda/stream
    (syntax-rules ()
      [(_ entry-point [name clause* ...] ...) (identifier? #'entry-point)
        (letrec ([name (match-lambda/stream clause* ...)] ...)
          entry-point)]))

  (define-syntax match-system/stream
    (syntax-rules ()
      [(_ value e* ...) ((match-system-lambda/stream e* ...) value)])))
