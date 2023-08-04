#!chezscheme

(library (json)
  (export
    try-get-json-string-token)

  (import (chezscheme))

  (define (get-json-string-content-hex-digit bip)
    (let ([x (integer->char (get-u8 bip))])
      (case (char-downcase x)
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f) x]
        [else (assertion-violationf 'get-json-string-content-hex-digit "Unexpected character ~a in 4-hex escape token" x)])))

  (define (get-json-string-content-token bip)
    (let ([x (integer->char (lookahead-u8 bip))])
      (case x
        [(#\linefeed #\return) (assertion-violation 'get-json-string-content-token "Control characters are not allowed in string")]
        [#\\
          (get-u8 bip)
          (let ([x (integer->char (get-u8 bip))])
            (case x
              [(#\" #\\ #\/ #\b #\f #\n #\r #\t) (string #\\ x)]
              [#\u
                (let* ([a (get-json-string-content-hex-digit bip)]
                      [b (get-json-string-content-hex-digit bip)]
                      [c (get-json-string-content-hex-digit bip)]
                      [d (get-json-string-content-hex-digit bip)])
                  (string #\\ #\u a b c d))]
              [else (assertion-violationf 'get-json-string-content-token "Unexpected character ~a in escape token" x)]))]
        [else (get-u8 bip) (string x)])))

  (define (try-get-json-string-token bip)
    (let ([x (lookahead-u8 bip)])
      (if (eof-object? x) #f
          (case (integer->char x)
            [#\"
              (get-u8 bip)
              (call-with-string-output-port (lambda (top)
                (do () ((eq? (integer->char (lookahead-u8 bip)) #\") (get-u8 bip))
                  (put-string top (get-json-string-content-token bip)))))]
            [else #f])))))
