(library (json)
  (export bytevector->json
          get-json
          json->scheme
          json->string
          json-array-data
          json-array?
          json-empty?
          json-false?
          json-null?
          json-object-data
          json-object?
          json-true?
          json?
          make-json-array
          make-json-object
          string->json)

  (import (chezscheme))

  (include "json.impl.scm"))
