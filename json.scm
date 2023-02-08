(library (json)
  (export get-json
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
          make-json-object)

  (import (chezscheme))

  (include "json.impl.scm"))
