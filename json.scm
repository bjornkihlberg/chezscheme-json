(library (json)
  (export get-json
          json->string
          json-array?
          json-empty?
          json-false?
          json-null?
          json-object-data
          json-object?
          json-true?
          make-json-object)

  (import (chezscheme))

  (include "json.impl.scm"))
