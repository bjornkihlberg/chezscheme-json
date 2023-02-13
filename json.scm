(library (json)
  (export get-json
          json->string
          json-object?
          json-array?
          json-empty?
          json-null?
          json-true?
          json-false?
          json-boolean?)

  (import (chezscheme))

  (include "json.impl.scm"))
