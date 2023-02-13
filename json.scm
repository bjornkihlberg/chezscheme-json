(library (json)
  (export get-json
          json->string
          json-array?
          json-empty?
          json-false?
          json-null?
          json-object?
          json-true?)

  (import (chezscheme))

  (include "json.impl.scm"))
