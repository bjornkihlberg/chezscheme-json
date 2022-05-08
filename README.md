# chezscheme-json

JSON library for Chez Scheme

---

## Run tests

```
scheme --script tests.ss
```

## Quickstart

- `null` are parsed as `'json-null`
- `true` are parsed as `'json-true`
- `false` are parsed as `'json-false`
- `[1, 2, 3]` are parsed as `(vector 'json-array (list 1 2 3))`
- `{"x":5,"y":7}` are parsed as `(vector 'json-object '(("x" . 5) ("y" . 7)))`
- incorrect JSON will be simply be represented by `#f`

`get-json`, `string->json` and `bytevector->json` returns a scheme structure that preserves information of the original JSON structure.

`get-json` takes a binary input port and will read until it reaches the eof object. _I don't know if this is good design because it might not provide any value and it might actually be more valuable to let it gradually pick out multiple JSON values. For example if continually listening on a message queue._

```scheme
(string->json "123") ; 123
(string->json "123e2") ; 12300.0
(string->json "[1, 2, 3]") ; #(json-array (list 1 2 3))
```

**myjson.json**:

```json
["huey", "dewey", "louie"]
```

```scheme
(call-with-port
    (open-file-input-port "./myjson.json")
    get-json) ; (vector 'json-array '("huey" "dewey" "louie"))
```

`json->scheme` can be used to strip a parsed datastructure from JSON specific information.

- `'json-null` is translated to `'()`
- `'json-true` is translated to `#t`
- `'json-false` is translated to `#f`
- `(vector 'json-array (list 1 2 3))` is translated to `(list 1 2 3)`
- `(vector 'json-object '(("x" . 5) ("y" . 7)))` is translated to `'((x . 5) (y . 7)))`
