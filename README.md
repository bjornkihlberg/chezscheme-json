# chezscheme-json

JSON library for Chez Scheme

---

## Run tests

```
scheme --script tests.ss
```

## Quickstart

- `null` are parsed as symbol `'json-null`
- `true` are parsed as symbol `'json-true`
- `false` are parsed as symbol `'json-false`
- `[1, 2, 3]` are parsed as `(vector 'json-array (list 1 2 3))`
- `{"x":5,"y":7}` are parsed as `(vector 'json-object '(("x" . 5) ("y" . 7)))`
- incorrect JSON will be simply be represented by `#f`

`get-json`, `string->json` and `bytevector->json` returns a scheme structure that preserves information of the original JSON structure. `get-json` takes a binary input port and will read until it reaches the eof object.

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
