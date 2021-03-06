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

`get-json` takes a binary input port and will read until it reaches the end of a complete json value.

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

`json->string` applied on a JSON structure seralizes it to a string.

## Notes

I suspect the code could be cleaned up but I'm tempted to let it be and come back in a few years to see what I've learned since. I'm working on a better code formatter for Chez Scheme as well which I will apply here as soon as it's ready.
