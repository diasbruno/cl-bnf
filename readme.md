# cl-bnf
[![CI](https://github.com/diasbruno/cl-bnf/actions/workflows/ci.yml/badge.svg)](https://github.com/diasbruno/cl-bnf/actions/workflows/ci.yml)

A simple BNF.

`(:char . #\a)`     matches the given char.

`(:string . "abc")` matches the given string.

`(:and exp ...)`    all expressions must match.

`(:or exp ...)`     returns the first expression that matches.

`(:* . exp)`        collect all strings matches the expression, if any.

`(:? . exp)`        optional match.

`#'some-function`   execute the function with the current char. must return boolean.

`and` can also be written in a form of sequence `rule-c := rule-a rule-b`.

`or` can also be written using `:/`, for example: `rule-c := rule-a :/ rule-b`.

## rules & grammars

You can define a single rule using `define-rule`

```lisp
(cl-bnf:define-rule word (:* . #'alpha-char-p) :call #'stringify)
```

...or using the `define-grammar`

```lisp
(cl-bnf:define-grammar (abc . parser)
   abc := #\a #\b #\c
   cba := "cba"
   abc-cba := abc :/ cba
   parser := abc-cba)

(abc "abc") ;; (#\a #\b #\c)
(abc "cba") ;; "cba"
```

#### transformations

- `:call` apply a function to the results using `funcall`.
- `:apply` apply a function to the results using `apply`.
- `:tag` return a cons like `(cons TAG RESULTS)`.
- If none where specified, it return all matches.

## License

This project is released under the MIT License.

See `license`.
