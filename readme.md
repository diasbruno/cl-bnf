# cl-bnf [![Build Status](https://travis-ci.org/diasbruno/jsc.svg?branch=master)](https://travis-ci.org/diasbruno/cl-bnf)

A simple BNF.

## tokens & terms

- `(:char #\a)` test the next element on the stream with the given char.
- `(:one #'fn)` test using a function to get the next char in the stream.
- `(:string "abc")` test if the string is in the stream.
- `(:and exp ...)` collect many expressions.
- `(:or exp ...)` collect the expression that matches.
- `(:many exp)` collect all matches of the expression, if any.
- `(:maybe exp)` turns the failed test into a valid one.

## rules & grammars

You can define a single rule using `define-rule`

```lisp
(define-rule word (:many (:one #'alpha-char-p)) :call #'stringify)
```

...or using the `define-grammar`

```lisp
(define-grammar json-number
   decimal-number := (:many (:one #'numeric-char-p))

   real-number := (:or (:and #'decimal-number
                             (:char #\.)
                             #'decimal-number)
                       (:and #'decimal-number
                             (:char #\.)))

   signed-part := (:or (:char #\+) (:char #\-))

   exp-chars := (:or (:char #\e) (:char #\E))

   exp-part := (:and #'exp-chars
                     #'signed-part
                     (:maybe #'decimal-number))

   numeric := (:or #'real-number #'decimal-number))

   number-literal := (:or (:and #'numeric
                                #'exp-part)
                          #'numeric) :call (lambda (matches)
                                             (cons :number (stringify matches)))

(parse #number-literal "1e3")
```

#### Using transformations:

- `:call` apply a function to the results using `funcall`.
- `:apply` apply a function to the results using `apply`.
- `:tag` return a cons like `(cons TAG RESULTS)`.
- If none where specified, it return a list of all matches.

## License

This project is released under the MIT License.

See `license`.
