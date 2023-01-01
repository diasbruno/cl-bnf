# cl-bnf
[![CI](https://github.com/diasbruno/cl-bnf/actions/workflows/ci.yml/badge.svg)](https://github.com/diasbruno/cl-bnf/actions/workflows/ci.yml)



A simple BNF.

`(:char . #\a)`     matches the given char.

`(:string . "abc")` matches the given string.

`(:and exp ...)`    all expressions must match.

`(:or exp ...)`     returns the first expression that matches.

`(:* . exp)`        collect all strings matches the expression, if any.

`(:? . exp)`        optional match.

`#'my-function`     execute the function with the current char.

`and` can also be written in a form of sequence `rule-c := rule-a rule-b`.

`or` can also be written using `:/`, for example: `rule-c := rule-a :/ rule-b`.

## rules & grammars

You can define a single rule using `define-rule`

```lisp
(define-rule word (:* . #'alpha-char-p) :call #'stringify)
```

...or using the `define-grammar`

```lisp
(define-grammar (json-number . number-literal)
   decimal-number := (:* . #'numeric-char-p)
   ;;     using a function ^

   dotted-decimal := decimal-number #\.
   ;;            and ^

   real-number := dotted-decimal decimal-number :/ dotted-decimal
   ;;                                        or ^

   signed-part := #\+ :/ #\-

   exp-chars := #\e :/ #\E

   exp-part := exp-chars signed-part (:? . decimal-number)

   numeric := real-number :/ decimal-number

   number-literal := numeric exp-part :/ numeric
   :on (lambda (matches)
         (cons :number (stringify matches)))

(json-number "1e3")
```

#### transformations

- `:call` apply a function to the results using `funcall`.
- `:apply` apply a function to the results using `apply`.
- `:tag` return a cons like `(cons TAG RESULTS)`.
- If none where specified, it return all matches.

## License

This project is released under the MIT License.

See `license`.
