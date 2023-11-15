ENV?=development

## run through roswell
LISP?=sbcl

LISPFLAGS=--quit --non-interactive

run-examples:
	$(LISP) --non-interactive --load example-runner.lisp

.PHONY: tests
tests:
	ENV=$(ENV) \
	$(LISP) \
	$(LISPFLAGS) --load tests-runner.lisp
