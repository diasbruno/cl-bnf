ENV?=development

## run through roswell
LISP=ros run --

LISPFLAGS=--quit --non-interactive

.PHONY: tests
tests:
	ENV=$(ENV) \
	$(LISP) \
	$(LISPFLAGS) --load tests-runner.lisp

run-examples:
	$(LISP) --non-interactive --load example-runner.lisp
