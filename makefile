run-tests:
	sbcl --non-interactive --load tests-runner.lisp

run-examples:
	sbcl --non-interactive --load example-runner.lisp
