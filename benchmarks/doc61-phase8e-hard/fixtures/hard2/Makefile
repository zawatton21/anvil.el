EMACS ?= emacs

.PHONY: check

check:
	$(EMACS) -Q --batch -L . \
		-l rq-data.el \
		-l rq-parse.el \
		-l rq-predicate.el \
		-l rq-eval.el \
		-l rq-format.el \
		-l tests/visible-test.el \
		-f ert-run-tests-batch-and-exit
