EMACS ?= emacs

.PHONY: check

check:
	$(EMACS) -Q --batch -L . \
		-l calc-tokenize.el \
		-l calc-parse.el \
		-l calc-eval.el \
		-l calc-api.el \
		-l tests/visible-test.el \
		-f ert-run-tests-batch-and-exit
