EMACS ?= emacs

.PHONY: check

check:
	$(EMACS) -Q --batch -L . \
		-l agg-parse.el \
		-l agg-summarize.el \
		-l agg-format.el \
		-l tests/visible-test.el \
		-f ert-run-tests-batch-and-exit
