EMACS ?= emacs

.PHONY: check

check:
	$(EMACS) -Q --batch -L . \
		-l flow.el \
		-l flow-report.el \
		-l tests/visible-test.el \
		-f ert-run-tests-batch-and-exit
