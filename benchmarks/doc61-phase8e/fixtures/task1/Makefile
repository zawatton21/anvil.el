EMACS ?= emacs

.PHONY: check

check:
	$(EMACS) -Q --batch -L . \
		-l strutil-core.el \
		-l strutil-format.el \
		-l tests/visible-test.el \
		-f ert-run-tests-batch-and-exit
