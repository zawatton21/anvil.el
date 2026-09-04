EMACS ?= emacs

.PHONY: check

check:
	$(EMACS) -Q --batch -L . \
		-l tt-width.el \
		-l tt-layout.el \
		-l tt-draw.el \
		-l tt.el \
		-l tests/visible-test.el \
		-f ert-run-tests-batch-and-exit
