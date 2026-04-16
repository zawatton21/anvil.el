.PHONY: test test-all lint byte-compile clean

test:
	eask test ert tests/anvil-test.el

test-all:
	emacs --batch -L . -l anvil-dev -f anvil-dev-test-run-all-batch

lint:
	eask lint package
	eask lint checkdoc

byte-compile:
	eask compile

clean:
	eask clean all
