.PHONY: test test-all lint byte-compile clean sync-toml-filters check-toml-filters-sync

# make on MSYS strips TEMP/TMP from the environment, which makes
# `make-temp-file' in the subprocess fall back to `c:/' (unwritable)
# and fail every test that touches a temp file.  Force sane defaults.
export TMPDIR ?= /tmp
export TEMP   ?= /tmp
export TMP    ?= /tmp

test:
	eask emacs --batch -Q -L . -L tests -l ert -l tests/anvil-test.el \
		-f ert-run-tests-batch-and-exit

test-all:
	emacs --batch --eval '(setq load-prefer-newer t)' -L . -l anvil-dev -f anvil-dev-test-run-all-batch

lint:
	eask lint package
	eask lint checkdoc

byte-compile:
	eask compile

clean:
	eask clean all

## Sync rtk-derived TOML filters -> anvil-shell-filter-builtin.el
sync-toml-filters:
	python3 scripts/sync-rtk-filters.py

check-toml-filters-sync:
	python3 scripts/sync-rtk-filters.py --check
