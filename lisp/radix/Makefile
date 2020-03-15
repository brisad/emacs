.POSIX:
EMACS = emacs

compile: radix.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

check: radix.elc
	$(EMACS) -Q -batch -L . -l radix.elc -f ert-run-tests-batch-and-exit

clean:
	rm -f radix.elc
