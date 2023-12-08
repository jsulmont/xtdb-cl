LISP ?= sbcl

all: test

clean:
	$(RM) *.fasl src/*.fasl
run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load xtdb-cl.asd \
		--eval '(ql:quickload :xtdb-cl)' \
		--eval '(asdf:make :xtdb-cl)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
