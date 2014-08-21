PROJECT = annex

# dependencies

DEPS = 

include erlang.mk

repl: all bin/start
	@bin/start annex

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: repl
