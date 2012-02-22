VSN=2.1.0
ERLC=/usr/bin/env erlc
ERL=/usr/bin/env erl
APPDIR= $(abspath ./_build/sinan/lib/sinan-$(VSN))
SRCDIR=src
TESTDIR=test
COPYDIRS= include src test
BEAMDIR=$(APPDIR)/ebin
SMOKETEST_DIR=$(CURDIR)/smoketests
PYPATH=$(PYTHONPATH):$(SMOKETEST_DIR)
BEHAVIOURS= src/sin_task.erl src/sin_dep_resolver.erl
SINFLAGS=-s $(CURDIR) -p sinan -n $(VSN)
RSYNC_OPTIONS=-vaz --delete
.SUFFIXES: .erl .beam .yrl

vpath %.erl src test

ERL_OBJ = $(patsubst src/%.erl,$(BEAMDIR)/%.beam, $(wildcard $(SRCDIR)/*erl))
ERL_TEST_OBJ = $(patsubst test/%.erl,$(BEAMDIR)/%.beam, $(wildcard $(TESTDIR)/*erl))

all: main

setup:
	for f in $(COPYDIRS) ; do	\
		mkdir -p $(APPDIR)/$$f ; \
		rsync $(RSYNC_OPTIONS) $$f $(APPDIR); \
	done
	mkdir -p $(APPDIR)/ebin;
	rsync $(RSYNC_OPTIONS) ebin/sinan.app $(APPDIR)/ebin/sinan.app

build_behaviours: $(BEHAVIOURS)
        # make sure sin_task gets built first so its always available
	$(ERLC) -pa $(BEAMDIR) +warn_export_vars +warn_export_all \
	+warn_obsolete_guard \
	+warnings_as_errors +bin_opt_info +debug_info -W -o $(BEAMDIR) $(BEHAVIOURS)

main: setup build_behaviours ${ERL_OBJ} ${ERL_TEST_OBJ}

$(BEAMDIR)/%.beam: %.erl
	$(ERLC) -pa $(BEAMDIR) +warn_export_vars +warn_export_all \
	+warn_obsolete_guard \
	+warnings_as_errors +bin_opt_info +debug_info -W -o $(BEAMDIR) $<

build: main
	$(ERL) -pa $(BEAMDIR) -s sinan main -extra $(SINFLAGS) build

escript: main
	$(ERL) -pa $(BEAMDIR) -s sinan main -extra $(SINFLAGS) escript

cucumber: main
	$(ERL) -pa $(BEAMDIR) -s sinan main -extra $(SINFLAGS) cucumber

proper: main
	$(ERL) -pa $(BEAMDIR) -s sinan main -extra $(SINFLAGS) proper

eunit: main
	$(ERL) -pa $(BEAMDIR) -s sinan main -extra $(SINFLAGS) eunit

dialyzer: main
	$(ERL) -pa $(BEAMDIR) -s sinan main -extra $(SINFLAGS) dialyzer

run: main
	$(ERL) -pa $(BEAMDIR)

debug: main
	$(ERL) -pa $(BEAMDIR) -s debugger start

smoketests: main
	for f in $(wildcard $(SMOKETEST_DIR)/tests/*.py) ; do	\
		PYTHONPATH=$(PYPATH) python $$f ; \
	done

testall : cucumber proper eunit smoketests

gh-pages:
	./do-gh-pages

clean:
	rm -rf _build ;
	rm -rf erl_crash.dump
