VSN=2.0.23a
ERLC=/usr/local/bin/erlc
ERL=/usr/local/bin/erl
APPDIR= $(abspath ./_build/sinan/lib/sinan-$(VSN))
SRCDIR=src
TESTDIR=test
COPYDIRS= include src test
BEAMDIR=$(APPDIR)/ebin
SMOKETEST_DIR=$(CURDIR)/smoketests
PYPATH=$(PYTHONPATH):$(SMOKETEST_DIR)
BEHAVIOURS= src/sin_task.erl src/sin_dep_resolver.erl
RSYNC_OPTIONS=-vaz --delete
.SUFFIXES: .erl .beam .yrl

vpath %.erl src test

ERL_OBJ = $(patsubst src/%.erl,$(BEAMDIR)/%.beam, $(wildcard $(SRCDIR)/*erl))
ERL_TEST_OBJ = $(patsubst test/%.erl,$(BEAMDIR)/%.beam, $(wildcard $(TESTDIR)/*erl))

all: main

setup: $(COPYDIRS)
	for f in $^ ; do	\
		mkdir -p $(APPDIR)/$$f ; \
		rsync $(RSYNC_OPTIONS) $$f $(APPDIR); \
	done
	mkdir -p $(APPDIR)/ebin;
	rsync $(RSYNC_OPTIONS) ebin/sinan.app $(APPDIR)/ebin/sinan.app

build_behaviours: $(BEHAVIOURS)
        # make sure sin_task gets built first so its always available
	erlc -pa $(BEAMDIR) +warn_export_vars +warn_export_all \
	+warn_obsolete_guard \
	+warnings_as_errors +bin_opt_info +debug_info -W -o $(BEAMDIR) $(BEHAVIOURS)

main: setup build_behaviours ${ERL_OBJ} ${ERL_TEST_OBJ}

$(BEAMDIR)/%.beam: %.erl
	erlc -pa $(BEAMDIR) +warn_export_vars +warn_export_all \
	+warn_obsolete_guard \
	+warnings_as_errors +bin_opt_info +debug_info -W -o $(BEAMDIR) $<

build: main
	erl -pa $(BEAMDIR) -s sinan main -extra -s $(CURDIR) build

escript: main
	erl -pa $(BEAMDIR) -s sinan main -extra -s $(CURDIR) escript

cucumber: main
	erl -pa $(BEAMDIR) -s sinan main -extra -s $(CURDIR) cucumber

proper: main
	erl -pa $(BEAMDIR) -s sinan main -extra -s $(CURDIR) proper

eunit: main
	erl -pa $(BEAMDIR) -s sinan main -extra -s $(CURDIR) eunit

dialyzer: main
	erl -pa $(BEAMDIR) -s sinan main -extra -s $(CURDIR) dialyzer

run: main
	$(ERL) -pa $(BEAMDIR)

debug: main
	$(ERL) -pa $(BEAMDIR) -s debugger start

smoketests: main
	for f in $(wildcard $(SMOKETEST_DIR)/tests/*.py) ; do	\
		PYTHONPATH=$(PYPATH) python2 $$f ; \
	done

testall : cucumber test smoketests

gh-pages:
	./do-gh-pages

clean:
	rm -rf _build ;
	rm -rf erl_crush.dump
