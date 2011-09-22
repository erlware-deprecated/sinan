VSN=1.0.1
ERLC=/usr/local/bin/erlc
ERL=/usr/local/bin/erl
APPDIR= $(abspath ./_build/sinan/apps/sinan-$(VSN))
SRCDIR=src
TESTDIR=test
COPYDIRS= src test
BEAMDIR=$(APPDIR)/ebin
SMOKETEST_DIR=$(CURDIR)/smoketests
PYPATH=$(PYTHONPATH):$(SMOKETEST_DIR)
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

main: setup ${ERL_OBJ} ${ERL_TEST_OBJ}

$(BEAMDIR)/%.beam: %.erl
	erlc -pa $(BEAMDIR) +warn_export_vars +warn_export_all \
	+warn_obsolete_guard \
	+warnings_as_errors +bin_opt_info +debug_info -W -o $(BEAMDIR) $<

escript: main
	erl -pa $(BEAMDIR) -s sinan manual_start -s sinan main -extra -s $(CURDIR) escript

cucumber: main
	erl -pa $(BEAMDIR) -s sinan manual_start -s sinan main -extra -s $(CURDIR) cucumber

test: main
	erl -pa $(BEAMDIR) -s sinan manual_start -s sinan main -extra -s $(CURDIR) test all

run: main
	$(ERL) -pa $(BEAMDIR) -s sinan manual_start

debug: main
	$(ERL) -pa $(BEAMDIR) -s sinan manual_start -s debugger start

smoketests: main
	for f in $(wildcard $(SMOKETEST_DIR)/tests/*.py) ; do	\
		PYTHONPATH=$(PYPATH) python $$f ; \
	done

testall : cucumber test smoketests

gh-pages:
	./do-gh-pages

clean:
	rm -rf _build ;
	rm -rf erl_crush.dump
