VSN=1.0.1
ERLC=/usr/local/bin/erlc
ERL=/usr/local/bin/erl
APPDIR= $(abspath ./_build/development/apps/sinan-$(VSN))
SRCDIR=src
TESTDIR=test
COPYDIRS= src test priv
BEAMDIR=$(APPDIR)/ebin
RSYNC_OPTIONS=-vaz --delete

.SUFFIXES: .erl .beam .yrl

vpath %.erl src test

ERL_OBJ = $(patsubst src/%.erl,$(BEAMDIR)/%.beam, $(wildcard $(SRCDIR)/*erl))
ERL_TEST_OBJ = $(patsubst test/%.erl,$(BEAMDIR)/%.beam, $(wildcard $(TESTDIR)/*erl))

all: main

setup: $(COPYDIRS)
	for f in $^ ; do        \
		mkdir -p $(APPDIR)/$$f ; \
		rsync $(RSYNC_OPTIONS) $$f $(APPDIR); \
	done
	mkdir -p $(APPDIR)/ebin;
	rsync $(RSYNC_OPTIONS) ebin/sinan.app $(APPDIR)/ebin/sinan.app

main: setup ${ERL_OBJ} ${ERL_TEST_OBJ}

$(BEAMDIR)/%.beam: %.erl
	erlc +warn_export_vars +warn_export_all \
	+warn_obsolete_guard \
	+warnings_as_errors +bin_opt_info +debug_info -W -o $(BEAMDIR) $<

cucumber: main
	erl -pa $(BEAMDIR) -s sinan manual_start -s sinan main -extra -s $(CURDIR) cucumber

test: main
	erl -pa $(BEAMDIR) -s sinan manual_start -s sinan main -extra -s $(CURDIR) test

run: main
	$(ERL) -pa $(BEAMDIR) -s sinan manual_start

debug: main
	$(ERL) -pa $(BEAMDIR) -s sinan manual_start -s debugger start

clean:
	rm -rf _build ;
	rm -rf erl_crush.dump
