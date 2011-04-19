VSN=1.0.0
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
	for f in $^ ; do	\
                mkdir -p $(APPDIR)/$$f ; \
		rsync $(RSYNC_OPTIONS) $$f $(APPDIR); \
	done
	mkdir -p $(APPDIR)/ebin;
	rsync $(RSYNC_OPTIONS) ebin/sinan.app $(APPDIR)/ebin/sinan.app

main: setup ${ERL_OBJ} ${ERL_TEST_OBJ}

$(BEAMDIR)/%.beam: %.erl
#         +warn_unused_vars Both of these should be enabled but can't until
#        proper is fixed
	erlc +warn_export_vars +warn_export_all \
	+warn_obsolete_guard \
	+warnings_as_errors +bin_opt_info +debug_info -W -o $(BEAMDIR) $<

proper: main
	for f in ${ERL_TEST_OBJ} ; do	\
		$(ERL) -noshell -pa $(BEAMDIR) -s proper module `basename $$f .beam` -s init stop; \
	done

run: main
	$(ERL) -pa $(BEAMDIR) -s sin_app manual_start

debug: main
	$(ERL) -pa $(BEAMDIR) -s sin_app manual_start -s debugger start

clean:
	rm -rf _build ;
	rm -rf erl_crush.dump