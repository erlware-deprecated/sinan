VSN=4.0.1
ERLC=/usr/bin/env erlc
ERL=/usr/bin/env erl

RELDIR=$(CURDIR)/_build/sinan
APPDIR=$(RELDIR)lib/sinan-$(VSN)
LOGDIR=$(RELDIR)/logs
BINDIR=$(DESTDIR)/usr/bin
INSTALL_TARGET=$(DESTDIR)/usr/lib/erlang/lib/sinan-$(VSN)

TARBALL=../erlang-sinan_$(VSN).orig.tar.gz
SRCDIR=src
TESTDIR=test
COPYDIRS= include src test
BEAMDIR=$(APPDIR)/ebin
SMOKETEST_DIR=$(CURDIR)/smoketests
PYPATH=$(PYTHONPATH):$(SMOKETEST_DIR)
BEHAVIOURS= src/sin_task.erl src/sin_dep_resolver.erl

.PHONY=all setup build escript cucumber proper eunit dialyzer \
	run debug smoketests testall gh-pages clean install-deb \
	build-deb publish-ppa update-version

SINFLAGS=-s $(CURDIR) -p sinan -n $(VSN)
ERLFLAGS= -noinput -pa $(BEAMDIR)

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
	$(ERL) $(ERLFLAGS) -s sinan main -extra $(SINFLAGS) build

escript: main
	$(ERL) $(ERLFLAGS) -s sinan main -extra $(SINFLAGS) escript

cucumber: main
	$(ERL) $(ERLFLAGS) -s sinan main -extra $(SINFLAGS) cucumber

proper: main
	$(ERL) $(ERLFLAGS) -s sinan main -extra $(SINFLAGS) proper

eunit: main
	$(ERL) $(ERLFLAGS) -s sinan main -extra $(SINFLAGS) eunit

dialyzer: main
	$(ERL) $(ERLFLAGS) -s sinan main -extra $(SINFLAGS) dialyzer

run: main
	$(ERL) -pa $(BEAMDIR)

debug: main
	$(ERL) $(ERLFLAGS) -s debugger start

smoketests: main
	for f in $(wildcard $(SMOKETEST_DIR)/tests/*.py) ; do	\
		PYTHONPATH=$(PYPATH) python $$f ; \
	done

ct: main $(LOGDIR)
	ct_run -dir $(CURDIR)/test -logdir $(LOGDIR) -erl_args -pa $(BEAMDIR)

testall : cucumber proper eunit smoketests

gh-pages:
	./do-gh-pages

clean:
	rm -f ../erlang-sinan_*.debian.tar.gz
	rm -f ../erlang-sinan_*.dsc
	rm -f ../erlang-sinan_*.build
	rm -f ../erlang-sinan_*.changes
	rm -rf debian/patches
	rm -rf debian/erlang-sinan
	rm -rf $(CURDIR)/usr
	rm -rf _build
	rm -rf erl_crash.dump
	find smoketests -name \*.pyc -exec rm -f {} \;

update-version:
	awk '{sub(/project_vsn, \"[0-9]+\.[0-9]+\.[0-9]+[a-z]?\"/,"project_vsn, \"$(VSN)\"");print}' sinan.config > tmp.txt
	@mv tmp.txt sinan.config
	awk '{sub(/vsn, \"[0-9]+\.[0-9]+\.[0-9]+[a-z]?\"/,"vsn, \"$(VSN)\"");print}' ebin/sinan.app > tmp.txt
	@mv tmp.txt ebin/sinan.app
	awk '{sub(/\"v[0-9]+\.[0-9]+\.[0-9]+[a-z]?\"/,"\"v$(VSN)\"");print}' src/sin_task_version.erl > tmp.txt
	@mv tmp.txt src/sin_task_version.erl
	git add sinan.config
	git add Makefile
	git add ebin/sinan.app
	git add src/sin_task_version.erl
	git commit -m "Version bump $(VSN)"
	git tag v$(VSN)

$(LOGDIR):
	mkdir -p $(LOGDIR)


##
## Debian packaging support for sinan
##

$(TARBALL):
	git archive --format=tar --prefix=sinan/ HEAD | gzip > $(TARBALL)

install-deb:
	mkdir -p $(INSTALL_TARGET)
	mkdir -p $(BINDIR)
	cp -r $(APPDIR)/* $(INSTALL_TARGET)
	cp -r _build/sinan/escript/sinan $(BINDIR)

build-deb: $(TARBALL)
	pdebuild
	debuild -S


publish-ppa: build-deb
	dput -f ppa:afiniate/ppa ../erlang-sinan_$(VSN)_source.changes
