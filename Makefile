REBAR = rebar3

.PHONY: rel package compile test

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

qc: clean all
	$(REBAR) as eqc eqc -x -n 100000

eqc-ci: all
	$(REBAR) -D EQC_CI -C rebar_eqc_ci.config compile eunit skip_deps=true --verbose

bench:
	$(REBAR) ct

eqc-compile: compile
	mkdir tbin
	(cd eqc; erl -noshell -DEQC -DTEST -eval 'make:all([{parse_transform, eqc_cover}, {outdir, "../tbin"}])' -s init stop)
	(cd src; erl -noshell -DEQC -DTEST -eval 'make:all([{parse_transform, eqc_cover}, {i, "../include"}, {outdir, "../_build/default/lib/mmath/ebin"}])' -s init stop)

###
### Docs
###
docs:
	$(REBAR) skip_deps=true doc

##
## Developer targets
##

xref:
	$(REBAR) xref

console:
	$(REBAR) shell
