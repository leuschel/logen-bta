LOGEN=logen
SICS=sicstus
SPLD=spld
BTABIN=bin/bta
BTASRC=${SICS} -l  cli_bta.pl --goal "go_cli,halt." --
BTAs=${SICS} -l  cli_bta.pl --goal "go_cli,halt." -- -scc
BTA=${BTABIN} -scc

$(BTABIN):  bin/bta.sav
	${SPLD} --static --output $(BTABIN) --resources=./bin/bta.sav=/bta.sav
bin/bta.sav:  src/*.pl
	echo "Building stand-alone binary of the bta"
	${SICS} -l src/cli_bta.pl --goal "save_program('bin/bta.sav'),halt."

examples/re_matcher_bti.pl.ann: $(BTABIN) examples/re_matcher_bti.pl
	echo "Running BTA: "
	${BTA} examples/re_matcher_bti.pl -o examples/re_matcher_bti.pl.ann --entry "test3(d)."
test: examples/re_matcher_bti.pl.ann
	echo "Running Logen: "
	${LOGEN} examples/re_matcher_bti.pl "test3(X)" -ap --spec_file examples/re_matcher_spec.pl -v
	echo "Running specialised program: "
	sicstus -l examples/re_matcher_spec.pl --goal "test3__0([a,b]), \+ test3__0([a,b,a]), halt."


clean:
	-rm bin/bta*
	#rm spldgen*.sav.o
