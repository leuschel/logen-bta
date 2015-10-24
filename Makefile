#LOGEN=/usr/local/logen_29Nov2008/logen
LOGEN=/Users/leuschel/svn_root/logen/logen
ECCE=/Users/leuschel/svn_root/ecce/ecce_source/ecce
SICS=sicstus
SPLD=spld
BTABIN=bin/bta
BTASRC5=/usr/local/sicstus4.0.5/bin/sicstus -l  cli_bta.pl --goal "go_cli,halt." --
BTASRC=${SICS} -l  cli_bta.pl --goal "go_cli,halt." --
BTAs=${SICS} -l  cli_bta.pl --goal "go_cli,halt." -- -scc
BTA=${BTABIN} -scc

bin/bta.sav:  src/*.pl
	echo "Building stand-alone binary of the bta"
	${SICS} -l src/cli_bta.pl --goal "save_program('bin/bta.sav'),halt."
$(BTABIN):  bin/bta.sav
	${SPLD} --static --output $(BTABIN) --resources=./bin/bta.sav=/bta.sav

examples/re_matcher_bti.pl.ann: $(BTABIN) examples/re_matcher_bti.pl
	echo "Running BTA: "
	${BTA} examples/re_matcher_bti.pl -o examples/re_matcher_bti.pl.ann --entry "test3(d)."
test: examples/re_matcher_bti.pl.ann
	echo "Running Logen: "
	${LOGEN} examples/re_matcher_bti.pl "test3(X)" -ap --spec_file examples/re_matcher_spec.pl
	echo "Running specialised program: "
	sicstus -l examples/re_matcher_spec.pl --goal "test3__0([a,b]), \+ test3__0([a,b,a]), halt."


clean:
	rm bin/bta*
	#rm spldgen*.sav.o
