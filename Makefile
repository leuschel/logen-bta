LOGEN=logen
SICS=sicstus
SPLD=spld
BTABIN=bin/bta
BTASRC=${SICS} -l  cli_bta.pl --goal "go_cli,halt." --
BTAs=${SICS} -l  cli_bta.pl --goal "go_cli,halt." -- -scc
BTA=${BTABIN} -scc
#.PHONY lambdaint_bti_bench

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

TESTS=examples/benchmarks
ECCE=ecce_cli
$(TESTS)/lambdaint_bti.pl.ann: $(TESTS)/lambdaint_bti.pl
	${BTA} $(TESTS)/lambdaint_bti.pl -scc -o $(TESTS)/lambdaint_bti.pl.ann --entry "bench(d,d)." -scc
$(TESTS)/lambdaint_bti_logen.pl: $(TESTS)/lambdaint_bti.pl.ann
	${LOGEN} $(TESTS)/lambdaint_bti.pl "bench(22,25)" --spec_file $(TESTS)/lambdaint_bti_logen.pl -v
$(TESTS)/lambdaint_bti_ecce.pl: $(TESTS)/lambdaint_bti.pl
	${ECCE} $(TESTS)/lambdaint_bti.pl -pe "bench(X,Y)" -o $(TESTS)/lambdaint_bti_ecce.pl
lambdaint_bti_bench: $(TESTS)/lambdaint_bti_logen.pl $(TESTS)/lambdaint_bti_ecce.pl
	echo "ORIGINAL"
	sicstus -l $(TESTS)/lambdaint_bti.pl --goal "bench(22,25),halt."
	echo "LOGEN VERSION"
	sicstus -l $(TESTS)/lambdaint_bti_logen.pl --goal "bench__0(22,25),halt."
	echo "ECCE"
	sicstus -l $(TESTS)/lambdaint_bti_ecce.pl --goal "bench(22,25),halt."

testif: examples/if_simple_spec.pl
	sicstus -l examples/if_simple_spec.pl --goal "test__0(X),print(X),nl,X==a,halt."
examples/if_simple_spec.pl: $(BTABIN) examples/if_simple.pl.ann
	echo "Running LOGEN: "
	${LOGEN} examples/if_simple.pl "test(X)" -ap --spec_file examples/if_simple_spec.pl -v
examples/if_simple.pl.ann: $(BTABIN) examples/if_simple.pl
	echo "Running BTA: "
	${BTA} examples/if_simple.pl -o examples/if_simple.pl.ann --entry "test(d)."


examples/imp_int_spec.pl: $(BTABIN) examples/imp_int.pl.ann
	echo "Running LOGEN: "
	${LOGEN} examples/imp_int.pl "test2(X,R)" -ap --spec_file examples/imp_int_spec.pl -v
	cat examples/imp_int_spec.pl
examples/imp_int.pl.ann: $(BTABIN) examples/imp_int.pl
	echo "Running BTA: "
	${BTA} examples/imp_int.pl -o examples/imp_int.pl.ann --entry "test2(d,d)."
examples/imp_int_bti_spec.pl: $(BTABIN) examples/imp_int_bti.pl.ann
	echo "Running LOGEN: "
	${LOGEN} examples/imp_int_bti.pl "test2(X,R1,R2)" -ap --spec_file examples/imp_int_bti_spec.pl -v
	cat examples/imp_int_bti_spec.pl
examples/imp_int_bti.pl.ann: $(BTABIN) examples/imp_int_bti.pl
	echo "Running BTA: "
	${BTA} examples/imp_int_bti.pl -o examples/imp_int_bti.pl.ann --entry "test2(d,d,d)."

examples/imp_int_taint_spec.pl: $(BTABIN) examples/imp_int_taint.pl.ann
	echo "Running LOGEN: "
	${LOGEN} examples/imp_int_taint.pl "test2(X,R1,R2)" -ap --spec_file examples/imp_int_taint_spec.pl -v
	cat examples/imp_int_taint_spec.pl
examples/imp_int_taint.pl.ann: $(BTABIN) examples/imp_int_taint.pl Makefile
	echo "Running BTA: "
	${BTA} -ng examples/imp_int_taint.pl -o examples/imp_int_taint.pl.ann --entry "test2(d,d,d)."

taint: examples/imp_int_taint_spec.pl
bench: lambdaint_bti_bench
clean:
	-rm bin/bta*
	#rm spldgen*.sav.o
