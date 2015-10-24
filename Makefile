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
bin/bta:  src/*.pl */*.pl
	echo "Building stand-alone binary of the bta"
	${SICS} -l cli_bta.pl --goal "save_program('bin/bta.sav'),halt."
	${SPLD} --static --output bin/bta --resources=./bin/bta.sav=/bta.sav
#bta2:  *.pl */*.pl
#	/usr/local/sicstus4.0.5/bin/sicstus -l self/bta_single_file.pl --goal "save_program('bin/bta2.sav'),halt."
#	/usr/local/sicstus4.0.5/bin/spld --static --output bin/bta2 --resources=./bin/bta2.sav=/bta2.sav
testvs:
	make verysimple2
install: bin/bta
	cp bin/bta /var/local/www/systems/size-change-bta/bin/linux32_glibc2.7/
bench:
	rm tests/dppd/*.log
	make contains
	make ssuply
	make liftsolve
	make liftsolve_bti
	make match
	make regexp_r3
	make vanilla-dppd
	make imperative_power
re:
	${BTA} re_matcher_bti.pl -o re_matcher_bti.pl.ann --entry "test3(d)."
	make re_pe
re_pe:
	${LOGEN} re_matcher_bti.pl "test3(X)" -ap --spec_file re_matcher_3.pl 
	sicstus -l re_matcher_3.pl --goal "test3__0([a,b]), \+ test3__0([a,b,a]), halt."
sat_simple:
	${BTA} very_simple/sat_simple_rec.pl -o very_simple/sat_simple_rec.pl.ann --entry "sat(s,s)."
	more very_simple/sat_simple_rec.pl.ann
verysimple:
	${BTA} very_simple/simple.pl -o very_simple/simple.pl.ann --entry "p(s)."
	more very_simple/simple.pl.ann
	${LOGEN} very_simple/simple.pl "p(s(a))" -np
verysimple2:
	${BTA} very_simple/simple_fun.pl -o very_simple/simple_fun.pl.ann --entry "f(s,s,d)."
	more very_simple/simple_fun.pl.ann
	${LOGEN} very_simple/simple_fun.pl "f(s(0),s(0),R)" -np --spec_file very_simple/simple_fun_spec.pl
	sicstus -l very_simple/simple_fun_spec.pl --goal "f__0(R),print(R),nl,R==s(s(s(0))),halt."
	${BTA} very_simple/simple_fun.pl -o very_simple/simple_fun.pl.ann --entry "fm(s,s,d)."
	more very_simple/simple_fun.pl.ann
	${LOGEN} very_simple/simple_fun.pl "fm(s(0),s(0),R)" -np --spec_file very_simple/simple_fun_spec.pl
	more very_simple/simple_fun_spec.pl
	sicstus -l very_simple/simple_fun_spec.pl --goal "fm__0(R),print(R),nl,R==s(s(s(0))),halt."
selfapp:
	bin/bta self/bta_single_file.pl -o self/bta_single_file.pl.ann --entry "go_cli." -scc
	#more self/bta_single_file.pl.ann
	# ${LOGEN} self/bta_single_file.pl "go_cli" -np
test1:
	${BTASRC} very_simple/test1b.pl -o very_simple/test1.pl.ann --entry "test(d)."
	more very_simple/test1.pl.ann
	${LOGEN} very_simple/test1.pl "test(X)" -w
test: bin/bta
	${BTA} sample_input.pl --time_out 1000 -o sample_input.pl.ann
	${LOGEN} sample_input.pl "test(X)"
	${BTA} tests/match.pl -o tests/match.pl.ann
	${LOGEN} tests/match.pl "match([a,a,b],R)"
match:
	${BTA} tests/match.pl -o tests/match_spec.pl.ann -ih
	${LOGEN} tests/match.pl "match([a,a,b],R)" --spec_file tests/match_spec.pl
	echo "Running     ***    ECCE     ***"
	${ECCE} tests/match.pl -pe "match([a,a,b],R)" -o tests/match_ecce.pl
	echo "Running     ***    Experiment     ***"
	sicstus -l tests/match_dppd.pl --goal "dppd,halt."
	sicstus -l tests/match_dppd_spec.pl --goal "dppd,halt."
	sicstus -l tests/match_dppd_ecce.pl --goal "dppd,halt."
match-semi: bin/bta
	${BTA} tests/match.pl -sc -su -om -o tests/match_spec.pl.ann 
	${LOGEN} tests/match.pl "match([a,a,b],R)" --spec_file tests/match_spec.pl
	${ECCE} tests/match.pl -pe "match([a,a,b],R)" -o tests/match_ecce.pl
	sicstus -l tests/match_dppd.pl --goal "dppd,halt."
	sicstus -l tests/match_dppd_spec.pl --goal "dppd,halt."
	sicstus -l tests/match_dppd_ecce.pl --goal "dppd,halt."
inter_medium: bin/bta
	${BTA} tests/inter_medium.pl -o tests/inter_medium.pl.ann
	${LOGEN} tests/inter_medium.pl "test1(X,Y)"
inter_mediumu: bin/bta
	${BTA} tests/inter_medium_unfolded.pl -o tests/inter_medium_unfolded.pl.ann
	${LOGEN} tests/inter_medium_unfolded.pl "test1(X,Y)"
cont: bin/bta
	${BTA} tests/continuation_interpreter.pl -o tests/continuation_interpreter.pl.ann
	${LOGEN} tests/continuation_interpreter.pl "test1(X,Y,Z,Res)"
rev:
	${BTA} tests/rev.pl -o tests/rev.pl.ann --entry "rev2(d,s)."
	${LOGEN} tests/rev.pl "rev([a,b,c],R)"
	${LOGEN} tests/rev.pl "rev2(X,[d,e,f])"
rotate:
	${BTA} tests/rev.pl -o tests/rev.pl.ann --entry "rotate(s,d)."
	${LOGEN} tests/rev.pl "rotate([d,e,f],X)"
list: bin/bta
	${BTA} tests/list_manip.pl -o tests/list_manip.pl.ann
	${LOGEN} tests/list_manip.pl "test([a,b,c])"
	${LOGEN} tests/list_manip.pl "test2([1,1,0])"
simple: bin/bta
	${BTA} tests/simple_tests.pl -o tests/simple_tests.pl.ann --entry "p(d)."
	${LOGEN} tests/simple_tests.pl "p([a,a])"
	${BTA} tests/simple_tests.pl -o tests/simple_tests.pl.ann --entry "p2(d)."
	${LOGEN} tests/simple_tests.pl "p2([a,b])"
vanilla:
	${BTA} tests/vanilla_list.pl -o tests/vanilla_list.pl.ann --entry "test1(d)." -scc
	${LOGEN} tests/vanilla_list.pl "test1(X)"
vanilla-dppd:
	${BTA} tests/vanilla_list.pl -o tests/vanilla_list.pl.ann --entry "solve(list_nv)." -ih
	${LOGEN} tests/vanilla_list.pl "solve([doubleapp(X,Y,Z,R)])" --spec_file tests/vanilla_list_spec.pl
	sicstus -l tests/dppd/vanilla.doubleapp.bm.bor
	sicstus -l tests/dppd/vanilla.doubleapp.bm.bsp
	${ECCE} tests/vanilla_list.pl -pe "solve([doubleapp(X,Y,Z,R)])" -o tests/vanilla_list_spec_ecce.pl
	sicstus -l tests/dppd/vanilla.doubleapp.bm.bspe
dbaccess:
	${BTA} tests/dbaccess.pl -o tests/dbaccess.pl.ann --entry "bench(d)."
	${LOGEN} tests/dbaccess.pl "bench(X)" --spec_file tests/dbaccess_spec.pl
	${ECCE} tests/dbaccess.pl -pe "bench(X)" -o tests/dbaccess_spec_ecce.pl
	sicstus -l tests/dbaccess.pl --goal "bench(100000),halt."
	sicstus -l tests/dbaccess_spec.pl --goal "bench__0(100000),halt."
	sicstus -l tests/dbaccess_spec_ecce.pl --goal "bench__1(100000),halt."
model_elim:
	${BTA} tests/model_elim.pl -o tests/model_elim.pl.ann --entry "solve(nv,s)."
	${LOGEN} tests/model_elim.pl "solve(neg(app(X,Y,Z)),[])"
contains:
	${BTA} tests/contains.pl -o tests/contains.pl.ann --entry "contains(s,d)." -scc -ll
	${LOGEN} tests/contains.pl "contains([a,a,b],X)." --spec_file tests/contains_spec.pl
	${ECCE} tests/contains.pl -pe "contains([a,a,b],X)." -o tests/contains_spec_ecce.pl
	sicstus -l tests/dppd/contains.kmp.bm.bor
	sicstus -l tests/dppd/contains.kmp.bm.bsp
	sicstus -l tests/dppd/contains.kmp.bm.bspe
liftsolve:
	${BTA} tests/liftsolve.pl -o tests/liftsolve.pl.ann --entry "solve(s,list_nv)." -ih
	${LOGEN} tests/liftsolve.pl --spec_file tests/liftsolve_spec.pl "solve([term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),term(cons,[var(h),var(z)])]), term(app,[var(x),var(y),var(z)]) ]) ], [term(app,[X,Y,Z])])"
	${ECCE} tests/liftsolve.pl -o tests/liftsolve_spec_ecce.pl -pe "solve([term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),term(cons,[var(h),var(z)])]), term(app,[var(x),var(y),var(z)]) ]) ], [term(app,[X,Y,Z])])"
	sicstus -l tests/dppd/liftsolve.app.bm.bor
	sicstus -l tests/dppd/liftsolve.app.bm.bsp
	sicstus -l tests/dppd/liftsolve.app.bm.bspe
liftsolve_bti:
	${BTA} tests/liftsolve_bti.pl -o tests/liftsolve_bti.pl.ann --entry "solve(s,list_nv)." -ih --scc
	${LOGEN} tests/liftsolve_bti.pl --spec_file tests/liftsolve_spec.pl "solve([term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),term(cons,[var(h),var(z)])]), term(app,[var(x),var(y),var(z)]) ]) ], [term(app,[X,Y,Z])])"
	sicstus -l tests/dppd/liftsolve.app.bm.bsp
imp_bta:
	${BTASRC} tests/imperative_power.pl -o tests/imperative_power.pl.ann --entry "power(s,s,d,d)." -sc
	${BTA} tests/imperative_power.pl -o tests/imperative_power.pl.ann --entry "power(s,s,d,d)." -sc
imperative_power_bta:
	${BTASRC} tests/imperative_power.pl -o tests/imperative_power.pl.ann --entry "power(s,s,d,d)." -sc
	${LOGEN} tests/imperative_power.pl "power(2,5,Ein,Eout)" --spec_file tests/imperative_power_spec.pl -w
imperative_power_btan:
	${BTASRC} tests/imperative_power.pl -o tests/imperative_power.pl.ann --entry "power(s,s,d,d)." -sc --scc
	${LOGEN} tests/imperative_power.pl "power(2,5,Ein,Eout)" --spec_file tests/imperative_power_spec.pl -w
imperative_power:
	${BTA} tests/imperative_power.pl -o tests/imperative_power.pl.ann --entry "power(s,s,d,d)." -sc --scc
	${LOGEN} tests/imperative_power.pl "power(2,5,Ein,Eout)" --spec_file tests/imperative_power_spec.pl
	echo "ORIGINAL"
	sicstus -l tests/dppd/imperative-solve.bm.bor
	echo "LOGEN SPECIALISED"
	sicstus -l tests/dppd/imperative-solve.bm.bsp
	${ECCE} tests/imperative_power.pl -pe "power(2,5,Ein,Eout)" -o tests/imperative_power_spec_ecce.pl
	echo "ECCE"
	sicstus -l tests/dppd/imperative-solve.bm.bspe
picemul:
	${BTA} tests/picemul.pl -o tests/picemul.pl.ann --entry "specialize_this." -ih
	${LOGEN} tests/picemul.pl "specialize_this" --spec_file tests/picemul_spec.pl
picemul_ecce:
	${ECCE} tests/picemul.pl -pe "specialize_this"
prop: bin/bta
	${BTA} tests/prop_int.pl -o tests/prop_int.pl.ann --entry "test(d)."
	${LOGEN} tests/prop_int.pl "test(X)"
map: bin/bta
	${BTA} tests/map.pl -o tests/map.pl.ann --entry "reduce_add(d,d)."
	${LOGEN} tests/map.pl "reduce_add([1,2,3],R)"
	${BTA} tests/map.pl -o tests/map.pl.ann --entry "reduce_add(list,d)."
	${LOGEN} tests/map.pl "reduce_add([1,2,3],R)"
	echo "To Do: infer patterns for call + use special HO-annotations"
ctl1:
	${BTASRC} tests/ctl.pl -o tests/ctl.pl.ann -su --entry "isat(d,s)."  -scc -v -ll
ctl:
	${BTA} tests/ctl.pl -o tests/ctl.pl.ann -su --entry "isat(d,s)." -scc
	${LOGEN} tests/ctl.pl "isat(_X,ef(p(unsafe)))" --spec_file tests/ctl_spec.pl
	${ECCE} tests/ctl.pl -pe "isat(_X,ef(p(unsafe)))" -o tests/ctl_spec_ecce.pl
	sicstus -l tests/ctl.pl --goal "isat(s(s(s(0))),ef(p(unsafe)));halt."
	sicstus -l tests/ctl_spec.pl --goal "isat__0(s(s(s(0))));halt."
	sicstus -l tests/ctl_spec_ecce.pl --goal "statistics(runtime,Y),print(Y),nl,isat__1(s(s(s(0))));statistics(runtime,X),print(X),nl,halt."
ctl_orig:
	${BTA} tests/ctl_orig.pl -o tests/ctl_orig.pl.ann -su --entry "isat(d,s)." -ih
	${LOGEN} tests/ctl_orig.pl "isat(_X,ef(p(unsafe)))" --spec_file tests/ctl_spec.pl
	${ECCE} tests/ctl_orig.pl -pe "isat(_X,ef(p(unsafe)))" -o tests/ctl_spec_ecce.pl
	sicstus -l tests/ctl_orig.pl --goal "isat(s(s(s(0))),ef(p(unsafe)));halt."
	sicstus -l tests/ctl_spec.pl --goal "isat__0(s(s(s(0))));halt."
	sicstus -l tests/ctl_spec_ecce.pl --goal "statistics(runtime,Y),print(Y),nl,isat__1(s(s(s(0))));statistics(runtime,X),print(X),nl,halt."
ctlom: bin/bta
	${BTA} tests/ctl.pl -o tests/ctl.pl.ann -om --entry "isat(d,s)."
	${LOGEN} tests/ctl.pl "isat(s(s(s(0))),ef(p(unsafe)))" --spec_file tests/ctl_spec.pl
	sicstus -l tests/ctl_spec.pl --goal "isat__0;halt."
lambdaint:
	${BTA} tests/lambdaint.pl -o tests/lambdaint.pl.ann --entry "bench(d,d)." -scc
	${LOGEN} tests/lambdaint.pl "bench(22,25)" --spec_file tests/lambdaint_spec.pl
	sicstus -l tests/lambdaint.pl --goal "bench(22,25),halt."
	sicstus -l tests/lambdaint_spec.pl --goal "bench__0(22,25),halt."
	${ECCE} tests/lambdaint.pl -pe "bench(X,Y)" -o tests/lambdaint_ecce.pl
	sicstus -l tests/lambdaint_ecce.pl --goal "bench(22,25),halt."
lambdaint_bti:
	${BTA} tests/lambdaint_bti.pl -scc -o tests/lambdaint_bti.pl.ann --entry "bench(d,d)." -scc
	${LOGEN} tests/lambdaint_bti.pl "bench(22,25)" --spec_file tests/lambdaint_bti_spec.pl
	echo "ORIGINAL"
	sicstus -l tests/lambdaint_bti.pl --goal "bench(22,25),halt."
	echo "LOGEN VERSION"
	sicstus -l tests/lambdaint_bti_spec.pl --goal "bench__0(22,25),halt."
	echo "ECCE"
	${ECCE} tests/lambdaint_bti.pl -pe "bench(X,Y)" -o tests/lambdaint_bti_ecce.pl
	sicstus -l tests/lambdaint_bti_ecce.pl --goal "bench(22,25),halt."
lambdaint_bti_new:
	${BTA} tests/lambdaint_bti_new.pl -scc -o tests/lambdaint_bti_new.pl.ann --entry "bench(d,d)." -d out.dot -d2 foo.dot
	${LOGEN} tests/lambdaint_bti_new.pl "bench(22,25)" --spec_file tests/lambdaint_bti_new_spec.pl
	cat tests/lambdaint_bti_new_spec.pl
	sicstus -l tests/lambdaint_bti_new_spec.pl --goal "bench__0(22,25),halt."
lambdaintom: bin/bta
	${BTA} tests/lambdaint.pl -o tests/lambdaint.pl.ann -su -sc --entry "bench(d,d)."
	${LOGEN} tests/lambdaint.pl "bench(20,24)" --spec_file tests/lambdaint_spec.pl
	sicstus -l tests/lambdaint.pl --goal "bench(20,24),halt."
	sicstus -l tests/lambdaint_spec.pl --goal "bench__0(20,24),halt."
transpose: bin/bta
	${BTA} tests/transpose.pl -o tests/transpose.pl.ann --entry "entry(list,d,d,d)."
	${LOGEN} tests/transpose.pl "entry([X1,X2,X3,X4,X5,X6,X7,X8,X9],R2,R3,Res)" --spec_file tests/transpose_spec.pl
	sicstus -l tests/transpose_dppd.pl --goal "dppd,halt."
	sicstus -l tests/transpose_dppd_spec.pl --goal "dppd,halt."
regexp: bin/bta
	${BTA} tests/regexp.pl -o tests/regexp.pl.ann --entry "gen(s,d)."
	${LOGEN} tests/regexp.pl "gen(star(char(a)),R)"
regexp_r3:
	${BTA} tests/regexp.pl -o tests/regexp.pl.ann --entry "gen(s,d)." -ih
	${LOGEN} tests/regexp.pl --spec_file tests/regexp_spec.pl "gen(star(cat(or(char(a),char(b)),cat(or(char(a),char(b)),cat(or(char(a),char(b)),cat(or(char(a),char(b)),cat(or(char(a),char(b)),or(char(a),char(b)))))))),S)"
	sicstus -l tests/regexp_dppd.pl --goal "dppd,halt."
	sicstus -l tests/regexp_dppd_spec.pl --goal "dppd,halt."
	${ECCE} tests/regexp.pl -o tests/regexp_ecce.pl -pe "gen(star(cat(or(char(a),char(b)),cat(or(char(a),char(b)),cat(or(char(a),char(b)),cat(or(char(a),char(b)),cat(or(char(a),char(b)),or(char(a),char(b)))))))),S)"
	sicstus -l tests/regexp_dppd_ecce.pl --goal "dppd,halt."
groundunify:
	${BTA} tests/groundunify.pl -o tests/groundunify.pl.ann --entry "unify(nv,s,d)."
	${LOGEN} tests/groundunify.pl --spec_file tests/groundunify_spec.pl "unify(struct(p,[X]),struct(p,[struct(a,[])]),Sub)"
ssuply:
	${BTA} tests/ssuply.pl -o tests/ssuply.pl.ann --entry "ssupply( d, d, d )." -ih
	${LOGEN} tests/ssuply.pl --spec_file tests/ssuply_spec.pl "ssupply( _supplier, _part, _quantity )"
	${ECCE} tests/ssuply.pl -pe "ssupply( _supplier, _part, _quantity )" -o tests/ssuply_spec_ecce.pl
	sicstus -l tests/dppd/ssuply.bm.bor
	sicstus -l tests/dppd/ssuply.bm.bsp
demo_tiny2: bin/bta
	${ECCE} tests/demo_tiny2.pl -pe "test_orun_directly_simple(Add,X)"
	${ECCE} tests/demo_tiny2.pl -pe "test_orun_directly_loop(N1,N2,Output)"
	${BTA} tests/demo_tiny2.pl -o tests/demo_tiny2.pl.ann --entry "test_orun_directly_loop(d,d,d)."
	${LOGEN} tests/demo_tiny2.pl "test_orun_directly_loop(10,20,R)" --spec_file tests/demo_tiny2_spec.pl
demo_vanilla_list: bin/bta
	${ECCE} tests/demo_vanilla_list.pl -pe "replaceleaves1(Tree, NewLeaf, NewTree)"
	${BTA} tests/demo_vanilla_list.pl -o tests/demo_vanilla_list.pl.ann --entry "replaceleaves1(d,d,d)."
	${LOGEN} tests/demo_vanilla_list.pl "replaceleaves1(X,Y,R)" --spec_file tests/demo_vanilla_list_spec.pl
javabc0: javabc/small_jvm_int_I.pl
	${BTA} javabc/small_jvm_int_I.pl -ng -o javabc/small_jvm_int_I.pl.ann --entry "main(d,d)." -scc
	${LOGEN} javabc/small_jvm_int_I.pl "main(X,Y)" --spec_file javabc/small_jvm_int_I_spec.pl
	sicstus -l javabc/small_jvm_int_I.pl --goal "main([5],R),nl,print(result(R)),nl,nl,R==5,halt."
	sicstus -l javabc/small_jvm_int_I_spec.pl --goal "main__0([5],R),nl,print(result(R)),nl,nl,R==5,halt."
	${LOGEN} javabc/small_jvm_int_I2.pl "main(X,Y)" --spec_file javabc/small_jvm_int_I_spec2.pl
	sicstus -l javabc/small_jvm_int_I_spec2.pl --goal "main__0([5],R),nl,print(result(R)),nl,nl,R==5,halt."
javabc1: javabc/small_jvm_int_I.pl
	${BTA} javabc/small_jvm_int_I.pl -ng -nl -mv -sc -o javabc/small_jvm_int_I.pl.ann --entry "entry(d,d)."
	${LOGEN} javabc/small_jvm_int_I.pl "entry(X,Y)" -np --spec_file javabc/small_jvm_int_I_spec.pl
	cat javabc/small_jvm_int_I_spec.pl
	sicstus -l javabc/small_jvm_int_I_spec.pl --goal "entry__0(5,R),nl,print(result(R)),nl,nl,R==5,halt."
	${LOGEN} javabc/small_jvm_int_I.pl -ap "entry(X,Y)" --spec_file javabc/small_jvm_int_I_spec.pl
	cat javabc/small_jvm_int_I_spec.pl
	sicstus -l javabc/small_jvm_int_I_spec.pl --goal "entry__0(5,R),nl,print(result(R)),nl,nl,R==5,halt."
javabc11: javabc/small_jvm_int_I.pl
	${BTA} javabc/small_jvm_int_I.pl -sc -o javabc/small_jvm_int_I.pl.ann --entry "entry_driver(d,d,d)." -ih
	${LOGEN} javabc/small_jvm_int_I.pl "entry_driver(Nr,X,Y)" --spec_file javabc/small_jvm_int_I_spec.pl
	${ECCE} javabc/small_jvm_int_I.pl -pe "entry_driver(Nr,X,Y)." -o  javabc/small_jvm_int_I_spec_ecce.pl
	sicstus -l javabc/small_jvm_int_I.pl --goal "statistics(runtime,_),entry_driver(50000,5,R),statistics(runtime,X),nl,print(result(R,X)),nl,nl,R==5,halt."
	sicstus -l javabc/small_jvm_int_I_spec.pl --goal "statistics(runtime,_),entry_driver__0(50000,5,R),statistics(runtime,X),nl,print(result(R,X)),nl,nl,R==5,halt."
	sicstus -l javabc/small_jvm_int_I_spec_ecce.pl --goal "statistics(runtime,_),entry_driver__1(50000,5,R),statistics(runtime,X),nl,print(result(R,X)),nl,nl,R==5,halt."
javabc2:
	bin/btaext javabc/small_jvm_int_I.pl -o javabc/small_jvm_int_I.pl.ann --entry "main(d,d)."
	${LOGEN} javabc/small_jvm_int_I.pl "main(X,Y)" 
javabc_ip: javabc_ip/small_jvm_int_III.pl
	${BTA} javabc_ip/small_jvm_int_III.pl -ng -nl -mv -sc -o javabc_ip/small_jvm_int_III.pl.ann --entry "entry(s,d,d,d)."
	${LOGEN} javabc_ip/small_jvm_int_III.pl "entry(lcm,X,Y,R)" -ap -w --spec_file javabc_ip/small_jvm_int_III_spec.pl
	cat javabc_ip/small_jvm_int_III_spec.pl
	sicstus -l javabc_ip/small_jvm_int_III.pl --goal "entry(lcm,12,15,R),nl,print(lcm(R)),nl,nl,R==60,halt."
	sicstus -l javabc_ip/small_jvm_int_III_spec.pl --goal "entry__0(12,15,R),nl,print(lcm(R)),nl,nl,R==60,halt."
javabc_ip2: javabc_ip/small_jvm_int_III.pl
	${BTA} javabc_ip/small_jvm_int_III.pl -ng -nl -mv -o javabc_ip/small_jvm_int_III.pl.ann --entry "entry2(d,d,d,d)."
	${LOGEN} javabc_ip/small_jvm_int_III.pl "entry2(X,Y,R,R2)" -ap --spec_file javabc_ip/small_jvm_int_III_spec.pl
	cat javabc_ip/small_jvm_int_III_spec.pl
	sicstus -l javabc_ip/small_jvm_int_III.pl --goal "entry2(12,15,R,R2),nl,print(lcm_gcd(R,R2)),nl,nl,R==60,halt."
	sicstus -l javabc_ip/small_jvm_int_III_spec.pl --goal "entry2__0(12,15,R,R2),nl,print(lcm_gcd(R,R2)),nl,nl,R==60,halt."
javabc_ip3: javabc_ip/small_jvm_int_III.pl
	${BTA} javabc_ip/small_jvm_int_III.pl -ng -nl -mv -o javabc_ip/small_jvm_int_III.pl.ann --entry "entry_driver(d,d,d)."
	${LOGEN} javabc_ip/small_jvm_int_III.pl "entry_driver(M,A,R)" -ap --spec_file javabc_ip/small_jvm_int_III_spec.pl
	cat javabc_ip/small_jvm_int_III_spec.pl
	sicstus -l javabc_ip/small_jvm_int_III.pl --goal "entry_driver(lcm,[12,15],R),nl,print(lcm(12,15,R)),nl,nl,R==60,halt."
	sicstus -l javabc_ip/small_jvm_int_III_spec.pl --goal "entry_driver__0(lcm,[12,15],R),nl,print(lcm(12,15,R)),nl,nl,R==60,halt."
	sicstus -l javabc_ip/small_jvm_int_III_spec.pl --goal "entry_driver__0(fact,[5],R),nl,print(fact(5,R)),nl,nl,R==120,halt."
javabc_heap0: javabc_heap/small_jvm_int_IV.pl
	${BTA} javabc_heap/small_jvm_int_IV.pl -ng -nl -mv -o javabc_heap/small_jvm_int_IV.pl.ann --entry "entry_driver(d,d,d,d,d)."
	${LOGEN} javabc_heap/small_jvm_int_IV.pl "entry_driver(M,A,Hin,R,Hout)" -ap --spec_file javabc_heap/small_jvm_int_IV_spec.pl
	cat javabc_heap/small_jvm_int_IV_spec.pl
javabc_heap_sca: javabc_heap/small_jvm_int_IV.pl
	${BTA} javabc_heap/small_jvm_int_IV.pl -mv -o javabc_heap/small_jvm_int_IV.pl.ann --entry "entry_driver(d,d,d,d,d)."
	${LOGEN} javabc_heap/small_jvm_int_IV.pl "entry_driver(M,A,Hin,R,Hout)"
simple_length: javabc_ip/simple_length.pl
	${BTA} javabc_ip/simple_length.pl -ng -nl -mv -sc --entry "test(d,d,d)."
	${BTA} javabc_ip/simple_length.pl -ng -nl -mv -sc --entry "entry(s,d,d,d)."
demo_tiny2_v2: tests/demo_tiny2_v2.pl
	${BTA} tests/demo_tiny2_v2.pl -mv -o tests/demo_tiny2_v2.pl.ann --entry "otinyinterp(s,list_nv,d)."
	${LOGEN} tests/demo_tiny2_v2.pl "otinyinterp(['''{''', arg(2), int(1), add, storearg(2), arg(1), int(1), sub, storearg(1), arg(1), '''}''', arg(2)], [int(50000), int(20)], Output)" --spec_file tests/demo_tiny2_v2_spec.pl
	swipl -g "time(otinyinterp__0(Output)),print(Output),Output==[int(50020)],print(ok),nl,halt." -c tests/demo_tiny2_v2_spec.pl
probi:
	${BTA} prob/b_interpreter.pl --entry "b_execute_top_level_operation(s,s,s,d,d)." -o prob/b_interpreter.pl.ann -scc
probsi:
	${BTASRC} prob/b_interpreter.pl --entry "b_execute_top_level_operation(s,s,s,d,d)." -o prob/b_interpreter.pl.ann -scc
promela:
	${BTASRC} prob/h_int.pl --entry "test ." -o prob/h_int.pl.ann
	${LOGEN} prob/h_int.pl "test" --spec_file prob/h_int.pl_spec.pl -w -v
	# sicstus -l prob/h_int.pl --goal "test."
	sicstus -l prob/h_int.pl_spec.pl --goal "use_module(library(avl)),test__0."
leupel:
	${BTA} large/leupel_complete.pl --entry "futa3." -o large/leupel_complete.pl.ann
	${LOGEN} large/leupel_complete.pl "futa3" --spec_file large/leupel_complete_futa3.pl -w -v
goedel:
	${BTA} large/goedel_complete_src.pl
goedel_bta:
	${BTA} large/goedel_complete_src.pl --entry "make_modules(s,d,d)." -o large/goedel_complete_src.pl.ann
goedel_bta2:
	${BTA} large/goedel_complete_src.pl --entry "parse_language1(s,d,d,d,d,d)."
SAT=/Users/leuschel/svn_root/ecce/ecce_examples/sat_codish
sat1:
	${BTA} ${SAT}/sum_32_8_po.pl -su -om --entry "go(d) ." -o ${SAT}/sum_32_8_po.pl.ann -d ${SAT}/sum32_dep.dot
	${LOGEN} ${SAT}/sum_32_8_po.pl "go(X)" --spec_file ${SAT}/sum_32_8_po_logenspec.pl
sat2:
	${BTA} ${SAT}/sum_128_16_po.pl -su -om -ng --entry "go(d) ." -o ${SAT}/sum_128_16_po.pl.ann
	${LOGEN} ${SAT}/sum_128_16_po.pl "go(X)" --spec_file ${SAT}/sum_128_16_po_logenspec.pl
clean:
	rm spldgen*.sav.o
