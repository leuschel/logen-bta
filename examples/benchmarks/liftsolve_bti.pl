/* --------------------- */
/* solve(GrRules,NgGoal) */
/* --------------------- */
solve(_GrRules,[]).
solve(GrRules,[NgH|NgT]) :-
	non_ground_member(term(clause,[NgH|NgBody]),GrRules),
	solve(GrRules,NgBody),
	solve(GrRules,NgT).

/* -------------------------------------- */
/* non_ground_member(NgExpr,GrListOfExpr) */
/* -------------------------------------- */
non_ground_member(NgX,[GrH|_GrT]) :-
	make_non_ground(GrH,NgX).
non_ground_member(NgX,[_GrH|GrT]) :-
	non_ground_member(NgX,GrT).


/* --------------------------------------------------- */
/* make_non_ground(GroundRepOfExpr,NonGroundRepOfExpr) */
/* --------------------------------------------------- */
make_non_ground(G,NG) :-
	mkng(G,NG,[],[],_,_).

mkng(var(N),X,[],[],[N],[X]).
mkng(var(N),X,[N|T1],[X|T2],[N|T1],[X|T2]).
mkng(var(N),X,[M|T1],[Y|T2],[M|TT1],[Y|TT2]) :-
	N \== M,
	mkng(var(N),X,T1,T2,TT1,TT2).
mkng(term(F,Args),term(F,IArgs),InSub1,InSub2,OutSub1,OutSub2) :-
	l_mkng(Args,IArgs,InSub1,InSub2,OutSub1,OutSub2).

l_mkng([],[],Sub1,Sub2,Sub1,Sub2).
l_mkng([H|T],[IH|IT],InSub1,InSub2,OutSub1,OutSub2) :-
	mkng(H,IH,InSub1,InSub2,IntSub1,IntSub2),
	l_mkng(T,IT,IntSub1,IntSub2,OutSub1,OutSub2).


%'$MEMOANN'(mkng,6,[s,d,s,list,d,d]).
%'$MEMOANN'(l_mkng,6,[s,d,s,list,d,d]).
%'$UNFOLDCALLS'(mkng(X,_,_,_,_,_)) :- true.  % no longer needed with mkng_var

/*
member(X,[X|T]).
member(X,[Y|T]) :-
	member(X,T).

append([],L,L).
append([H|T],M,[H|T2]) :-
	append(T,M,T2).
	*/
	
test1(Z) :-
  solve([
	 term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),
	 term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),
					term(cons,[var(h),var(z)])]),
		term(app,[var(x),var(y),var(z)]) ])
		],
	[term(app,[term(cons,[term(a,[]),term(null,[])]),
			term(cons,[term(b,[]),term(null,[])]),Z])]).