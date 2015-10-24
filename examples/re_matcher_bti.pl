
eq(A,B,M,R) :- A=B,M=R.
eq(A,B,_,false) :- A \= B.

%eq(A,B,M,R) :- (A=B -> M=R ; R=false).

shift(epsilon,Env,_,_,Env,false).
shift(char(A),Env,B,Mark,Env,R) :- eq(A,B,Mark,R). %(A=B -> R=Mark ; R=false).
shift(choice(A,B),Env,C,Mark,Env2,R) :-
	 shift(A,Env,C,Mark,Env1,RA),
	 shift(B,Env1,C,Mark,Env2,RB),
	 or(RA,RB,R).
shift(seq(A,OldA,B),Env,C,Mark,Env3,R) :-
     shift(A,Env,C,Mark,Env1,RA),
     empty(A,EA),
     and(Mark,EA,And1),
     get_mark(OldA,Env,OldAMark),
     or(And1,OldAMark,M2),
     shift(B,Env1,C,M2,Env2,RB),
     empty(B,EB), and(RA,EB,And2),
     set_mark(OldA,Env2,RA,Env3),
     or(And2,RB,R).
shift(star(A,Old),Env,C,Mark,Env2,R) :-
     get_mark(Old,Env,OldMark),
     or(Mark,OldMark,Or),
     shift(A,Env,C,Or,Env1,R),
     set_mark(Old,Env1,R,Env2).

'$UNFOLDCALLS'(get_mark(N,_,_)) :- true.
'$UNFOLDCALLS'(set_mark(N,_,_,_)) :- true.

get_mark(1,[H|_],H).
get_mark(N,[_|T],R) :- N>1, N1 is N-1,
   get_mark(N1,T,R).
set_mark(1,[_|T],New,[New|T]).
set_mark(N,[H|T],New,[H|TR]) :- N>1, N1 is N-1,
   set_mark(N1,T,New,TR).

'$MEMOANN'(or,3,[d,d,d]).
or(true,_,true).
or(false,A,A).

'$MEMOANN'(and,3,[d,d,d]).
and(true,A,A).
and(false,_,false).

'$MEMOANN'(match,3,[d,s,list]).
match([],RE,_) :- empty(RE,true).
match([H|T],RE,Env) :-
   shift(RE,Env,H,true,Env2,HRes),
   match2(T,RE,Env2,HRes).

'$MEMOANN'(match2,4,[d,s,list,d]).
match2([],_,_,true).
match2([H|T],RE,Env,_) :-
    shift(RE,Env,H,false,Env2,HRes),
    match2(T,RE,Env2,HRes).
   
test1(X) :- match(X,choice(char(a),char(b)),[]).
test2(X) :- match(X,choice(char(a),epsilon),[]).
test3(X) :- match(X,seq(char(a),1,char(b)),[false]).
test4(X) :- match(X,seq(char(a),1,seq(char(a),2,char(b))),[false,false]).
test5(X) :- match(X,star(seq(char(a),1,seq(char(a),2,char(b))),3),[false,false,false]).

% :- test5([a,a,b,a,a,b]).

test :- 
        print(test1),nl,
        test1([a]), test1([b]), \+ test1([]), \+ test1([c]), \+test1([a,b]),
        print(test2),nl,
        test2([a]), test2([]), \+ test2([b]), \+ test2([c]), \+test2([a,b]),
        print(test3),nl,
        test3([a,b]), \+ test3([a]), \+ test3([b]), \+test3([a,c]),
        print(test4),nl,
        test4([a,a,b]), \+ test4([a,a]), \+ test4([a,b]), \+test4([a,c]),
        print(test5),nl,
        test5([a,a,b]), test5([a,a,b,a,a,b]), test5([]),
        \+ test5([a,a]), \+ test5([a,b]), \+test5([a,c]).
        
empty(epsilon,true).
empty(char(_),false).
empty(star(_,_),true).
empty(seq(A,_,B),R) :-  empty(A,EA), empty(B,EB), and(EA,EB,R).
empty(choice(A,B),R) :- empty(A,EA), empty(B,EB), or(EA,EB,R).