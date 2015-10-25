
% a binding-time improved version of lambdaint:
% separated environment into two lists

%:- mode l_eval(i,i,o).

l_eval([],_E,_V,[]).
l_eval([H|T],E,V,[EH|ET]) :-
	eval(H,E,V,EH),
	l_eval(T,E,V,ET).

%:- mode eval(i,i,o).

% annotations below are now sufficient:
% Still need to split ENV into static and dynamic component to further improve performance
'$MEMOANN'(eval,4,[s,s,list,d]).
%'$MEMOANN'(eval_if,6,[s,s,s,s,list,d]).
'$MEMOANN'(l_eval,4,[s,s,list,d]).
%'$MEMOANN'(test,3,[s,s,list]).

%'$UNFOLDCALLS'(eval(_,_,_,_)) :- true.
%'$UNFOLDCALLS'(test(_,_,_)) :- true.



%eval(X,E,V,_R) :- print(eval(X,E,V)),nl,fail.
eval(cst(C),_Env,_Vals,constr(C,[])).
eval(constr(C,Args),Env,Vals,constr(C,EArgs)) :-
	l_eval(Args,Env,Vals,EArgs).
eval(var(VKey),Env,Vals,Val) :- lookup(VKey,Env,Vals,Val).
eval(plus(X,Y),Env,Vals,constr(XY,[])) :-
	eval(X,Env,Vals,constr(VX,[])),
	eval(Y,Env,Vals,constr(VY,[])),
	XY is VX + VY.
eval(minus(X,Y),Env,Vals,constr(XY,[])) :-
	eval(X,Env,Vals,constr(VX,[])),
	eval(Y,Env,Vals,constr(VY,[])),
	XY is VX - VY.
eval(times(X,Y),Env,Vals,constr(XY,[])) :-
	eval(X,Env,Vals,constr(VX,[])),
	eval(Y,Env,Vals,constr(VY,[])),
	XY is VX * VY.
eval(eq(X,Y),Env,Vals,constr(BoolRes,[])) :-
	eval(X,Env,Vals,VX),
	eval(Y,Env,Vals,VY),
	(VX=VY -> (BoolRes = true) ; (BoolRes = false)).
eval(let(VKey,VExpr,InExpr),Env,Vals,Result) :-
        eval(VExpr,Env,Vals,VVal),
        store(Env,Vals,VKey,VVal,InEnv,InVals),
        eval(InExpr,InEnv,InVals,Result).
eval(if(Test,Then,Else), Env,Vals, Res) :-
    eval_if(Test,Then,Else,Env,Vals,Res).
eval(lambda(X,Expr),_Env,_Vals,lambda(X,Expr)).
eval(apply(Arg,F),Env,Vals,Res) :-
   % print(apply(Arg,F,Env)),nl,
	eval_fun(F,Env,Vals,FVal),    %% FVal could be dynamic with eval instead of eval_fun
	%print(FVal),nl,
	%%rename(FVal,Env,lambda(X,Expr)),
	FVal = lambda(X,Expr),
	%print(eval_arg(Arg,Env)),nl,
	eval(Arg,Env,Vals,ArgVal),
	%print(arg(ArgVal)),nl,
    store(Env,Vals,X,ArgVal,NewEnv,NewVals),
   % print(new_eval(Expr,NewEnv)),nl,
	eval(Expr,NewEnv,NewVals,Res).
eval(fun(F),_,_,FunDef) :- function(F,FunDef).
eval(print(X),_,_,constr(true,[])) :-
   print(X),nl.

rename(Expr,_Env,RenExpr) :- RenExpr = Expr.

eval_fun(lambda(X,Expr),_,_,lambda(X,Expr)).
eval_fun(fun(F),_,_,FunDef) :- function(F,FunDef).
% TO DO: add let, if, ... if needed ???
% NOTE: eval_fun must provide a static last argument upon success

eval_if(Test, Then, _Else, Env,Vals, Res) :-
    test(Test,Env,Vals),
    !,
    eval(Then,Env,Vals,Res).

eval_if(_Test,_Then,Else, Env,Vals, Res) :-
    eval(Else,Env,Vals,Res).

test(eq(X,Y), Env,Vals) :-
    eval(X,Env,Vals,VX),
    eval(Y,Env,Vals,VX).



function(fib, lambda(x,
                     if(eq(var(x),cst(0)),
                         cst(1),
                         if(eq(var(x),cst(1)),
                            cst(1),
                            plus(apply(minus(var(x),cst(1)),fun(fib)),
                                 apply(minus(var(x),cst(2)),fun(fib)))
                            )
                        )
                    )).
     
%:- mode store(i,i,i,o).

store([],[],Key,Value,[Key],[Value]).
store([Key|T1],[_Value2|T2],Key,Value,[Key|T1],[Value|T2]).
store([Key2|T1],[Value2|T2],Key,Value,[Key2|BT1],[Value2|BT2]) :-
   Key \== Key2,
   store(T1,T2,Key,Value,BT1,BT2).

%:- mode lookup(i,i,o).

lookup(Key,[Key|_T],[Value|_],Value).
lookup(Key,[Key2|T1],[_Value2|T2],Value) :-
   Key \== Key2,
   lookup(Key,T1,T2,Value).
   
   

fib(X,FibX) :-
                store([],[],xx,constr(X,[]),Env,Vals),
                %print(fib(Env,Vals)),nl,
                eval(apply(var(xx),fun(fib)),Env,Vals,constr(FibX,_)).
              %  eval(apply(cst(X),fun(fib)),Env,Vals,constr(FibX,_)).

bench(Low,Up) :- Low>Up, print('Done'),nl.

bench(Low,Up) :-
    Low =< Up, 
    print(fib(Low)), print(' == '), 
    statistics(runtime,[_,_]),
    fib(Low,Res),!,statistics(runtime,[_,T]),print(Res), print(' : time = '),
    print(T),nl,
    L1 is Low+1,
%    print('debug'),
    bench(L1,Up).



%:- bench(15,20).
bta_entry(bench,2,[d,d]).

