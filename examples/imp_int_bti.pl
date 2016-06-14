/* A BTI (Binding Time Improved) version of imp_int.pl */

/* Store(OldEnv, VariableName, NewValue, NewEnv) */
store([],[],Key,Value,[Key],[Value]) :- 
   print(assigning_undefined_var(Key,Value)),nl.
store([Key|KT],[_Value2|VT],Key,Value,[Key|KT],[Value|VT]).
store([Key2|KT],[Value2|VT],Key,Value,[Key2|KT2],[Value2|VT2]) :-
   Key \== Key2,
   store(KT,VT,Key,Value,KT2,VT2).

/* lookup(VariableName, Env, CurrentValue) */
lookup(Key,[],_,_) :- print(lookup_var_not_found_error(Key)),nl,fail.
lookup(Key,[Key|_],[Value|_T],Value).
lookup(Key,[Key2|KT],[_Value2|VT],Value) :-
   Key \== Key2,lookup(Key,KT,VT,Value).

def(InK,InV,Key,[Key|InK],[undefined|InV]).



eval(X,_InK,_InV,Res) :- number(X),Res=X.
eval('$'(X),InK,InV,Res) :- lookup(X,InK,InV,Res).
eval('+'(X,Y),InK,InV,Res) :- eval(X,InK,InV,RX), eval(Y,InK,InV,RY), Res is RX+RY.

eint(':='(X,V),InK,InV,OutV,OutK) :- eval(V,InK,InV,Res),store(InK,InV,X,Res,OutV,OutK).
eint('def'(X),InK,InV,OutV,OutK) :- def(InK,InV,X,OutV,OutK).
eint((X;Y),InK,InV,OutV,OutK) :- eint(X,InK,InV,InK2,InV2), eint(Y,InK2,InV2,OutV,OutK).


eint(if(BE,S1,S2),InK,InV,OutV,OutK) :-
      eval_be(BE,InK,InV,Res),
      (Res=true -> eint(S1,InK,InV,OutV,OutK) ; eint(S2,InK,InV,OutV,OutK)).

eval_be('='(X,Y),InK,InV,Res) :- eval(X,InK,InV,RX), eval(Y,InK,InV,RY),
      (RX=RY -> Res=true ; Res=false), print(eq(RX,RY,Res)),nl.
eval_be('<'(X,Y),InK,InV,Res) :- eval(X,InK,InV,RX), eval(Y,InK,InV,RY),
      (RX<RY -> Res=true ; Res=false).

test2(X,RK,RV) :- eint( ('def'(x) ;
                   ':='(x,'$'(input)) ;
                    def(z) ;
                    ':='(x, '$'(x)+1) ;
                    ':='(z ,'$'(x)+('$'(x)+2))
                    ) , [input],[X],RK,RV).

%test3(R) :- eint( (def x; x:= 1; def z; if($x=1, z:=1, z:=2)), [],R).
%test4(X,R) :- eint( (def x; x:= X; def z; if($x=1, z:=1, z:=2)), [],R).
