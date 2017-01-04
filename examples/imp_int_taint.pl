/* A BTI (Binding Time Improved) version of imp_int.pl with tainting */

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


unwrap(int(X,T),int,X,T).
unwrap(string(X,T),string,X,T).

get_taint(int(_,T),T).
get_taint(string(_,T),T).

combine_taint(safe,X,X).
combine_taint(unsafe,_,unsafe).

check_safe(safe).
check_safe(unsafe) :- format('***** UNSAFE *******~n~n',[]).

'$MEMOANN'(eval,4,[s,s,d,d]).
'$MEMOANN'(eint,5,[s,s,d,d,d]).
'$UNFOLDCALLS'(lookup(K,Ks,_,_)) :- ground(K),ground(Ks).
'$UNFOLDCALLS'(store(Ks,_,K,_,_,_)) :- ground(K),ground(Ks).

eval(X,_InK,_InV,Res) :- number(X),Res=int(X,safe).
eval(X,_InK,_InV,Res) :- atom(X),Res=string(X,safe).
eval('$'(X),InK,InV,Res) :- lookup(X,InK,InV,Res).
eval('$_GET'(X),InK,InV,Res) :- lookup(X,InK,InV,Res).
eval('+'(X,Y),InK,InV,int(Res,TRes)) :-
   eval(X,InK,InV,RX),
   eval(Y,InK,InV,RY), 
   unwrap(RX,int,IX,TX),
   unwrap(RY,int,IY,TY),
   combine_taint(TX,TY,TRes),
   Res is IX+IY.
eval('++'(X,Y),InK,InV,string(Res,TRes)) :-
   eval(X,InK,InV,RX),
   eval(Y,InK,InV,RY), 
   unwrap(RX,string,SX,TX),
   unwrap(RY,string,SY,TY),
   combine_taint(TX,TY,TRes),
   atom_concat(SX,SY,Res).
eval('url_encode'(X),InK,InV,string(SX,safe)) :- eval(X,InK,InV,RX), 
   unwrap(RX,string,SX,_TX).

%eint(Stmt,InK,InV,_,_) :- format('--> ~w  (~w / ~w)~n',[Stmt,InK,InV]),fail.
eint(':='(X,V),InK,InV,OutK,OutV) :- eval(V,InK,InV,Res),store(InK,InV,X,Res,OutK,OutV).
eint('def'(X),InK,InV,OutK,OutV) :- def(InK,InV,X,OutK,OutV).
eint((X;Y),InK,InV,OutK,OutV) :-
   eint(X,InK,InV,InK2,InV2),
   eint(Y,InK2,InV2,OutK,OutV).
eint('echo'(X),InK,InV,InK,InV) :- eval(X,InK,InV,Res),
   unwrap(Res,string,S,T),
   format('ECHO (~w): ~w~n',[T,S]),
   check_safe(T).


xeint(if(BE,S1,S2),InK,InV,OutK,OutV) :-
      eval_be(BE,InK,InV,Res),
      (Res=true -> eint(S1,InK,InV,OutK,OutV) ; eint(S2,InK,InV,OutK,OutV)).

eval_be('='(X,Y),InK,InV,Res) :- eval(X,InK,InV,RX), eval(Y,InK,InV,RY),
      (RX=RY -> Res=true ; Res=false), print(eq(RX,RY,Res)),nl.
eval_be('<'(X,Y),InK,InV,Res) :- eval(X,InK,InV,RX), eval(Y,InK,InV,RY),
      (RX<RY -> Res=true ; Res=false).

% unsafe:
test1(X,RK,RV) :- eint( ('def'(x) ;
                   ':='(x,'$_GET'(input)) ;
                    def(z) ;
                    ':='(x,  '++'('$'(x) , ' <//b>')) ;
                    ':='(z ,'++'('<b> ','$'(x))) ;
                    'echo'('$'(z))
                    ) , [input],[string(X,unsafe)],RK,RV).

% safe:
test2(X,RK,RV) :- eint( ('def'(x) ;
                   ':='(x,'$_GET'(input)) ;
                    def(z) ;
                    ':='(x,  '++'('$'(x) , ' <//b>')) ;
                    ':='(z ,'++'('<b> ','$'(x))) ;
                    ':='(z ,url_encode('$'(z))) ;
                    'echo'('$'(z))
                    ) , [input],[string(X,unsafe)],RK,RV).

test3(X,RK,RV) :- eint( ('def'(x) ;
                   ':='(x,'$_GET'(input)) ;
                    def(z) ;
                    ':='(x, '$'(x)+1) ;
                    ':='(z ,'$'(x)+('$'(x)+2))
                    ) , [input],[int(X,unsafe)],RK,RV).

