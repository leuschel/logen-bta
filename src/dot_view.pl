% Module to view Size-Change Graphs using DOT
/* (c) Michael Leuschel and German Vidal 2007-2015 */

:- module(dot_view,
 [print_dep_graph_for_dot/1, print_dep_graph_for_dot/0,
  print_scc_graph_for_dot/1, print_scc_graph_for_dot/0]).



:- use_module(prolog_reader).
:- use_module(library(ugraphs)).
:- use_module(size_change_analysis_without_labels_with_sccs).


print_dep_graph_for_dot(F) :-
   tell(F),(print_dep_graph_for_dot -> true ; true),told.



print_dep_graph_for_dot:-
    retractall(edge(_,_,_)),
    print_graph_header(dependency_graph),
    prolog_reader:get_clause(Call,Body,_Ref),
    %nl,print(prolog_reader:get_clause(Call,Body,_Ref)),
    %print(user_output,clause(Call,Body)),nl(user_output),
    add_dep_graph(Body,Call),
    fail.
print_dep_graph_for_dot :-
    edge(PA,NA,PB,NB,Nr),
   print_pred(PA,NA),
   print(' -> '), 
   print_pred(PB,NB),
   functor(B,PB,NB),
   (prolog_reader:is_user_pred(B)
    -> (Nr=1 -> print(' [color=blue]') ;
           print(' [color=blue,label='), print(Nr), print(']'))
    ;  print(' [color=gray,style=dotted]')),
    nl,
   fail.
print_dep_graph_for_dot :- print_graph_footer.

:- dynamic edge/5.

%add_dep_graph(X,_A) :- print(user_output,X),nl(user_output),fail.
add_dep_graph(true,_A) :- !.
add_dep_graph((B,C),A) :- !,
    (add_dep_graph(B,A) ;
    add_dep_graph(C,A)).
add_dep_graph((B;C),A) :- !,
    (add_dep_graph(B,A) ;
    add_dep_graph(C,A)).
add_dep_graph((B->C),A) :- !,
    (add_dep_graph(B,A) ;
    add_dep_graph(C,A)).
add_dep_graph(X,A) :- meta_predicate(X,InnerCall),!,
    add_dep_graph(InnerCall,A).
add_dep_graph(B,A) :- add_edge(A,B).

add_edge(A,B) :-  functor(B,PB,NB), B\= ('='), 
  functor(A,PA,NA),
  (retract(edge(PA,NA,PB,NB,Nr))
    -> N1 is Nr+1 ; N1 = 1),
  assert(edge(PA,NA,PB,NB,N1)).
   
print_pred('!',0) :- !, print('CUT'). 
print_pred('=<',2) :- !, print('LEQ'). 
print_pred('<',2) :- !, print('LT'). 
print_pred('>',2) :- !, print('GT').
print_pred('>=',2) :- !, print('GEQ'). 
print_pred('=',2) :- !, print('EQ').
print_pred('=..',2) :- !, print('EQ_DOT_DOT').
print_pred('==',2) :- !, print('EQ_EQ').
print_pred('\\=',2) :- !, print('NEQ').
print_pred('\\==',2) :- !, print('NEQ_EQ').
print_pred(P,N) :- 
  print(P),print('_'),print(N).
   
meta_predicate(\+(X),X).
meta_predicate(findall(_,PX,_),X) :- peel_prolog_reader(PX,X).
meta_predicate(call(X),X).
meta_predicate(when(_Cond,X),X).
peel_prolog_reader(PX,R) :- nonvar(PX),prolog_reader:X=PX,!,R=X.
peel_prolog_reader(X,X).
/* ---------------------------------------------------------------------- */   


print_graph_header(Type) :-
    print('digraph '), print(Type), print(' {'),nl,
   % print('graph [orientation=landscape, page="8.5, 11",ratio=fill,size="7.5,10"];'),nl,
    print('graph [page="8.5, 11",ratio=fill,size="7.5,10"];'),nl.

print_graph_footer :-  print('}'),nl.   

/**************************************************************************/

print_scc_graph_for_dot(F) :-
   tell(F),(print_scc_graph_for_dot -> true ; true),told.

print_scc_graph_for_dot :-
    print_graph_header(dependency_graph),
    call_graph(CGraph),
    edges(CGraph,Edges),
    member((P,N)-(Q,M),Edges),
    print(P),print('_'),print(N), 
    print(' -> '), 
    print(Q),print('_'),print(M),
    print(' [color=blue]'),nl,
    fail.
print_scc_graph_for_dot :- print_graph_footer.

