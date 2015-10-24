/* A Binding-Time for Logen */
/* developed by Michael Leuschel and German Vidal */
/* (c) Michael Leuschel and German Vidal 2007-2015 */

:- module(sca_wo_labels_with_sccs_simpler,[size_change_analysis_without_labels/0,
                                call_is_not_terminating/3,
                                compute_global_binding_times_without_labels/4,
                                call_graph/1,sccs/1]).

/* --------------------------------- */

/* SIZE CHANGE ANALYSIS */
/*
   output of the analysis: is_not_terminating_without_labels(Pred,Arity,Pattern) 
                    + more precise version: pattern_is_not_terminating
                           compute_global_binding_times_without_labels(Pred,Arity,CallPat,Pattern) 
   for each user's predicate with risk of non-termination

   NOTE: if there is no fact associated to some predicate, then this 
         predicate can be safely unfolded...

   - the facts 'is_not_terminating_without_labels' can be used in the BTA for ensuring 
     local termination
   - the predicate 'compute_global_binding_times_without_labels' can be used to generalize dynamic 
     arguments in order to ensure the global termination

     NOTE: one should also generalize all dynamic repeated variables by 
           one in the global level
*/

call_is_not_terminating(Fun,Arity,CallPattern) :-
  clean_up_call_pattern(CallPattern,CCP), % generalise information that we cannot use, nv -> d,...
 % (Fun=remove_channels -> print(checking(Fun,Arity,CallPattern,CCP)),nl ; true),
  pattern_is_not_terminating(Fun,Arity,CCP),
  ((user:verbose,CCP \= CallPattern,\+ pattern_is_not_terminating(Fun,Arity,CCP))
    -> print(not_terminating(Fun,Arity,CallPattern,CCP)),nl ; true).

clean_up_call_pattern([],[]).
clean_up_call_pattern([H|T],[CH|CT]) :- clean_up_bt(H,CH),clean_up_call_pattern(T,CT).
clean_up_bt(s,R) :- !,R=s.
clean_up_bt(d,R) :- !,R=d.
clean_up_bt(list,R) :- user:cli_option(allow_list_length_norm),!,R=s. % TO DO: we should check if that argument was really analysed using list-length !!
clean_up_bt(list_nv,R) :- user:cli_option(allow_list_length_norm),!,R=s. % TO DO: we should check if that argument was really analysed using list-length !!
clean_up_bt(_,d). % do not know this binding time -> assume it is dynamic



:- use_module(library(lists)).
%:- prolog_flag(compiling,_,profiledcode). 

:- use_module(dot_view).

vprint(X) :- (user:verbose -> print(X) ; true).
vprintln(X) :- (user:verbose -> print(X),nl ; true).
vvprintln(X) :- (user:cli_option(very_verbose) -> (print(X),nl) ; true).

:- use_module(library(ugraphs)).

:- dynamic call_graph/1. %% stores the final (simplifies) call graph
:- dynamic sccs/1.       %% stores the nontrivial strongly connected components
:- dynamic graph_edge/4. %% stores the edges of call_graph (useful to get better indexing)

create_sccs :-
  findall(A-B,dependency(A,B),List),
  assert_self_loops(List),
  add_edges([],List,InitialGraph),
  %%assert(call_graph(InitialGraph)),
  reduce(InitialGraph,SCCs),
  vertices(SCCs,Vertices),
  findall(SelfPred,self_looping_pred(SelfPred),List2),
  remove_non_recursive_preds(Vertices,List2,Vertices2),
  %%nl,print(Vertices2),nl,
  assert(sccs(Vertices2)),
  edges(InitialGraph,Edges),
  findall(P-Q,(member(SCC,Vertices2),member(P,SCC),member(P-Q,Edges),member(Q,SCC)),FinalEdges),
  assert_edges(FinalEdges),
  add_edges([],FinalEdges,FinalGraph),
  assert(call_graph(FinalGraph)).

assert_edges([]).
assert_edges([(P,N)-(Q,M)|R]) :- assert(graph_edge(P,N,Q,M)),assert_edges(R).

dependency(CallPred,BodyAtomPred) :-
  prolog_reader:get_clause(Call,Body,_Ref),
  Body\==true,
  occurs(Body,BodyAtom),
  %nl,print(occurs(Body,BodyAtom)),
  functor(Call,P,N),
  functor(BodyAtom,Q,M),
  CallPred = (P,N),
  BodyAtomPred = (Q,M).

%occurs(true,_) :- fail.
occurs((B,C),A) :- occurs(B,A) ; occurs(C,A).
occurs((B;C),A) :- occurs(B,A) ; occurs(C,A).
occurs((B->C),A) :- occurs(B,A) ; occurs(C,A).
occurs(M,A) :- meta_predicate(M,InnerCall),
    occurs(InnerCall,A).
occurs(A,A) :- prolog_reader:is_user_pred(A),functor(A,P,_N),P\==(':').

remove_non_recursive_preds([],_,[]).
remove_non_recursive_preds([[P]|R],SelfLoopingPreds,[[P]|RR]) :- 
	member(P,SelfLoopingPreds), !, 
	remove_non_recursive_preds(R,SelfLoopingPreds,RR).
remove_non_recursive_preds([[_]|R],SelfLoopingPreds,RR) :- 
	!, remove_non_recursive_preds(R,SelfLoopingPreds,RR).
remove_non_recursive_preds([A|R],SelfLoopingPreds,[A|RR]) :- 
	!, remove_non_recursive_preds(R,SelfLoopingPreds,RR).

:- dynamic self_looping_pred/1.

assert_self_loops(List) :-
	member(A-A,List),
	assert(self_looping_pred(A)),
	fail.
assert_self_loops(_).

:- dynamic initial_node/1.
:- dynamic loop/1.

:- use_module(library(avl)).

assert_initial_nodes :-
    print('* Computing call graph *'),nl,
	call_graph(Graph),
	edges(Graph,Edges),
	vprintln(edges(Edges)),
    print('* Computing SCCs *'),nl,
	sccs(SCCs),
	vprintln(sccs(SCCs)),
    print('* Computing Loops *'),nl,
        member(SCC,SCCs),
	 member(V,SCC),
	mypath_to(V,Path,Edges,V),
	%%loop(Path,Loop),
        %\+((loop(Loop_),perm(Loop,Loop_))),
    sort(Path,ElementsOnPath),
    \+ loop(ElementsOnPath),
	assert(loop(ElementsOnPath)),
	vprintln(assert(loop(ElementsOnPath))),
	fail.
/*
	findall(Loop,(member(V,Vertices),mypath(V,Path,Edges),loop(Path,Loop)),AllLoops),
	nl,print(AllLoops),nl,
	remove_permutations(AllLoops,LL),
	nl,print(LL),nl,
	fail.
*/
assert_initial_nodes :-
	%findall(Loop,retract(loop(Loop)),LL),
	%sort(LL,LLS),
	%assert_loops(LLS),
	print('* Selecting nodes to cut loops *'),nl,
	select_nodes,
	(user:verbose ->
    nl,print('List of initial nodes'),nl,
    findall(Node,initial_node(Node),List),
    print(List),nl
    ;
    true).

/*
remove_permutations([],[]).
remove_permutations([L],[L]).
remove_permutations([P,Q|R],LL) :- perm(P,PP),
	member(PP,[Q|R]),!,remove_permutations([Q|R],LL).
remove_permutations([P,Q|R],[P|LL]) :- remove_permutations([Q|R],LL). */

mypath_to(Orig,Loop,Edges,Dest) :- empty_avl(VisitedAVL),
     mypath_to2(Orig,Loop,Edges,Dest,VisitedAVL).
%mypath_to(A,[A],Edges,Dest,_AVL) :- member(A-Dest,Edges).
mypath_to2(A,[A|B],Edges,Dest,AVL) :- 
	select(A-C,Edges,NewEdges),
	\+ avl_fetch(C,AVL), % check if we have already passed around this node: only detect simple loops
	(C=Dest -> B=[]
	        ;  avl_store(C,AVL,true,NewAVL),
	            mypath_to2(C,B,NewEdges,Dest,NewAVL)).


% determine a valid cover set (a set of predicates covering all loops) using a greedy algorithm
select_nodes :-
	loop(Loop),
	member(Pred,Loop),
	assert(initial_node(Pred)),
	print(selected(Pred)),nl,
	remove_loops(Pred),!,
	select_nodes.
select_nodes.
	
remove_loops(Pred) :-
	loop(Loop),
	member(Pred,Loop),
	retractall(loop(Loop)),
	%%nl,print(retractall(loop(Loop))),nl,
	fail.
remove_loops(_Pred).


size_change_analysis_without_labels :-
    print('Starting SIZE-CHANGE ANALYSIS'),nl,
    create_sccs,

    (user:cli_option(dependency_graph2(F))
      -> print_scc_graph_for_dot(F) ; true),

    (create_size_change_graphs -> vprintln('Created Size Change Graphs') 
     ; 
     (print('### Creating Size Change Graphs Failed !'),nl, fail)),

    %%trivial strategy: start from just one node of every SCC...
    (user:cli_option(use_sccs) -> assert_initial_nodes ; true),

     % generate all concatenation of size-change graphs until fixpoint is reached
    (all_concats -> true ; print('No Maximal Size Change Graphs'),nl), 
    nl,print('****************OUTPUT OF THE SIZE-CHANGE ANALYSIS*********************'),nl,
    assert_results,
    print('****************OUTPUT OF THE SIZE-CHANGE ANALYSIS*********************'),nl.
    

:- dynamic sc_graph_from/4.

/*
assert_sc_graph(sc(from(PA,NA),TO,A)) :- A=arrows(A1),!, %print(assert_sc_graph(PA,NA,TO,A1)),nl,
   ((sc_graph_from(PA,NA,TO,StoredArrows),StoredArrows=arrows(A2),compare_arrows(A1,A2,W))
     -> ((W=weaker,A1\=A2) -> retract(sc_graph_from(PA,NA,TO,StoredArrows)),
                vprintln('% Removed stronger stored graph'),vprint_a_graph(sc(from(PA,NA),TO,StoredArrows)),
                assert(sc_graph_from(PA,NA,TO,A)), 
                vprint('% NEW Weaker: '),vprint_a_graph(sc(from(PA,NA),TO,A))
             ;  vprintln('% SAME graph already exists'))
     ; (assert(sc_graph_from(PA,NA,TO,A)),
        vprint('% NEW: '),vprint_a_graph(sc(from(PA,NA),TO,A)))
    ).
*/
assert_sc_graph(sc(from(PA,NA),TO,A)) :- 
    (sc_graph_from(PA,NA,TO,A) -> vprintln('% SAME graph already exists')
     ; assert(sc_graph_from(PA,NA,TO,A)),
       vprint('% NEW: '),
       vprint_a_graph(sc(from(PA,NA),TO,A))
    ),!.
assert_sc_graph(SC) :- print('### Unknown SC in assert: '), print(SC),nl.

retract_sc_graph(sc(from(PA,NA),TO,A)) :- !,
   retract(sc_graph_from(PA,NA,TO,A)).
retract_sc_graph(SC) :- print('### Unknown SC in retract: '), print(SC),nl.

sc_graph(sc(from(PA,NA),TO,A)) :- !, sc_graph_from(PA,NA,TO,A).
sc_graph(SC) :- print('### Unknown SC sc_graph: '), print(SC),nl.


compare_arrows(A1,A2,Res) :- !, %print(compare_arrows(A1,A2,Res)),nl,
  (weaker_arrows(A1,A2) -> Res=weaker ; (weaker_arrows(A2,A1) -> Res=stronger ; fail)).
  %,print(res(Res)),nl.
  
weaker_arrows(A1,A2) :- var(A2),!,A2=A1.
weaker_arrows(A1,A2) :- weaker_arrows1(A1,A2). % -> print(weaker(A1,A2)),nl ; print(not_weaker(A1,A2)),fail.
weaker_arrows1([],_). /* no relationship weaker than anything else */
weaker_arrows1([(FromArgNr,ToArgNr,G_GEQ)|T],A2) :- wa2(A2,FromArgNr,ToArgNr,G_GEQ,T).

wa2([],_,_,_,_) :- fail.
/*
wa2([H|Rest2], FromArgNr, ToArgNr, G_GEQ, Rest1) :-
   (H @> (FromArgNr,ToArgNr,G_GEQ)
     -> wa2(Rest2,FromArgNr, ToArgNr, G_GEQ, Rest1)
     ;  H = (FromArgNr,ToArgNr,G2),
        weaker(G_GEQ,G2),
        weaker_arrows1(Rest1,Rest2)
   ).
*/
wa2([H|Rest2], FromArgNr, ToArgNr, G_GEQ, Rest1) :-
   (H \= (FromArgNr,ToArgNr,_)
     -> wa2(Rest2,FromArgNr, ToArgNr, G_GEQ, Rest1)
     ;  H = (FromArgNr,ToArgNr,G2),
        weaker(G_GEQ,G2),
        weaker_arrows1(Rest1,Rest2)
   ).


weaker(geq,_).
weaker(g,g).

% ---------------

:- dynamic defined_predicate/2.
assert_defined_predicate(P,N) :-
   (defined_predicate(P,N) -> true ; assert(defined_predicate(P,N))).

create_size_change_graphs:- retractall(defined_predicate(_,_)),
    prolog_reader:get_clause(Call,Body,_Ref),
    functor(Call,P,N), assert_defined_predicate(P,N),
    %nl,print(prolog_reader:get_clause(Call,Body,_Ref)),
    vprintln((Call:-Body)),
    add_sc_graphs(Body,Call),
    fail.
create_size_change_graphs.

add_sc_graphs(true,_A) :- !.
add_sc_graphs((B,C),A) :- !,
    add_sc_graphs(B,A),
    add_sc_graphs(C,A).
add_sc_graphs((B;C),A) :- !,
    add_sc_graphs(B,A),
    add_sc_graphs(C,A).
add_sc_graphs((B->C),A) :- !,
    add_sc_graphs(B,A),
    add_sc_graphs(C,A).
add_sc_graphs(X,A) :- meta_predicate(X,InnerCall),!,
    add_sc_graphs(InnerCall,A).
add_sc_graphs(B,A) :- vvprintln('->'(A,B)),
   prolog_reader:is_user_pred(B),
   A =.. [PA|AArgs],length(AArgs,NA),
   B =.. [PB|BArgs],length(BArgs,NB),
   %% new condition: A->B must belong to the simplified call graph
   (user:cli_option(use_sccs) -> graph_edge(PA,NA,PB,NB); true),
   !,
   (
    NA=0,!,
    assert_sc_graph(sc(from(PA,NA),to(PB,NB),arrows([])))
   ;
    NB=0,!,
    assert_sc_graph(sc(from(PA,NA),to(PB,NB),arrows([])))
   ;
    A =.. [PA|AArgs],length(AArgs,NA),
    B =.. [PB|BArgs],length(BArgs,NB),
    %nl,print(addsc(1,AArgs,BArgs,LL)),nl,
    addsc(1,AArgs,BArgs,LL),
    sort(LL,SLL),
    assert_sc_graph(sc(from(PA,NA),to(PB,NB),arrows(SLL)))
    %,nl,print(assert(sc_graph(sc(from(PA,NA),to(PB,NB),arrows(LL)))))
   )
   ;
   true.
   
meta_predicate(\+(X),X).
meta_predicate(findall(_,PX,_),X) :- peel_prolog_reader(PX,X).
meta_predicate(call(X),X).
meta_predicate('$MEMO'(X),X).
meta_predicate('$UNFOLD'(X),X).
meta_predicate(when(_Cond,X),X).
meta_predicate(pp_cll(X),X). % PROB self-check meta-predicates
meta_predicate(pp_mnf(X),X).
meta_predicate(pp_cll(X),X).
meta_predicate(mnf(X),X).

peel_prolog_reader(PX,R) :- nonvar(PX),prolog_reader:X=PX,!,R=X.
peel_prolog_reader(X,X).

addsc(_,[],_,[]).
addsc(NA,[A|AArgs],BArgs,RR) :- 
     addsc_(NA,A,1,BArgs,L),
     NNA is NA+1,
     addsc(NNA,AArgs,BArgs,LL),
     append(L,LL,RR).

addsc_(_,_,_,[],[]).
addsc_(NA,A,NB,[B|BArgs],L) :-
     NNB is NB+1,
     addsc_(NA,A,NNB,BArgs,LL),
     (can_compare_as_lists(A,B)
      % TODO: compare that we later do not combine this list_length link with a termsize link !!!!
 	  -> sym_list_size(A,ASze), sym_list_size(B,BSze) , vvprintln(list_comp(A,B,ASze,BSze))
 	  ;  sym_term_size(A,ASze), sym_term_size(B,BSze)),
     ( g_sym(ASze,BSze) -> L = [(NA,NB,g)|LL]
	  ; geq_sym(ASze,BSze) -> L = [(NA,NB,geq)|LL]
	  ; L = LL
     ).

can_compare_as_lists(A,B) :- user:cli_option(allow_list_length_norm),
        (var(A);is_a_list(A)),
	    (var(B);is_a_list(B)).

is_a_list([]).
is_a_list([_|T]) :- (var(T) -> true ; is_a_list(T)).

%% sym_list_size computes the symbolic list-length norm of a term
%% - a symbolic size 4+2X+Y is represented by (4,[(X,2),(Y,1)])

sym_list_size(T,R) :-
    (
     var(T),!,
     R = (0,[(T,1)])
    ;
     T = [],!,
     R = (0,[])
    ;
     T = [_|Rest],
     sym_list_size(Rest,(K,L)),
     M is K+1,
     R = (M,L)
    ).

%% sym_term_size computes the symbolic term size norm of a term
%% - a symbolic size 4+2X+Y is represented by (4,[(X,2),(Y,1)])

sym_term_size(T,R) :-
    (
     var(T),!,
     R = (0,[(T,1)])
    ;
     T =.. [_|Args],
     length(Args,N),
     sym_term_size_args(Args,(K,L)),
     M is N+K,
     R = (M,L)
    ).

sym_term_size_args([],(0,[])).
sym_term_size_args([A|AR],R) :-
    sym_term_size_args(AR,S2),
    sym_term_size(A,S1),
    add_sym(S1,S2,R).


%% add_sym sums two symbolic term sizes:
%% - the first component is an integer
%% - the second component is a list of pairs (Var,Int)

add_sym((N,L),(M,R),(K,S)) :-
    K is N+M,
    add_vars(L,R,S).

add_vars([],R,R).
add_vars([A|RA],RB,R) :- 
    add_v(A,RB,RR),
    add_vars(RA,RR,R).

add_v((X,N),[],[(X,N)]).
add_v((X,N),[(Y,M)|R],S) :-
    (
     X==Y,!,
     K is N+M,
     S = [(X,K)|R]
    ;
     add_v((X,N),R,SS),
     S = [(Y,M)|SS]
    ). 


%% geq_sym succeeds when the first size is greater than or equal 
%% to the second size:

geq_sym((N,L),(M,R)) :-
    N>=M, 
    sort(L,LS),
    sort(R,RS),
    geq_sym_list(LS,RS).

geq_sym_list(_,[]).
geq_sym_list([(Y,M)|T],[(X,N)|R]) :-
    (X==Y,!,
     M>=N,
     geq_sym_list([(Y,M)|T],R)
    ;
     geq_sym_list(T,[(X,N)|R])
    ).


%% g_sym succeeds when the first size is strictly greater than 
%% the second size:

g_sym(A,B) :- g_sym_(A,B,false). %% the Boolean is used to check
                                 %% that there is at least one strict <

g_sym_((N,L),(M,R),B) :-
    (N>=M,NB=B
     ;
    N>M,NB=true),
    sort(L,LS),
    sort(R,RS),
    g_sym_list(LS,RS,NB).

g_sym_list(_,[],true).
g_sym_list([(Y,M)|T],[(X,N)|R],B) :-
    (X==Y,!,
     (M>=N,NB=B
      ;
      M>N,NB=true),
     g_sym_list([(Y,M)|T],R,NB)
    ;
     g_sym_list(T,[(X,N)|R],B)
    ).



%%just for debugging
print_graphs([]) :- !.
print_graphs([sc(from(P,N),to(Q,M),arrows(Ars))|R]) :- !, print('  '),
	print(P),print('/'),print(N),print(' --> '),
	print(Q),print('/'),print(M),print(': '),
	print_arrows(Ars),nl,
	print_graphs(R).
print_graphs(X) :- print('### could not print graph: '),nl,
   print(X),nl.

print_arrows([]).
print_arrows([(N,M,R)|L]) :-
%	print(N),print(M),print(R),print('-'),
	print(N),print_geq(R),print(M),print(' '),
	print_arrows(L).

print_geq(g) :- !, print('>').
print_geq(geq) :- !, print('>=').
print_geq(X) :- print(X).

:- dynamic i/1.
i(0). % iteration counter

assert_idempotent_graphs :-
   /* count(X), print(X), print(' superfluous idempotent graph(s) removed'),nl, */
   retractall(sc_graph_from(_,_,_,_)),
   vprintln('Asserting Idempotent Graphs: '), reset_count,
   print('Final idempotent size-change graphs: '),nl,
   retract(existing_sc_graph(_Hash,P,N,P,N,Arrows)),
   SC = sc(from(P,N),to(P,N),arrows(Arrows)),
   sc_concat(SC,SC,SC), /* it is idempotent */  %print('.'),
   assert_sc_graph(SC),
   print_graphs([SC]),
   inc_count,
   fail.
assert_idempotent_graphs :- count(X), print(X), print(' idempotent graphs'),nl.


vprint_a_graph(SC) :- (user:verbose -> print_graphs([SC]) ; true).

:- dynamic count/1.
reset_count :- retract(count(_)), assert(count(0)).
count(0).
inc_count :- retract(count(X)),X1 is X+1, assert(count(X1)).

% generate all concatenation of size-change graphs until fixpoint is reached
all_concats :-
    initialise_delta_sc_graphs,
    iterate.
    
iterate :- check_hashs,
    delta_sc_graph(ExistingGraph),
    treate_new_sc_graph(ExistingGraph),
    fail.
iterate :-
   (change_occured
      -> print('new iteration '),
          (user:verbose ->
                retract(i(N)),!,nl,nl,
                print('iteration '),print(N),M is N+1,assert(i(M)),nl
            ; true),
          move_new_sc_graphs,
          iterate
  ;  print('fixpoint reached'),nl,
     move_new_sc_graphs,
     (user:verbose -> count_total_nr_graphs ; true),
     assert_idempotent_graphs
  ).

count_total_nr_graphs :- print('Total number of graphs: '),reset_count,
   existing_sc_graph(_), inc_count, fail.
count_total_nr_graphs :- count(X),  print(X),nl,
    print('Number of hash collisions: '),reset_count,
    existing_sc_graph(Hash,P,N,Q,M,Arrows),
    existing_sc_graph(Hash,P2,N2,Q2,M2,Arrows2),
    t(P2,N2,Q2,M2,Arrows2) @> t(P,N,Q,M,Arrows),
    inc_count,fail.
count_total_nr_graphs :- count(X),  print(X),nl.

/* this is probably not required: sufficient to append new SCGraph to existing ones */
treate_new_sc_graph(SCGraph) :-  vprint_a_graph(SCGraph),
   SCGraph = sc(_,to(P,N),_), 
   sc_graph_from(P,N,TO,SA), ExistingGraph = sc(from(P,N),TO,SA),
   sc_concat(SCGraph,ExistingGraph,NewGraph),
   (sc_graph_exists(NewGraph) -> fail
      ; assert_new_sc_graph(NewGraph)
   ),
   fail.
 /*
treate_new_sc_graph(SCGraph) :- 
   SCGraph = sc(from(P,N),_,_), 
%   ExistingGraph = sc(_,to(P,N),_),sc_graph_from(PA,NA,to(P,N),StoredArrows), ExistingGraph = sc(from(PA,NA),to(P,N),StoredArrows),
   sc_graph_to(P,N,From,StoredArrows),
   ExistingGraph = sc(From,to(P,N),StoredArrows),
 %  to_to_hash(P,N,Hash),
 %  (existing_sc_graph(Hash,ExistingGraph) ; delta_sc_graph(Hash,ExistingGraph)),
   sc_concat(ExistingGraph,SCGraph,NewGraph),
   (sc_graph_exists(NewGraph) -> fail
      ; assert_new_sc_graph(NewGraph)
   ),
   fail. */
treate_new_sc_graph(_).

:- dynamic existing_sc_graph/6. /* already treated SC graphs */
:- dynamic delta_sc_graph/6.  /* the ones to be treated in this iteration */
:- dynamic new_sc_graph/6.   /* new ones found */
:- use_module(library(terms),[term_hash/2]).
sc_graph_exists(SC) :- term_hash(SC,Hash),
    (existing_sc_graph(Hash,P,N,Q,M,Arrows) 
     ; delta_sc_graph(Hash,P,N,Q,M,Arrows) 
     ; new_sc_graph(Hash,P,N,Q,M,Arrows)),
    SC = sc(from(P,N),to(Q,M),arrows(Arrows)).
existing_sc_graph(sc(from(P,N),to(Q,M),arrows(Arrows))) :-
   existing_sc_graph(_Hash,P,N,Q,M,Arrows).
%existing_sc_graph(Hash,sc(from(P,N),to(Q,M),arrows(Arrows))) :-
%   existing_sc_graph(Hash,P,N,Q,M,Arrows).
delta_sc_graph(sc(from(P,N),to(Q,M),arrows(Arrows))) :-
   delta_sc_graph(_Hash,P,N,Q,M,Arrows).
%delta_sc_graph(Hash,sc(from(P,N),to(Q,M),arrows(Arrows))) :-
%   delta_sc_graph(Hash,P,N,Q,M,Arrows).
%new_sc_graph(sc(from(P,N),to(Q,M),arrows(Arrows))) :-
%   new_sc_graph(_Hash,P,N,Q,M,Arrows).
 
initialise_delta_sc_graphs :- 
 (user:verbose -> nl,print('Initial delta graphs:'),nl ; true),
 sc_graph(SC),
 SC=sc(from(P,N),_To,_Arrows),
 %% new condition: the initial node should be one of the selected nodes
 (user:cli_option(use_sccs) -> initial_node((P,N)) ; true),
 assert_delta_sc_graph(SC), %print('.'),
 (user:verbose -> print_graphs([SC]) ; true), %print('.'),
 fail.
initialise_delta_sc_graphs :- nl. 
 
:- dynamic from_to_hash/3.  %% is slightly beneficial for imperative_power, not so good for e.g. picemul; maybe also add a hash_to_from database to check if we need to assert new entries
:- dynamic hash_to_from/3.
assert_sc_graph_exists(SC) :- SC = sc(from(P,N),to(Q,M),arrows(Arrows)),
   (sort(Arrows,Arrows) -> true ; print(not_sorted_exists(Arrows))),
   term_hash(SC,Hash),
   assert(existing_sc_graph(Hash,P,N,Q,M,Arrows)).
assert_delta_sc_graph(SC) :- SC = sc(from(P,N),to(Q,M),arrows(Arrows)),
   (sort(Arrows,Arrows) -> true ; print(not_sorted_delta(Arrows))),
   term_hash(SC,Hash), assert_delta_sc_graph(Hash,P,N,Q,M,Arrows).
assert_delta_sc_graph(Hash,P,N,Q,M,Arrows) :- 
   assert(delta_sc_graph(Hash,P,N,Q,M,Arrows)),
   (hash_to_from(P,N,Hash) -> true ; (assert(hash_to_from(Hash,P,N)),assert(from_to_hash(P,N,Hash)))).
assert_new_sc_graph(SC) :- SC = sc(from(P,N),to(Q,M),arrows(Arrows)),
   (sort(Arrows,Arrows) -> true ; print(not_sorted_new(Arrows))),
   term_hash(SC,Hash), % print('.'),
   assert(new_sc_graph(Hash,P,N,Q,M,Arrows)).
move_new_sc_graphs :- reset_count,
   retract(delta_sc_graph(Hash,P,N,Q,M,Arrows)),
   assert(existing_sc_graph(Hash,P,N,Q,M,Arrows)), inc_count,
   fail.
move_new_sc_graphs :-  count(X),print('copied '), print(X), print(' delta graph(s) and '),
   reset_count,
   retract(new_sc_graph(Hash,P,N,Q,M,Arrows)),
   %remove_redundant_graphs(Hash,P,N,Q,M,Arrows),
   assert_delta_sc_graph(Hash,P,N,Q,M,Arrows),
   inc_count,
   fail.
move_new_sc_graphs :- count(X), print(X), print(' new graph(s)'),nl.

check_hashs :- user:verbose,
    print('Checking Hash'),nl,
    (existing_sc_graph(Hash,P,N,_Q,_M,_) ; delta_sc_graph(Hash,P,N,_Q,_M,_)),
    (from_to_hash(P,N,Hash) -> true ; print(no_from_to_hash(P,N,Hash)),nl),
    (hash_to_from(Hash,P,N) -> true ; print(no_hash_to_from(Hash,P,N)),nl),
    fail.
check_hashs.    


remove_redundant_graphs(_Hash,P,N,Q,M,Arrows) :-
   (( from_to_hash(P,N,Hash2), existing_sc_graph(Hash2,P,N,Q,M,Arrows2),
      compare_arrows(Arrows,Arrows2,W))
    -> %print(compare_ex(W,P,N,Q,M,Arrows,Arrows2)),nl,
       (W=weaker -> retract_existing_sc_graph(Hash2,P,N,Q,M,Arrows2),
                    vprintln('% Removed redundant stronger existing graph'),
                    vprint_a_graph(sc(from(P,N),to(Q,M),arrows(Arrows2)))
                 ; fail)
    ;   (( from_to_hash(P,N,Hash2), delta_sc_graph(Hash2,P,N,Q,M,Arrows2),
           compare_arrows(Arrows,Arrows2,W))
         -> %print(compare_delta(W,P,N,Q,M,Arrows,Arrows2)),nl,
          (W=weaker -> retract_delta_sc_graph(Hash2,P,N,Q,M,Arrows2),
                    vprintln('% Removed redundant stronger delta graph'),
                    vprint_a_graph(sc(from(P,N),to(Q,M),arrows(Arrows2)))
                     ; fail)
         ;  true
        )
    ).

retract_existing_sc_graph(Hash2,P,N,Q,M,Arrows2) :-
   retract(existing_sc_graph(Hash2,P,N,Q,M,Arrows2)),!,
   ((existing_sc_graph(Hash2,P,N,_,_,_);delta_sc_graph(Hash2,P,N,_,_,_))
    -> true ; retract(from_to_hash(P,N,Hash2)),retract(hash_to_from(Hash2,P,N))).
retract_delta_sc_graph(Hash2,P,N,Q,M,Arrows2) :-
   retract(delta_sc_graph(Hash2,P,N,Q,M,Arrows2)),!,
  ((existing_sc_graph(Hash2,P,N,_,_,_);delta_sc_graph(Hash2,P,N,_,_,_))
   -> true ; retract(from_to_hash(P,N,Hash2)),retract(hash_to_from(Hash2,P,N))).
   
change_occured :- new_sc_graph(_,_,_,_,_,_).
 
%% sc_concat concatenates two size_change graphs

sc_concat(sc(From,to(Q,M),arrows(Arrows1)),
          sc(from(Q,M),To,arrows(Arrows2)),
          sc(From,To,arrows(NewArrows))) :-
    findall((A,C,G3),(member((A,B,G1),Arrows1),member((B,C,G2),Arrows2),  /*using ord_member does not speed up (slightly slower for imperative_power) */
                     combine(G1,G2,G3)),Arrows_),
    sort(Arrows_,Arrows3),
   %%Arrows3 = NewArrows.
   remove_redundant_arrows(Arrows3,NewArrows), !.

combine(g,g,g).
combine(g,geq,g).
combine(geq,g,g).
combine(geq,geq,geq).

/* assumes g @< geq */
remove_redundant_arrows([],[]).
remove_redundant_arrows([(From,To,G)|T],[(From,To,G)|R]) :- 
  (G=g -> rra(T,From,To,R) ; remove_redundant_arrows(T,R)).

rra([],_,_,[]).
rra([(NewFrom,NewTo,NewG)|T],From,To,R) :-
   ( (NewTo=To,From=NewFrom) /* the new arrow is redundant */
     -> %print(removed(NewFrom,NewTo,NewG)),nl,
        remove_redundant_arrows(T,R)
     ;  R=[(NewFrom,NewTo,NewG)|RR],
        (NewG=g -> rra(T,NewFrom,NewTo,RR) ; remove_redundant_arrows(T,RR))
   ).

%ord_member(From,To,Order,[(F1,T1,O1) | RestArrows]) :-
%  ( (From = F1, To=T1, Order=O1)
%   ; (From @>= F1, ord_member(From,To,Order, RestArrows))  ).
%%

:- dynamic is_not_terminating_without_labels/3.
:- dynamic global_binding_times/4.

assert_results :- defined_predicate(Pred,N),
    vprintln(analysing(Pred,N)),
    (sc_graph(sc(from(Pred,N),TO,LLL)) -> true
      ; vprintln(' -> no idempotent graphs (terminating)'),fail),
    (TO=to(Pred,N) -> true
            ; print('### not idempotent sc_graph: '),print(sc(from(Pred,N),TO,LLL)),nl),
    %findall(Arrows,retract_sc_graph(sc(from(Pred,N),_,arrows(Arrows))),List),
    findall(Arrows,sc_graph(sc(from(Pred,N),_,arrows(Arrows))),List),
    List\=[],
    assert_unfolding_patterns(Pred,N,List,LocalPat),
    (user:cli_option(show_terminating_patterns) -> exhaustive_analysis(Pred,N) ; true),
    %print(local_pat(Pred,N,LocalPat)),nl,
    gfp_global_pattern(Pred,N,LocalPat,List,_CurGlobalPat,FixPointGlobalPat),
    /* actually not that useful to compute the best FixPointGlobalPat; as
       propagation has to start again from actual pattern in PE;
       currently this is only used to store the List of arrows for later retrieval */
    assert(global_binding_times(Pred,N,FixPointGlobalPat,List)),
    portray_clause(global_binding_times(Pred,N,FixPointGlobalPat)),
    %%assert_patterns(P,N,N,List,[],[]),
    fail.
assert_results.

% perform an exhaustive analysis of all possible calling patterns:
exhaustive_analysis(Pred,N) :- print('PREDICATE: '),print(Pred/N),nl,
   length(Pat,N), sd_list(Pat), print(' '), print(Pat), print(' --> '),
   (pattern_is_not_terminating(Pred,N,Pat) -> print('not terminating')
    ; is_not_terminating_without_labels(Pred,N,Pat) -> print('**TERMINATING**')
    ;  print('TERMINATING')),
   nl,fail.
exhaustive_analysis(_,_).
sd_list([]). sd_list([H|T]) :- (H=s ; H=d), sd_list(T).

% check if a call pattern is possibly not terminating:
pattern_is_not_terminating(Pred,N,Pattern) :- static_positions(Pattern,1,StaticPos),
   pattern_is_not_terminating2(StaticPos,N,Pred).

% simply compute the number of the static arguments and put them into a list:
static_positions([],_,[]).
static_positions([s|T],N,[N|ST]) :- !, N1 is N+1, static_positions(T,N1,ST).
static_positions([_|T],N,ST) :- !, N1 is N+1, static_positions(T,N1,ST).

pattern_is_not_terminating2(StaticPos,N,Pred) :-
   sc_graph(sc(from(Pred,N),_,arrows(Arrows))), % there exists an idempotent SC graph for Pred/N
   \+ decrease(Arrows,StaticPos). % such that there is no decrease of any static argument

% check if a static argument decreases
decrease([(CurPos,CurPos,g)|_T],StaticPos) :- member(CurPos,StaticPos),!.
decrease([_|T],StaticPos) :- decrease(T,StaticPos).

   
/* code should also work for N=0 */
assert_unfolding_patterns(Pred,0,ListOfArrows,Res) :- !,
   Res=[],
   (ListOfArrows=[]
     -> /* no maximal size_change_graph: ok to unfold */
         print(is_terminating(Pred,0)),nl
     ;   assert(is_not_terminating_without_labels(Pred,0,[])),
         portray_clause(is_not_terminating(Pred,0,[]))
   ).
assert_unfolding_patterns(Pred,N,ListOfArrows,Res) :-
    assert_unfolding_patterns2(Pred,N,N,ListOfArrows,[],Res).

assert_unfolding_patterns2(Pred,N,CurPos,ListOfArrows,LocalPat,Res) :-
    (member_all(ListOfArrows,(CurPos,CurPos,g)) /* standard termination condition */
      -> NewLocalPat = [d|LocalPat]
      ;  NewLocalPat = [_|LocalPat]),
     (CurPos=<1
      -> assert(is_not_terminating_without_labels(Pred,N,NewLocalPat)),
         portray_clause(is_not_terminating(Pred,N,NewLocalPat)),
         Res =  NewLocalPat
      ; NewPos is CurPos-1,
        assert_unfolding_patterns2(Pred,N,NewPos,ListOfArrows,NewLocalPat,Res)
     ).

/* iterate global pattern calculation until a fixpoint is reached */
gfp_global_pattern(Pred,N,LocalPat,ListOfArrows,CurGlobalPat,FixPointGlobalPat) :-
    %print(new_iteration(Pred,N,LocalPat,ListOfArrows)),nl,
    iterate_global_pattern(LocalPat,CurGlobalPat,1,ListOfArrows,NewCurGlobalPat),
    %print(iterate(Pred,N,NewCurGlobalPat)),nl,
    (CurGlobalPat = NewCurGlobalPat
      -> FixPointGlobalPat = CurGlobalPat
      ;  gfp_global_pattern(Pred,N,LocalPat,ListOfArrows,NewCurGlobalPat,FixPointGlobalPat)
    ).

iterate_global_pattern([],_,_,_LA,[]).
iterate_global_pattern([LocalPat|TLP],CurGlobalPat,CurPos,ListOfArrows,NewGlobalPat) :-
    ((LocalPat==d,is_static(CurPos,CurGlobalPat) ; /* then we have termination */
      member_all_dest(ListOfArrows,CurPos,CurGlobalPat))  /* quasi termination */
      -> NewGlobalPat = [s|TGlobalPat]
      ;  NewGlobalPat = [d|TGlobalPat]),
    CP1 is CurPos+1,
    iterate_global_pattern(TLP,CurGlobalPat,CP1,ListOfArrows,TGlobalPat).


member_all([],_).
member_all([L|RL],A) :-
    member(A,L),!,
    member_all(RL,A).

/* check whether there is one arrow, g or geq, to A in every idempotent size-change-graph,
   also this arrow needs to originate from an argument that is marked as static */
member_all_dest([],_,_).
member_all_dest([L|RL],A,CurGlobPat) :-
    member((From,A,_),L), is_static(From,CurGlobPat),!,  % we need to check that _From is static + iterate again when doing PE
    %print(ok(From,A,CurGlobPat)),nl,   
    member_all_dest(RL,A,CurGlobPat).

is_static(X,[H|T]) :- (X=1 -> H=s ; X1 is X-1, is_static(X1,T)).

:- dynamic computed_global_binding_times/4.
% generalising_pattern_to_ensure_quasi_termination
compute_global_binding_times_without_labels(Pred,Arity,CurrentPattern,FixPointGlobalPat) :-
   computed_global_binding_times(Pred,Arity,CurrentPattern,R),!,R=FixPointGlobalPat.
compute_global_binding_times_without_labels(Pred,Arity,CurrentPattern,FixPointGlobalPat) :-
    vprintln(generalising_pattern_to_ensure_quasi_termination(Pred,Arity,CurrentPattern)),
    %print(computing_global_binding_times(Pred,Arity,CurrentPattern)),nl,
    /* compute global_binding_time information based on current s/d pattern of PE */
    (\+ call_is_not_terminating(Pred,Arity,CurrentPattern)
     ->  FixPointGlobalPat = CurrentPattern, % no need to generalise, we have termination
         vprintln(pattern_is_terminating)
     ; is_not_terminating_without_labels(Pred,Arity,LocalPat) -> 
        global_binding_times(Pred,Arity,_StoredGlobalPat,ListOfArrows), % just used to get the ListOfArrows
        gfp_global_pattern(Pred,Arity,LocalPat,ListOfArrows,CurrentPattern,FixPointGlobalPat),
        assert(computed_global_binding_times(Pred,Arity,CurrentPattern,FixPointGlobalPat)),
        vprintln(computed_global_binding_times(Pred,Arity, CurrentPattern,FixPointGlobalPat))
      ; print('### no is_not_terminating_without_labels fact for: '),
        print(Pred), print('/'), print(Arity),nl,
        % this predicate can always be safely unfolded -> also no need to generalise
        % can happen when a user forces a predicate to be memoed using hints.
        FixPointGlobalPat = CurrentPattern
    ).
    
    
    
%%not used
%member_some(A,[L|RL]) :-
%    member(A,L),!
%    ;
%    member_some(A,RL).

/* END OF SIZE CHANGE ANALYSIS */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


