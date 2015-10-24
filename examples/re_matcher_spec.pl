
/*  test3(A) :- test3__0(A). */
test3__0([a|A]) :-
        b\=a,
        match2__1(A,false).
test3__0([b|A]) :-
        a\=b,
        match2__2(A,false).
test3__0([A|B]) :-
        a\=A,
        b\=A,
        match2__2(B,false).

/*  match2(A,seq(char(a),1,char(b)),[true],B) :- match2__1(A,B). */
match2__1([],true).
match2__1([a|A],_) :-
        b\=a,
        match2__2(A,false).
match2__1([b|A],_) :-
        a\=b,
        match2__2(A,true).
match2__1([A|B],_) :-
        a\=A,
        b\=A,
        match2__2(B,false).

/*  match2(A,seq(char(a),1,char(b)),[false],B) :- match2__2(A,B). */
match2__2([],true).
match2__2([a|A],_) :-
        b\=a,
        match2__2(A,false).
match2__2([b|A],_) :-
        a\=b,
        match2__2(A,false).
match2__2([A|B],_) :-
        a\=A,
        b\=A,
        match2__2(B,false).
