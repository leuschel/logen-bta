
test(X) :- p1(a,X).

p1(V,W) :- (V==a -> W=a ; W=b).