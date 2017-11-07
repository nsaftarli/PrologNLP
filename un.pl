unify(void,void).
unify(tree(E,L1,R1),tree(E,L2,R2)) :- unify(L1,L2), unify(R1,R2).
