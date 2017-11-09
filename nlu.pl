
/* Part 1: Basic atomics */
male(alan). male(bob). male(chris). male(dwight). male(ethan). male(fred). male(george). male(howard). male(ian). male(john). male(kevin). male(leroy).

female(anna). female(betty). female(cathy). female(debora). female(elle). female(fay). female(georgina). female(hallie). female(isabel). female(julie). female(kate).

person(alan). person(bob). person(chris). person(dwight). person(ethan). person(fred). person(george). person(howard). person(ian). person(john). person(anna). person(betty). person(cathy). person(debora). person(elle). person(fay). person(georgina). person(hallie). person(isabel). person(julie). person(kevin). person(kate). person(leroy).

home(alan,toronto). home(bob,toronto). home(chris,montreal). home(dwight,dryden). home(ethan,dryden). home(fred,newyork). home(george,chicago). home(howard,sydney). home(ian,dubai). home(john,bangkok). home(anna,toronto). home(betty,toronto). home(cathy,beijing). home(debora,montreal). home(elle,sarnia). home(fay,newyork). home(georgina,chicago). home(hallie,newyork). home(isabel,dubai). home(julie,bangkok).  home(kevin,montreal). home(kate,sarnia). home(leroy,chicago). 

city(toronto). city(ottawa). city(montreal). city(vancouver). city(sarnia). city(newyork). city(chicago). city(losangeles). city(sydney). city(dubai). city(bangkok). city(beijing). city(dryden).

country(canada). country(usa). country(australia). country(uae). country(thailand). country(china).


location(toronto,canada). location(montreal,canada). location(ottawa,canada). location(vancouver,canada). location(sarnia,canada). location(newyork,usa). location(chicago,usa). location(losangeles,usa). location(sydney,australia). location(dubai,uae). location(bangkok,thailand). location(beijing,china). location(dryden,canada).

population(toronto,2809000). population(ottawa,947031). population(montreal,1741000). population(sarnia,71594). population(vancouver,647540). population(newyork,8538000). population(chicago,2705000). population(losangeles,3976000). population(sydney,4029000). population(dubai,2866000). population(bangkok,8281000). population(beijing,21500000). population(dryden,7617).

married(alan,anna). married(bob,betty). married(chris,cathy). married(dwight,debora). married(ethan,elle). married(fred,fay). married(george,georgina). married(howard,hallie). married(ian,isabel). married(john,julie).

parent(alan,bob). parent(anna,bob). parent(alan,cathy). parent(anna,cathy). parent(dwight,ethan). parent(debora,ethan). parent(dwight,hallie). parent(debora,hallie). parent(fred,george). parent(fay,george). parent(ian,john). parent(isabel,john). parent(george,howard). parent(georgina,howard). parent(cathy,julie). parent(chris,julie). parent(kevin,anna). parent(bob,elle). parent(betty,elle). parent(kate,fay).

friend(alan,cathy). friend(anna,cathy). friend(betty,dwight). friend(bob,dwight). friend(betty,debora). friend(bob,debora). friend(ethan,fred). friend(ethan,georgina). friend(elle,fred). friend(elle,georgina). friend(isabel,george). friend(ian,george). friend(isabel,georgina). friend(ian,georgina). friend(alan,kevin). friend(alan,leroy).


/* Part 1: Relationship predicates */

father(X,Y) :- male(X), parent(X,Y).
mother(X,Y) :- female(X), parent(X,Y).
brother(X,Y) :- person(X), male(X), parent(Z,X), person(Y), not X = Y, parent(Z,Y).
sister(X,Y) :- person(X), female(X), parent(Z,X), person(Y), not X = Y, parent(Z,Y).

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

grandmother(X,Y) :- female(X), parent(Z,Y), parent(X,Z).
grandfather(X,Y) :- male(X), parent(Z,Y), parent(X,Z).

uncle(X,Y) :- parent(Z,Y), brother(X,Z).

auntie(X,Y) :- parent(Z,Y), sister(X,Z).

relative(X,Y) :- parent(X,Y).
relative(X,Y) :- parent(Y,X).
relative(X,Y) :- ancestor(Z,X), ancestor(Z,Y).

/* Part 2: Lexicon */

article(a). article(an). article(any). article(the). article(that). article(this).

common_noun(man,X) :- male(X). 
common_noun(woman,X) :- female(X). 
common_noun(mother,X) :- mother(X,_). 
common_noun(father,X) :- father(X,_). 
common_noun(brother,X) :- brother(X,_). 
common_noun(sister,X) :- sister(X,_). 
common_noun(parent,X) :- parent(X,_). 
common_noun(ancestor,X) :- ancestor(X,_). 
common_noun(grandmother,X) :- grandmother(X,_). 
common_noun(grandfather,X) :- grandfather(X,_).
common_noun(relative,X) :- relative(X,_). 
common_noun(city,X) :- city(X).
common_noun(country,X) :- country(X).
common_noun(friend,X) :- friend(X,Y).
common_noun(friend,X) :- friend(Y,X).
common_noun(person,X) :- person(X).
common_noun(husband,X) :- male(X), married(X,Y).
common_noun(husband,X) :- male(X), married(Y,X).
common_noun(wife,X) :- female(X), married(X,Y).
common_noun(wife,X) :- female(X), married(Y,X).
common_noun(child,X) :- parent(Y,X).


 
preposition(of,X,Y) :- friend(X,Y).
preposition(of,X,Y) :- friend(Y,X).
preposition(from,X,Y) :- home(X,Y). 
preposition(from,X,Y) :- home(X,Z), location(Z,Y).

preposition(in,X,Y) :- location(Y,X). 
preposition(in,X,Y) :- location(X,Y). 
preposition(in,X,Y) :- home(X,Y). 
preposition(with,X,Y) :- relative(X,Y).  
preposition(with,X,Y) :- married(X,Y).
preposition(with,X,Y) :- married(Y,X).
preposition(with,X,Y) :- friend(X,Y).
preposition(with,X,Y) :- friend(Y,X).


proper_noun(X) :- not article(X), not common_noun(X,_), not adjective(X,_), not preposition(X,_,_).

adjective(big,X) :- population(X,Y), Y>=50000.
adjective(small,X) :- population(X,Y), Y<50000.
adjective(largest,X) :- population(X,Y), not (population(X2,Y2), Y2 > Y).
adjective(largest,X) :- population(X,Y), not (population(X2,Y2), Y2 > Y, location(X,Country), location(X2,Country)).
adjective(smallest,X) :- population(X,Y), not (population(X2,Y2), Y2 < Y).
adjective(smallest,X) :- population(X,Y), not (population(X2,Y2), Y2 < Y, location(X,Country), location(X2,Country)).
adjective(large,X) :- adjective(big,X).
adjective(single,X) :- not married(X,_), not married(_,X).
adjective(married,X) :- not (not married(X,_), not married(_,X)).
adjective(parent,X) :- parent(X,_).
adjective(american,X) :- home(X,City), location(City,usa).
adjective(canadian,X) :- home(X,City), location(City,canada).
adjective(australian,X) :- home(X,City), location(City,australia).
adjective(thai,X) :- home(X,City), location(City,thailand).
adjective(chinese,X) :- home(X,City), location(City,china).
adjective(torontonian,X) :- home(X,toronto).
adjective(montrealer,X) :- home(X,montreal).
adjective(tiny,X) :- adjective(small,X). 


/* Part 3: Parser */

what(Words, Ref) :- np(Words, Ref).
who(Words, Ref) :- np(Words, Ref).

np([Name],Name) :- proper_noun(Name).
np([Art|Rest], Who) :- article(Art), np2(Rest, Who).


np2([Adj|Rest],Who) :- adjective(Adj,Who), np2(Rest, Who).
np2([Noun|Rest], Who) :- common_noun(Noun, Who), mods(Rest,Who).


/* Modifier(s) provide an additional specific info about nouns.
   Modifier can be a prepositional phrase followed by none, one or more
   additional modifiers.  */

mods([], _).
mods(Words, Who) :-
    appendLists(Start, End, Words),
    prepPhrase(Start, Who), mods(End, Who).

prepPhrase([Prep|Rest], Who) :-
    preposition(Prep, Who, Ref), np(Rest, Ref).

appendLists([], L, L).
appendLists([H|L1], L2, [H|L3]) :-  appendLists(L1, L2, L3).
