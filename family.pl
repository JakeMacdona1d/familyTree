%Jake Macdonald
%March 6 2023
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ECE3520/CpSc3520 SDE1: Prolog Declarative and Logic Programming

% Use the following Prolog relations as a database of familial
% relationships for 4 generations of people.  If you find obvious
% minor errors (typos) you may correct them.  You may add additional
% data if you like but you do not have to.

% Then write Prolog rules to encode the relations listed at the bottomG.
% You may create additional predicates as needed to accomplish this,
% including relations for debugging or extra relations as you desire.
% All should be included when you turn this in.  Your rules must be able
% to work on any data and across as many generations as the data specifies.
% They may not be specific to this data.

% Using SWI-Prolog, run your code, demonstrating that it works in all modes.
% Log this session and turn it in with your code in this (modified) file.
% You examples should demonstrate working across 4 generations where
% applicable.

% Fact recording Predicates:

% list of two parents, father first, then list of all children
% parent_list(?Parent_list, ?Child_list).

% Data:

parent_list([tom_smith, evelyn_harris],
            [mark_smith, freddy_smith, joe_smith, francis_smith]).

parent_list([mark_smith, pam_wilson],
            [martha_smith, frederick_smith]).

parent_list([freddy_smith, connie_warrick],
            [jill_smith, marcus_smith, tim_smith]).

parent_list([john_smith, layla_morris],
            [julie_smith, leslie_smith, heather_smith, zach_smith]).

parent_list([edward_thompson, susan_holt],
            [leonard_thompson, mary_thompson]).

parent_list([leonard_thompson, list_smith],
            [joe_thompson, catherine_thompson, john_thompson, carrie_thompson]).

parent_list([joe_thompson, lisa_houser],
            [lilly_thompson, richard_thompson, marcus_thompson]).

parent_list([john_thompson, mary_snyder],
            []).

parent_list([jeremiah_leech, sally_swithers],
            [arthur_leech]).

parent_list([arthur_leech, jane_smith],
            [timothy_leech, jack_leech, heather_leech]).

parent_list([robert_harris, julia_swift],
            [evelyn_harris, albert_harris]).

parent_list([albert_harris, margaret_little],
            [june_harris, jackie_harris, leonard_harris]).

parent_list([leonard_harris, constance_may],
            [jennifer_harris, karen_harris, kenneth_harris]).

parent_list([beau_morris, jennifer_willis],
            [layla_morris]).

parent_list([willard_louis, missy_deas],
            [jonathan_louis]).

parent_list([jonathan_louis, marsha_lang],
            [tom_louis]).

parent_list([tom_louis, catherine_thompson],
            [mary_louis, jane_louis, katie_louis]).

%the mentioned testing addition
%testing new parents
%parent_list([kenneth_harris, test1],
%            [child, edward_thompson]).

male_list([kenneth_harris]).
male_list([joe_smith]).
male_list([tim_smith]).
male_list([marcus_smith]).
male_list([frederick_smith]).
male_list([richard_thompson]).
male_list([marcus_thompson]).
male_list([timothy_leech]).
male_list([jack_leech]).
male_list([zach_smith]).

female_list([francis_smith]).
female_list([jill_smith]).
female_list([martha_smith]).
female_list([june_harris]).
female_list([jackie_harris]).
female_list([jennifer_harris]).
female_list([karen_harris]).
female_list([mary_thompson]).
female_list([lilly_thompson]).
female_list([carrie_thompson]).
female_list([mary_louis]).
female_list([jane_louis]).
female_list([katie_louis]).
female_list([heather_smith]).
female_list([julie_smith]).
female_list([leslie_smith]).
female_list([heather_leech]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWE1 Assignment - Create rules for:

% the Parent is the parent - mother or father of the child
% parent(?Parent, ?Child).
parent(Parent, Child) :-
    parent_list([Parent,_], Children),
    member(Child, Children).
parent(Parent, Child) :-
    parent_list([_,Parent], Children),
    member(Child, Children).


% Husband is married to Wife - note the order is significant
% This is found in the first list of the parent_list predicate
% married(?Husband, ?Wife).
married(Husband, Wife) :-
    parent_list([Husband, Wife], _).

% Ancestor is parent, grandparent, great-grandparent, etc. of Person
% Order is significant.  This looks for chains between records in the parent_list data
% ancestor(?Ancestor, ?Person).
ancestor(Ancestor, Person) :-
    parent(Ancestor, Person).
ancestor(Ancestor, Person) :-
    parent(Parent, Person),
    ancestor(Ancestor, Parent).
% Really the same as ancestor, only backwards.  May be more convenient in some cases.
% descendent(?Decendent, ?Person).
descendent(Descendent, Person) :-
    parent(Person, Descendent).
descendent(Descendent, Person) :-
    parent(Person, Parent),
    descendent(Descendent, Parent).

% There are exactly Gen generations between Ancestor and Person.  Person and parent
% have a Gen of 1.  The length of the chain (or path) from Person to Ancestor.
% Again order is significant.
% generations(?Ancesstor, ?Person, ?Gen).
generations(Ancestor, Person, Gen) :-
    ancestor(Ancestor, Person),
    generations_helper(Ancestor, Person, 1, Gen).

generations_helper(Ancestor, Person, I, Gen) :-
    parent(Parent, Person),
    J is I + 1,
    (Ancestor=Parent, Gen = I;
    generations_helper(Ancestor, Parent, J, Gen)).

% Ancestor is the ancestor of both Person1 and Person2.  There must not exist another
% common ancestor that is fewer generations.
% least_common_ancestor(?Person1, ?Person2, ?Ancestor).
least_common_ancestor(Person1, Person2, Ancestor) :-
    ancestor(Ancestor, Person1),
    ancestor(Ancestor, Person2),

    %since this will exhaustively try to prove itself true,
    %if it returns false, then the ancestor given is true.
    not(exists_closer_ancestor(Person1, Person2, Ancestor)),
    Person1\=Person2.

exists_closer_ancestor(Person1, Person2, Ancestor) :-
     descendent(Descendent, Ancestor),
     ancestor(Descendent,Person1),
     ancestor(Descendent,Person2),!.

% Do Person1 and Person2 have a common ancestor?
% blood(?Person1, ?Person2). %% blood relative
blood(Person1,Person2) :-
    ancestor(Ancestor,Person1),
    ancestor(Ancestor,Person2),
    Person1 \= Person2.

% Are Person1 and Person2 on the same list 2nd are of a parent_list record.
%This will not identify half-siblings. Written like this for dupes
% sibling(?Person1, Person2).
sibling(Person1, Person2) :-
    mother(Mother, Person1),
    mother(Mother, Person2),
    father(Father, Person1),
    father(Father, Person2),
    Person1\=Person2.

% These are pretty obvious, and really just capturing info we already can get - except that
% the gender is important.  Note that father is always first on the list in parent_list.
% father(?Father, ?Child).
father(Father, Child) :-
    parent_list([Father,_], Children),
    member(Child, Children).

% mother(?Mother, ?Child).
mother(Mother, Child) :-
    parent_list([_,Mother], Children),
    member(Child, Children).


% Note that some uncles may not be in a parent list arg of parent_list, but would have
% a male record to specify gender.
% uncle(?Uncle, ?Person).
uncle(Uncle, Person) :-
    parent(Parent, Person),
    sibling(Parent, Sibling),
    ((Uncle=Sibling,
    married(Uncle,_));
    (married(Uncle,Sibling))).
uncle(Uncle, Person) :-
    parent(Parent, Person),
   sibling(Parent, Uncle),
  male_list([Uncle]).

% aunt(?Aunt, ?Person). %%
aunt(Aunt, Person) :-
    parent(Parent, Person),
    sibling(Parent, Sibling),
    ((Aunt=Sibling,
    married(_,Aunt));
    (married(Sibling,Aunt))).
aunt(Aunt, Person) :-
    parent(Parent, Person),
    sibling(Parent, Aunt),
    female_list([Aunt]).

% does not handle the complexities of incest,
% like if a brother is also a cousin,
% the cousin-brother would not be considered
% a cousin by this rule.
% cousin(?Cousin, ?Person)
cousin(Cousin,Person) :-
    least_common_ancestor(Cousin, Person, Ancestor),
    %using father to remove duplicates
    father(Ancestor, Elder),
    ancestor(Elder, Person),
    Elder \= Cousin,
    not(ancestor(Elder, Cousin)),
    not(sibling(Elder, Cousin)).

%% 1st cousin, 2nd cousin, 3rd once removed, etc.
% cousin_type(+Person1, +Person2, -CousinType, -Removed).
getMinValue(X,Y, Min) :-
    % ! operator stops the search
    (X < Y, Min is X, !);
    Min is Y.


%Yes, I am aware of the built in abs function.
%But! For this assignment, built in functions are bad :d
absolute_value(X,Val) :-
    (X >= 0, Val is X, !);
    Val is ((-1)*X).

% cousin_type(+Person1, +Person2, -CousinType, -Removed).
cousin_type(Person1, Person2, CousinType, Removed) :-
    cousin(Person1, Person2),
    least_common_ancestor(Person1, Person2, Ancestor),

    %removes duplicates
    married(Ancestor,_),
    generations(Ancestor, Person1, Gen1),
    generations(Ancestor, Person2, Gen2),
    absolute_value((Gen2 - Gen1), Removed),
    getMinValue(Gen1, Gen2, MinGen),
    CousinType is (MinGen - 1).

% Logged Testing
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Testing Using given data
%%%%%%%%%
% PARENT
% ?- parent(tom_louis,Child).
% Child = mary_louis ;
% Child = jane_louis ;
% Child = katie_louis .

% ?- parent(Parent,Child).
% Parent = tom_smith,
% Child = mark_smith ;
% Parent = tom_smith,
% Child = freddy_smith ;
% Parent = tom_smith,
% Child = joe_smith .

% MARRIED
% ?- married(Husband,Wife).
% Husband = tom_smith,
% Wife = evelyn_harris ;
% Husband = mark_smith,
% Wife = pam_wilson .


% ANCESTOR
% ?- ancestor(julia_swift,kenneth_harris).
% true .
% ?- ancestor(Ancestor,Person).
% Ancestor = tom_smith,
% Person = mark_smith ;
% Ancestor = tom_smith,
% Person = freddy_smith ;
% Ancestor = tom_smith,
% Person = joe_smith ;
% Ancestor = tom_smith,
% Person = francis_smith .

% DESCENDENT
% ?- descendent(kenneth_harris,julia_swift).
% true .

% ?- descendent(Descendent,Person).
% Descendent = mark_smith,
% Person = tom_smith ;
% Descendent = freddy_smith,
% Person = tom_smith ;
% Descendent = joe_smith,
% Person = tom_smith .

% GENERATIONS
% ?- generations(susan_holt,katie_louis,Generations).
% Generations = 3 .
% ?- generations(Ancestor,Person,Generations).
% Ancestor = tom_smith,
% Person = mark_smith,
% Generations = 1 ;
% Ancestor = tom_smith,
% Person = freddy_smith,
% Generations = 1 ;
% Ancestor = tom_smith,
% Person = joe_smith,
% Generations = 1 ;
% Ancestor = tom_smith,
% Person = francis_smith,
% Generations = 1 ;
% Ancestor = mark_smith,
% Person = martha_smith,
% Generations = 1 ;
% Ancestor = mark_smith,
% Person = frederick_smith,
% Generations = 1 ;
% Ancestor = freddy_smith,
% Person = jill_smith,
% Generations = 1 ;
% Ancestor = freddy_smith,
% Person = marcus_smith,
% Generations = 1 ;
% Ancestor = freddy_smith,
% Person = tim_smith,
% Generations = 1 ;
% Ancestor = john_smith,
% Person = julie_smith,
% Generations = 1 ;
% Ancestor = john_smith,
% Person = leslie_smith,
% Generations = 1 .

% LEAST_COMMON_ANCESTOR
% ?- least_common_ancestor(Person1,Person2,margaret_little).
% Person1 = june_harris,
% Person2 = jackie_harris ;
% Person1 = june_harris,
% Person2 = leonard_harris ;
% Person1 = june_harris,
% Person2 = jennifer_harris ;
% Person1 = june_harris,
% Person2 = karen_harris ;
% Person1 = june_harris,
% Person2 = kenneth_harris ;
% Person1 = jackie_harris,
% Person2 = june_harris ;
% Person1 = jackie_harris,
% Person2 = leonard_harris ;
% Person1 = jackie_harris,
% Person2 = jennifer_harris ;
% Person1 = jackie_harris,
% Person2 = karen_harris ;
% Person1 = jackie_harris,
% Person2 = kenneth_harris ;
% Person1 = leonard_harris,
% Person2 = june_harris ;
% Person1 = leonard_harris,
% Person2 = jackie_harris ;
% Person1 = leonard_harris,
% Person2 = jennifer_harris ;
% Person1 = leonard_harris,
% Person2 = karen_harris ;
% Person1 = leonard_harris,
% Person2 = kenneth_harris ;
% Person1 = jennifer_harris,
% Person2 = june_harris ;
% Person1 = jennifer_harris,
% Person2 = jackie_harris ;
% Person1 = jennifer_harris,
% Person2 = leonard_harris ;
% Person1 = karen_harris,
% Person2 = june_harris ;
% Person1 = karen_harris,
% Person2 = jackie_harris ;
% Person1 = karen_harris,
% Person2 = leonard_harris ;
% Person1 = kenneth_harris,
% Person2 = june_harris ;
% Person1 = kenneth_harris,
% Person2 = jackie_harris ;
% Person1 = kenneth_harris,
% Person2 = leonard_harris ;
% false.
% ?- least_common_ancestor(karen_harris,martha_smith,Ancestor).
% Ancestor = robert_harris ;
% Ancestor = julia_swift ;
% false.
% ?- least_common_ancestor(karen_harris,kenneth_harris,Parent).
% Parent = leonard_harris ;
% Parent = constance_may ;
% false.
% ?- least_common_ancestor(Person1,Person2,Ancestor).
% Person1 = mark_smith,
% Person2 = freddy_smith,
% Ancestor = tom_smith ;
% Person1 = mark_smith,
% Person2 = joe_smith,
% Ancestor = tom_smith ;
% Person1 = mark_smith,
% Person2 = francis_smith,
% Ancestor = tom_smith ;
% Person1 = mark_smith,
% Person2 = martha_smith,
% Ancestor = tom_smith .
% ?- least_common_ancestor(jill_smith,julie_smith,Ancestor).
% false.

% BLOOD
% ?- blood(jill_smith,julie_smith).
% false.
% ?- blood(heather_smith,julie_smith).
% true .
% ?- blood(Person1,Person2).
% Person1 = mark_smith,
% Person2 = freddy_smith ;
% Person1 = mark_smith,
% Person2 = joe_smith ;
% Person1 = mark_smith,
% Person2 = francis_smith ;
% Person1 = mark_smith,
% Person2 = martha_smith .

% SIBLING
% ?- sibling(heather_smith,julie_smith).
% true .
% ?- sibling(Person1,Person2).
% Person1 = mark_smith,
% Person2 = freddy_smith ;
% Person1 = mark_smith,
% Person2 = joe_smith ;
% Person1 = mark_smith,
% Person2 = francis_smith ;
% Person1 = freddy_smith,
% Person2 = mark_smith ;
% Person1 = freddy_smith,
% Person2 = joe_smith ;
% Person1 = freddy_smith,
% Person2 = francis_smith ;
% Person1 = joe_smith,
% Person2 = mark_smith ;
% Person1 = joe_smith,
% Person2 = freddy_smith .

% FATHER
% ?- father(tom_smith,joe_smith).
% true .
% ?- father(Parent,Child).
% Parent = tom_smith,
% Child = mark_smith ;
% Parent = tom_smith,
% Child = freddy_smith ;
% Parent = tom_smith,
% Child = joe_smith ;
% Parent = tom_smith,
% Child = francis_smith ;
% Parent = mark_smith,
% Child = martha_smith .

% MOTHER
% ?- mother(Parent,Child).
% Parent = evelyn_harris,
% Child = mark_smith ;
% Parent = evelyn_harris,
% Child = freddy_smith ;
% Parent = evelyn_harris,
% Child = joe_smith ;
% Parent = evelyn_harris,
% Child = francis_smith ;
% Parent = pam_wilson,
% Child = martha_smith .

% UNCLE
% ?- uncle(tom_smith,june_harris).
% true .
% ?- uncle(Uncle,Person).
% Uncle = freddy_smith,
% Person = martha_smith ;
% Uncle = freddy_smith,
% Person = frederick_smith ;
% Uncle = mark_smith,
% Person = jill_smith ;
% Uncle = mark_smith,
% Person = marcus_smith ;
% Uncle = mark_smith,
% Person = tim_smith ;
% Uncle = tom_louis,
% Person = lilly_thompson .
% ?- uncle(joe_smith,martha_smith).
% true .

% AUNT
% ?- aunt(francis_smith,Person).
% Person = martha_smith ;
% Person = frederick_smith ;
% Person = jill_smith ;
% Person = marcus_smith ;
% Person = tim_smith ;
% false.
% ?- aunt(connie_warrick,Person).
% Person = martha_smith ;
% Person = frederick_smith ;
% false.
% ?- aunt(Aunt,Person).
% Aunt = connie_warrick,
% Person = martha_smith ;
% Aunt = connie_warrick,
% Person = frederick_smith ;
% Aunt = pam_wilson,
% Person = jill_smith ;
% Aunt = pam_wilson,
% Person = marcus_smith ;
% Aunt = pam_wilson,
% Person = tim_smith ;
% Aunt = catherine_thompson,
% Person = lilly_thompson ;
% Aunt = mary_snyder,
% Person = lilly_thompson .

% COUSIN
% ?- cousin(jane_louis, Cousin).
% Cousin = lilly_thompson ;
% Cousin = richard_thompson ;
% Cousin = marcus_thompson ;
% false.
% ?- cousin(Cousin,jane_louis).
% Cousin = lilly_thompson ;
% Cousin = richard_thompson ;
% Cousin = marcus_thompson ;
% false.
% ?- cousin(Cousin,Person).
% Cousin = martha_smith,
% Person = jill_smith ;
% Cousin = martha_smith,
% Person = marcus_smith ;
% Cousin = martha_smith,
% Person = tim_smith ;
% Cousin = martha_smith,
% Person = june_harris ;
% Cousin = martha_smith,
% Person = jackie_harris ;
% Cousin = martha_smith,
% Person = leonard_harris ;
% Cousin = martha_smith,
% Person = jennifer_harris ;
% Cousin = martha_smith,
% Person = karen_harris ;
% Cousin = martha_smith,
% Person = kenneth_harris ;
% Cousin = frederick_smith,
% Person = jill_smith ;
% Cousin = frederick_smith,
% Person = marcus_smith .
% ?- cousin(martha_smith, Cousin).
% Cousin = jill_smith ;
% Cousin = marcus_smith ;
% Cousin = tim_smith ;
% Cousin = june_harris ;
% Cousin = jackie_harris ;
% Cousin = leonard_harris ;
% Cousin = jennifer_harris ;
% Cousin = karen_harris ;
% Cousin = kenneth_harris ;
% false.

% COUSIN_TYPE
% ?- cousin_type(martha_smith, Cousin, CousinType, Removed).
% Cousin = jill_smith,
% CousinType = 1,
% Removed = 0 ;
% Cousin = marcus_smith,
% CousinType = 1,
% Removed = 0 ;
% Cousin = tim_smith,
% CousinType = 1,
% Removed = 0 ;
% Cousin = june_harris,
% CousinType = Removed, Removed = 1 ;
% Cousin = jackie_harris,
% CousinType = Removed, Removed = 1 ;
% Cousin = leonard_harris,
% CousinType = Removed, Removed = 1 ;
% Cousin = jennifer_harris,
% CousinType = 2,
% Removed = 0 ;
% Cousin = karen_harris,
% CousinType = 2,
% Removed = 0 ;
% Cousin = kenneth_harris,
% CousinType = 2,
% Removed = 0 ;
% false
% ?- cousin_type(Person1,Person2,2,0).
% Person1 = martha_smith,
% Person2 = jennifer_harris ;
% Person1 = martha_smith,
% Person2 = karen_harris ;
% Person1 = martha_smith,
% Person2 = kenneth_harris ;
% Person1 = frederick_smith,
% Person2 = jennifer_harris ;
% Person1 = frederick_smith,
% Person2 = karen_harris ;
% Person1 = frederick_smith,
% Person2 = kenneth_harris ;
% Person1 = jill_smith,
% Person2 = jennifer_harris ;
% Person1 = jill_smith,
% Person2 = karen_harris ;
% Person1 = jill_smith,
% Person2 = kenneth_harris ;
% Person1 = marcus_smith,
% Person2 = jennifer_harris ;
% Person1 = marcus_smith,
% Person2 = karen_harris ;
% Person1 = marcus_smith,
% Person2 = kenneth_harris ;
% Person1 = tim_smith,
% Person2 = jennifer_harris ;
% Person1 = tim_smith,
% Person2 = karen_harris ;
% Person1 = tim_smith,
% Person2 = kenneth_harris ;
% Person1 = jennifer_harris,
% Person2 = martha_smith ;
% Person1 = jennifer_harris,
% Person2 = frederick_smith ;
% Person1 = jennifer_harris,
% Person2 = jill_smith ;
% Person1 = jennifer_harris,
% Person2 = marcus_smith ;
% Person1 = jennifer_harris,
% Person2 = tim_smith ;
% Person1 = karen_harris,
% Person2 = martha_smith ;
% Person1 = karen_harris,
% Person2 = frederick_smith ;
% Person1 = karen_harris,
% Person2 = jill_smith ;
% Person1 = karen_harris,
% Person2 = marcus_smith ;
% Person1 = karen_harris,
% Person2 = tim_smith ;
% Person1 = kenneth_harris,
% Person2 = martha_smith ;
% Person1 = kenneth_harris,
% Person2 = frederick_smith ;
% Person1 = kenneth_harris,
% Person2 = jill_smith ;
% Person1 = kenneth_harris,
% Person2 = marcus_smith ;
% Person1 = kenneth_harris,
% Person2 = tim_smith ;
% ;false.
% ?- cousin_type(kenneth_harris,jill_smith,2,0).
% true .
% ?- cousin_type(kenneth_harris,jill_smith,1,0).
% false.


%Adding this relationship
%parent_list([kenneth_harris, test1],
%           [child, edward_thompson])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%?- generations(julia_swift,mary_louis,Generations).
%Generations = 7 .

%?- cousin(jackie_harris,mary_louis).
%false.

%?- cousin_type(martha_smith,mary_louis,CousinType,Removed).
%CousinType = 2,
%Removed = 4 .

%?- cousin_type(mark_smith,mary_louis,CousinType,Removed).
%CousinType = 1,
%Removed = 5 .

%?- least_common_ancestor(mary_louis,jill_smith,Ancestor).
%Ancestor = robert_harris ;
%Ancestor = julia_swift ;
%false.
