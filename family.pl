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


%testing new parents
%parent_list([kenneth_harris, test1],
 %           [child, edward_thompson]).

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
    not(exists_closer_ancestor(Person1, Person2, Ancestor)).

exists_closer_ancestor(Person1, Person2, Ancestor) :-
     descendent(Descendent, Ancestor),
     ancestor(Descendent,Person1),
     ancestor(Descendent,Person2).

% Do Person1 and Person2 have a common ancestor?
% blood(?Person1, ?Person2). %% blood relative
blood(Person1,Person2) :-
    ancestor(Ancestor,Person1),
    ancestor(Ancestor,Person2).

% Are Person1 and Person2 on the same list 2nd are of a parent_list record.
% sibling(?Person1, Person2).
%
%This will not identify half-siblings
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
cousin(Cousin,Person) :-
    %just gets one common ancestor
    (least_common_ancestor(Cousin, Person, Ancestor),!),
    parent(Ancestor, Elder),
    ancestor(Elder, Person),
    Elder \== Cousin,
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

cousin_type(Person1, Person2, CousinType, Removed) :-
    cousin(Person1, Person2),
    (least_common_ancestor(Person1, Person2, Ancestor),!),
    generations(Ancestor, Person1, Gen1),
    generations(Ancestor, Person2, Gen2),
    absolute_value((Gen2 - Gen1), Removed),
    getMinValue(Gen1, Gen2, MinGen),
    CousinType is (MinGen - 1).
