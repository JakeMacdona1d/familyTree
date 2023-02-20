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
            [june_harris, jackie_harrie, leonard_harris]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWE1 Assignment - Create rules for:

% the Parent is the parent - mother or father of the child
% parent(?Parent, ?Child).

parent(Parent, Child) :-
    parent_list([Parent, _], Children),
    member(Child, Children).
parent(Parent, Child) :-
    parent_list([_, Parent], Children),
    member(Child, Children).

% Husband is married to Wife - note the order is significant
% This is found in the first list of the parent_list predicate
% married(?Husband, ?Wife).
married(Husband, Wife) :-
    parent_list([Husband, Wife], _).
married(Husband, Wife) :-
    parent_list([Wife, Husband], _).

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

%right now, only looks for closest relative in respect to Person1
least_common_ancestor(Person1, Person2, Ancestor) :-
    generations(Ancestor, Person1, Gen),
    I is Gen -1,
    least_common_ancestor_helper(Person1, Person2, I).

%setof fails when theres only the empty set
least_common_ancestor_helper(Person1, Person2, Gen) :-
    not(setof(X, (ancestor(X,Person1)
               ,ancestor(X,Person2)
               ,generations(X,Person1,Gen))
             ,_));
    (I is Gen -1, I>0,
     least_common_ancestor_helper(Person1, Person2, I)).

% Do Person1 and Person2 have a common ancestor?
% blood(?Person1, ?Person2). %% blood relative
blood(Person1,Person2) :-
    ancestor(Ancestor,Person1),
    ancestor(Ancestor,Person2).

% Are Person1 and Person2 on the same list 2nd are of a parent_list record.
% sibling(?Person1, Person2).
sibling(Person1, Person2) :-
    parent(Parent, Person1),
    parent(Parent, Person2).


% These are pretty obvious, and really just capturing info we already can get - except that
% the gender is important.  Note that father is always first on the list in parent_list.
% father(?Father, ?Child).
father(Father, Child) :-
    parent_list([Father, _], Children),
    member(Child, Children).


% mother(?Mother, ?Child).
mother(Mother, Child) :-
    parent_list([_, Mother], Children),
    member(Child, Children).


% Note that some uncles may not be in a parent list arg of parent_list, but would have
% a male record to specify gender.
% uncle(?Uncle, ?Person). %%

% aunt(?Aunt, ?Person). %%

% cousins have a generations greater than parents and aunts/uncles.
% cousin(?Cousin, ?Person).

%calls helper in setof b/c this eliminates redundant outputs
cousin(Cousin, Person) :-
    setof(X,cousin_helper(X, Person),Cousins),
    member(Cousin, Cousins).
cousin_helper(Cousin, Person) :-
   parent(Parent, Person),
   ancestor(Ancestor, Parent),
   ancestor(Ancestor, Cousin),
   not(descendent(Cousin, Parent)),
   not(descendent(Parent, Cousin)),
   not(sibling(Parent, Cousin)),
   generations(Ancestor,Cousin, Gen),
   Gen>=2.


%% 1st cousin, 2nd cousin, 3rd once removed, etc.
% cousin_type(+Person1, +Person2, -CousinType, -Removed).
cousin_type(Person1, Person2, CousinType, Removed) :-
    cousin(Person1, Person2),
    least_common_ancestor(Person1, Person2, Ancestor),
    generations(Ancestor, Person1, Gen1),
    generations(Ancestor, Person2, Gen2),
    CousinType is Gen1 - Gen2 + 1,
    Removed is Gen2 - 1.


