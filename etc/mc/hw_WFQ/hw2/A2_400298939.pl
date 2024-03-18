:- use_module(library(clpb)).
:- use_module(library(clpfd)).
%sources: https://www.swi-prolog.org/pldoc/man?section=clpb
% http://eu.swi-prolog.org/man/clpb.html
% https://www.swi-prolog.org/pldoc/man?section=arith
% https://www.swi-prolog.org/man/clpfd.html
% https://stackoverflow.com/questions/8219555/what-is-the-difference-between-and-in-prolog

%% 1.1
%elem(X,Xs) :- false.
elem(X,[X|_]). 
elem(X,[_|Xs]) :- elem(X,Xs).

:- elem(1, [1,2,3]).            % 1 is in the list
:- \+(elem(4, [1,2,3])).        % 4 is not in the list
:- elem(2, [1,2,3,4,5]).        % 2 is in the list
:- \+(elem(10, [1,5,6,7])).     % 10 is not in the list
:- elem(a, [a,b,c]).            % a is in the list

%% 1.2
%pick(X,Xs,Ys) :- false. 
pick(X,[X|Xs],Xs).
pick(X,[Y|Xs],[Y|Ys]) :- pick(X,Xs,Ys).

:- pick(1, [1,2,3], [2,3]).            % [2,3] is [1,2,3] with 1 removed
:- \+ pick(1, [2,3], [2,3]).           % 1 is not in [2,3], so this is false
:- pick(1, [2,1,2,1,3], [2,2,1,3]).    % We only remove 1 from [2,1,2,1,3] one time
:- pick(1, [2,1,2,1,3], [2,1,2,3]).    % Furthermore, we can remove either 1
:- \+ pick(1, [2,1,2,1,3], [2,2,3]).   % However, we should only remove a single 1
:- pick(a, [a,b,c], [b,c]).            % [b,c] is [a,b,c] with a removed
:- \+ pick(x, [a,b,c], [a,b,c]).       % x is not in [a,b,c], so this is false
:- pick(b, [a,b,b,c], [a,b,c]).        % Remove only one b from [a,b,b,c]
:- pick(1, [1], []).                   % [1] becomes empty list after removing 1
:- \+ pick(1, [], []).                 % Empty list doesn't contain 

%% 1.3
%permute(Xs,Ys) :- false.
permute([], []).
permute([X|Xs],Ys) :- 
    pick(X,Ys,Zs), 
    permute(Xs,Zs).

:- permute([1,2,3], [3,2,1]).       % [3,2,1] is a permutation of [1,2,3]
:- permute([], []).                 % Empty list matches empty list
:- \+ permute([1,2,3], [1,2]).      % [1,2] is not a permutation of [1,2,3]
:- \+ permute([1,2,3], [4,5,6]).    % Different elements
:- permute([a,b,c], [c,b,a]).       % [c,b,a] is a permutation of [a,b,c]

%% 1.4
%sorted(Xs) :- false.
sorted([]).
sorted([_]).
sorted([X,Y|Xs]) :-
    X =< Y, 
    sorted([Y|Xs]).

:- sorted([1,2,3]).             % [1,2,3] is in ascending order.
:- \+ sorted([3,2,1]).          % [3,2,1] is not in ascending order.
:- \+ sorted([1,2,4,3]).        % [1,2,4,3] is not in ascending order.
:- sorted([-5,-4,-2]).          % [-5,-4,-2] is in ascending order.
:- sorted([0,0,0]).             % [0,0,0] is in ascending order(0=0).
    
%%1.5
%naive_sort(Xs,Ys) :- false.
%True is Ys is the sorted(ascending order) Xs
naive_sort(Xs,Ys) :-
    permute(Xs,Ys),
    sorted(Ys).

:- naive_sort([3,1,2], [1,2,3]).                    % [3,1,2] sorted is [1,2,3]
:- naive_sort([5,4,3,2,1], [1,2,3,4,5]).            % [5,4,3,2,1] sorted is [1,2,3,4,5]
:- naive_sort([1], [1]).                            % [1] is already sorted
:- naive_sort([], []).                              % Empty list is already sorted
:- naive_sort([-3,-5,2,-4,1], [-5,-4,-3,1,2]).      % [-3,-5,2,-4,1] sorted is [-5,-4,-3,2,1]


%%2
%goblins tell lie, gnomes tell truth.
%Clue #2
clue_exam(Alice,Bob) :- sat(Alice =:= ~Alice * ~Bob).

:- clue_exam(0,1). %Alice is goblin and Bob is gnome
:- \+ clue_exam(1,0). %Alice is gnome and Bob is goblin is wrongs
:- \+ clue_exam(0,0). %Alice is goblin and Bob is goblin is wrong
:- \+ clue_exam(1,1). %Alice is gnome and Bob is gnome is wrong

%2.1 Riddle #1
clue1(Alice,Bob) :- 
    sat(Alice =:= ~Bob),
    sat(Bob =:= Alice * Bob).
    

:- clue1(1,0). %Alice is gnome and Bob is goblin
:- \+ clue1(0,1). %Alice is goblin and Bob is gnome is wrongs
:- \+ clue1(0,0). %Alice is goblin and Bob is goblin is wrong
:- \+ clue1(1,1). %Alice is gnome and Bob is gnome is wrong

%2.2 Riddle #2
clue2(Alice,Bob,Carol,Dave) :- 
    sat(Alice =:= Dave),
    sat(Bob =:= ~Carol * Alice),
    sat(Carol =:= Carol + ~Carol),
    sat(Dave =:= card([2],[Alice, ~Alice, ~Bob * ~Carol])).

:- clue2(0,0,1,0). %True that Alice is goblin, Bob is goblin, Carol is gnome and Dave is goblin
:- \+ clue2(0,0,0,1). %False that Alice is goblin, Bob is goblin, Carol is goblin and Dave is gnome

%2.3 Riddle #3 NO MORE!!!
%Validate inputs
is_creature(X) :- var(X).
is_statement(gnome(X)) :- iscreature(X).
is_statement(goblin(X)) :- iscreature(X).
is_statement(and(X,Y)) :-
    isstatement(X),
    isstatement(Y).
is_statement(or(X,Y)) :-
    isstatement(X),
    isstatement(Y).

%Predicate program
goblins_or_gnomes([],_). %For when statement > creature
goblins_or_gnomes([_|Gs], []) :- 
    goblins_or_gnomes(Gs, []).

goblins_or_gnomes([G|Gs], [R|Rs]) :-
    creature_statement_truth(G, R),
    goblins_or_gnomes(Gs, Rs).          %Recursively check for the matches

creature_statement_truth(Creature, Statement) :-    
    statement_truth(Statement, State),      %Store statment in Bool
    sat(Creature =:= State).                %Compare two value to check if they are consistent

statement_truth(goblin(X), State) :-    
    sat(State =:= ~X).                  %Define goblin to have opposite value as X
statement_truth(gnome(X), State) :-
    sat(State =:= X).                   %Define gnome to have same value as X
statement_truth(and(X, Y), State) :-
    statement_truth(X, XS),
    statement_truth(Y, YS),
    sat(State =:= XS * YS).             %Define and function for two statement
statement_truth(or(X, Y), State) :-
    statement_truth(X, XS),
    statement_truth(Y, YS),
    sat(State =:= XS + YS).             %Define or function for two statement

:- goblins_or_gnomes([A], [gnome(A)]),A=1.                                          %A(goblin):A is gnome
:- goblins_or_gnomes([A,B], [and(goblin(A), goblin(B))]),A=0,B=1.                   %A(goblin):A & B are goblin, B(gnome):N/A
:- \+ goblins_or_gnomes([A,B], [and(goblin(A), goblin(B)), goblin(B)]),A=1,B=1.     %False,A(gnome):A & B are goblin, B(gnome):B is goblin
:- goblins_or_gnomes([A,B], [or(goblin(A), gnome(A)),(gnome(B))]),A=1,B=1.          %A(gnome):A is gnome or goblin, B(goblin):B is gnome
:- goblins_or_gnomes([A,B,C], [or(goblin(C), gnome(A))]),A=1,B=1,C=0.               %A(gnome):C is goblin or A is gnome, B(gnome):N/A, C(goblin):N/A


%%3 Interpreters in Prolog
%Validate inputs
boolean(true).
boolean(false).

is_expr(int(V)) :- V in inf..sup.  %% Integers.
is_expr(bool(B)) :- boolean(B).
is_expr(add(X, Y)) :- is_expr(X), is_expr(Y).
is_expr(mul(X, Y)) :- is_expr(X), is_expr(Y).
is_expr(neg(X)) :- is_expr(X).
is_expr(and(X,Y)) :- is_expr(X), is_expr(Y).
is_expr(xor(X,Y)) :- is_expr(X), is_expr(Y).
is_expr(if(B,X,Y)) :- is_expr(B), is_expr(X), is_expr(Y).

is_val(V) :- boolean(V); V in inf..sup.

%Expression
%eval_expr (E,V) :−false 
eval_expr(int(V),V).
eval_expr(bool(B),B).
eval_expr(add(X,Y),V) :-
    eval_expr(X,Xv),
    eval_expr(Y,Yv),
    V #= Xv + Yv.
eval_expr(mul(X,Y),V) :-
    eval_expr(X,Xv),
    eval_expr(Y,Yv),
    V #= Xv * Yv.
eval_expr(neg(X),V) :-
    eval_expr(X,Xv),
    V #= - Xv.
eval_expr(and(bool(X),bool(Y)),true) :-
    X == true,
    Y == true.
eval_expr(and(bool(_),bool(_)),false).
eval_expr(xor(bool(X),bool(Y)),true) :-
    X \== Y.
eval_expr(xor(bool(X),bool(Y)),false) :-
    X == Y.
eval_expr(if(bool(true),X,_),V) :-
    eval_expr(X, V).
eval_expr(if(bool(false),_,Y),V) :-
    eval_expr(Y, V).

%Test cases
:- eval_expr(add(int(1),int(2)),3).                               %1+2=3
:- eval_expr(mul(int(2),int(3)),6).                               %2*3=6
:- eval_expr(neg(int(1)),-1).                                     %-(1)=-1
:- eval_expr(mul(int(0),int(99)),0).                              %0*99=0
:- eval_expr(neg(int(-1)),1).                                     %-(-1)=1    

:- eval_expr(and(bool(true),bool(true)),true).                    %true AND true = true
:- eval_expr(and(bool(true),bool(false)),false).                  %true AND false = false
:- eval_expr(xor(bool(true),bool(false)),true).                   %true XOR false = true
:- eval_expr(xor(bool(true),bool(true)),false).                   %true XOR true = false
:- eval_expr(and(bool(xor(bool(true),bool(true))),bool(true)),false). %(true XOR true) AND true = false

:- eval_expr(if(bool(true),int(1),int(0)), 1).                    % if true then 1
:- eval_expr(if(bool(false),int(1),int(0)), 0).                   % if false then 0

:- eval_expr(add(int(400000000),int(298939)),400298939).                %400000000+298939=400298939
:- eval_expr(add(int(400298900),mul(int(3),int(13))),400298939).        %400298900+3*13=400298939
:- eval_expr(add(int(400000000),add(int(298900), int(39))),400298939).  %400000000+298900+39=400298939s




