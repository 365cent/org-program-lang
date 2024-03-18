% ===========================================================================
:- use_module(library(clpb)).
:- use_module(library(clpfd)).

% ===========================================================================
% expression definition
% boolean value true
expr(true_).
% boolean value false                                    
expr(false_).
% logic: if B is true, evaluate T, otherwise evaluate E
expr(if_then_else(B, T, E)) :- expr(B), expr(T), expr(E).
% natural number zero
expr(zero).
% suc represents the next natural number 
expr(suc(X)) :- expr(X).
% determines if an expression X evaluates to zero
expr(iszero(X)) :- expr(X).
% pred represents the preceding number
expr(pred(X)) :- expr(X).
% variable
expr(var(X)) :- atom(X).

% general values
% zero is a natural value
nv(zero).
% The successor of a natural value is also a natural values
nv(suc(X)) :- nv(X).
% true is a value
value(true_).
% false is a value
value(false_).
% any natural value is a value
value(X) :- nv(X).

% basic types
% boolean type
type(bool).
% natural number type
type(nat).

% typing rules for expressions
% True is of type boolean
typed(true_, bool).
% False is of type boolean
typed(false_, bool).
% Zero is of type natural number
typed(zero, nat).
% successor of a natural number is also a natural number
typed(suc(X), nat) :- typed(X, nat).
% predecessor of a natural number is also a natural number
typed(pred(X), nat) :- typed(X, nat).
% zero check results in a boolean
typed(iszero(X), bool) :- typed(X, nat).
% condition must be a boolean and both branches have same type
typed(if_then_else(X, Y, Z), T) :-
    typed(X, bool), typed(Y, T), typed(Z, T).
% it succeeds if the context contains a pair (Var, Type), indicating that Var has Type.
typed(Context, Var, Type) :-
    member((Var, Type), Context).
% it ensures that the argument of 'suc' is of type 'nat' in the given context.
typed(Context, suc(X), nat) :-
    typed(Context, X, nat).

% ===========================================================================
% single-step
% if true, choose the 'then' branch
sstep(if_then_else(true_, X, _), X).
% if false, choose the 'else' branch
sstep(if_then_else(false_, _, Y), Y).
% evaluate condition
sstep(if_then_else(Z, X, Y), if_then_else(W, X, Y)) :- sstep(Z, W).
% evaluate successor
sstep(suc(X), suc(Y)) :- sstep(X, Y).
% predecessor of zero is zero
sstep(pred(zero), zero).
% predecessor of a successor is the original number
sstep(pred(suc(X)), X) :- nv(X).
 % evaluate predecessor
sstep(pred(X), pred(Y)) :- sstep(X, Y).
% zero check for zero is true
sstep(iszero(zero), true_).
% zero check for successor of a number is false
sstep(iszero(suc(X)), false_) :- nv(X).
% evaluate zero check
sstep(iszero(X), iszero(Y)) :- sstep(X, Y).
sstep(Context, var(X), Value) :-
    member((var(X), Value), Context).

% multi-step
% if X is a value, it's the final result
mstep(X, X) :- value(X).
% apply single-step and then recurse
mstep(X, Y) :- sstep(X, Z), mstep(Z, Y).

% if-then-else
% rule for when the condition is true. The 'then' branch is chosen.
tsstep(if_then_else(true_, X, _), X, t_IfTrue).
% rule for when the condition is false. The 'else' branch is chosen.
tsstep(if_then_else(false_, _, Y), Y, t_IfFalse).
% recursive rule for when the condition itself needs to be evaluated.
tsstep(if_then_else(Z, X, Y), if_then_else(W, X, Y), Rule) :-
    tsstep(Z, W, SubRule),  % evaluate the condition
    Rule = t_If(SubRule).   % recursion of the If rule.

% successor
tsstep(suc(X), suc(Y), t_Suc(Rule)) :- 
    tsstep(X, Y, Rule).     % evaluate the inner expression and trace it with t_Suc(Rule).

% predecessor
% rule for predecessor of zero, result is zero.
tsstep(pred(zero), zero, t_PredZero).
% rule for predecessor of a successor, result is the original number.
tsstep(pred(suc(X)), X, t_PredSucc) :- 
    nv(X).
% evaluate the inner expression and trace it with t_Pred(Rule).
tsstep(pred(X), pred(Y), t_Pred(Rule)) :- 
    tsstep(X, Y, Rule).

% zero-check
% zero-check on zero, result is true.
tsstep(iszero(zero), true_, t_IsZeroZero).
% rule for zero-check on a successor, result is false.
tsstep(iszero(suc(X)), false_, t_IsZeroSucc) :-
    nv(X).
% evaluate the inner expression and trace it with t_IsZero(Rule).
tsstep(iszero(X), iszero(Y), t_IsZero(Rule)) :-
    tsstep(X, Y, Rule).

% typing derivation rules
% True is of type boolean
typederiv(true_, bool, t_True).
% False is of type boolean
typederiv(false_, bool, t_False).
% Zero is of type natural number
typederiv(zero, nat, t_Zero).
% successor type derivation
typederiv(suc(X), nat, t_Succ(T)) :- typederiv(X, nat, T).
% predecessor type derivation
typederiv(pred(X), nat, t_Pred(T)) :- typederiv(X, nat, T).
% zero check type derivation
typederiv(iszero(X), bool, t_Iszero(T)) :- typederiv(X, nat, T).
typederiv(if_then_else(X, Y, Z), T, t_If(XD, YD, ZD)) :-
    typederiv(X, bool, XD), % Condition must be a boolean
    typederiv(Y, T, YD),    % Then branch type derivation
    typederiv(Z, T, ZD).    % Else branch type derivation

% check if value is well-typed
typedvalue(V, T) :- value(V), typederiv(V, T, _).

% check for progress (either value or step)
progress(V, T) :- typederiv(V, T, _), (value(V); sstep(V, _)).

% add/update context: These tests ensure that the context correctly reflects changes such as adding new variables or updating existing ones, 
% which is vital for correctly handling variable scopes.
% add new variable to the context
add_to_context(Context, Var, Type, [(Var, Type)|Context]). % add Var to Context if not already present

% update existing variable in the context
update_context([(Var, _)|T], Var, NewType, [(Var, NewType)|T]). % update Var with NewType
update_context([H|T], Var, Type, [H|NewContext]) :-
    update_context(T, Var, Type, NewContext).   % recursively update context

% lookup a variable in the context
% lookup context: Ensures that variable lookup behaves correctly, finding variables that are present and failing for those that are not,
% crucial for correct variable resolution in expressions.
% return Type of Var if found at head of Context
lookup_context([(Var, Type)|_], Var, Type). 
lookup_context([_|T], Var, Type) :-
    lookup_context(T, Var, Type). % recursively search for Var in Context