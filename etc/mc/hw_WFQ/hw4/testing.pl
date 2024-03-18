:- [typing].
:- initialization(run_tests).

% Define run_tests predicate to execute all tests
run_tests :-
    test_expr,
    test_value,
    test_type,
    test_typed,
    test_sstep,
    test_mstep,
    test_tsstep,
    test_typederiv,
    test_addition.

% Define helper functions
accept_test(Test) :-
    (call(Test) -> writeln('PASS: '+ Test); writeln('FAIL: '+ Test)).

% Test cases for expr 
test_expr :-
    writeln('Testing expr: Positive cases'),

    % basic expressions
    accept_test(expr(true_)),
    accept_test(expr(false_)),
    accept_test(expr(zero)),

    % natural number
    accept_test(expr(suc(zero))),
    accept_test(expr(pred(zero))),

    % conditional expression with valid sub-expressions
    accept_test(expr(if_then_else(true_, false_, true_))),
    accept_test(expr(if_then_else(false_, suc(zero), pred(zero)))),

    % zero-check
    accept_test(expr(iszero(zero))),
    accept_test(expr(iszero(suc(zero)))),

    % variable expression
    accept_test(expr(var('X'))),

    writeln('Testing expr: Negative cases'),

    % invalid expressions
    accept_test(expr(if_then_else('x', true_, false_))),    % Invalid condition type
    accept_test(expr(suc('Y'))),                            % Invalid argument type for suc
    accept_test(expr(pred('z'))),                           % Invalid argument type for pred
    accept_test(expr(iszero('notzero'))).                   % Invalid argument type for iszero


% Test cases for value
test_value :-
    writeln('Testing value: Positive cases'),

    % boolean values
    accept_test(value(true_)),
    accept_test(value(false_)),

    % natural values
    accept_test(value(zero)),
    accept_test(value(suc(zero))),          % Zero is a natural value, so is its successor
    accept_test(value(suc(suc(zero)))),     % Testing with nested successors

    writeln('Testing value: Negative cases'),

    % non-values
    accept_test(value(if_then_else(true_, false_, true_))), % if_then_else is not a direct value
    accept_test(value(suc(true_))),                         % successor of a non-natural value is not a value
    accept_test(value(pred(zero))).                         % pred(zero) is not directly a value


% Test cases for type predicate
test_type :-
    writeln('Testing type/1: Positive cases'),

    % Test defined types
    accept_test(type(bool)),  % Test boolean type
    accept_test(type(nat)),   % Test natural number type

    writeln('Testing type/1: Negative cases'),

    % Test undefined types
    accept_test(type(string)),  % 'string' should'nt be a correct type
    accept_test(type(int)),     % 'int' should'nt be a correct type
    accept_test(type(list)).    % 'list' should'nt be a correct type


% Test cases for typed predicate
test_typed :-
    writeln('Testing typed: Positive cases'),

    % boolean values type
    accept_test(typed(true_, bool)),
    accept_test(typed(false_, bool)),

    % natural number values type
    accept_test(typed(zero, nat)),
    accept_test(typed(suc(zero), nat)),
    accept_test(typed(pred(zero), nat)),
    accept_test(typed(iszero(zero), bool)),

    % if_then_else expression type
    accept_test(typed(if_then_else(true_, zero, zero), nat)),
    accept_test(typed(if_then_else(false_, false_, true_), bool)),

    % Create a context
    Context = [(var('X'), nat), (var('Y'), bool)],

    % variable typing based on context
    accept_test(typed(Context, var('X'), nat)),
    accept_test(typed(Context, var('Y'), bool)),

    % typing of suc in context
    accept_test(typed(Context, suc(var('X')), nat)),  % X is nat, so suc(X) should be nat
   
    writeln('Testing sstep: Negative cases'),

    % incorrect typings
    accept_test(typed(true_, nat)),                             % true_ should not be typed as nat
    accept_test(typed(zero, bool)),                             % zero should not be typed as bool
    accept_test(typed(if_then_else(true_, zero, true_), nat)),  % inconsistent types in branches
    accept_test(typed(Context, var('X'), bool)),                % X is nat, not bool
    accept_test(typed(Context, var('Z'), nat)),                 % Z is not in the context
    accept_test(typed(Context, suc(var('Y')), nat)).            % Y is bool, so suc(Y) should not be nat


% Test cases for sstep predicate
test_sstep :-
    writeln('Testing sstep: Positive cases'),

    % if_then_else
    accept_test(sstep(if_then_else(true_, zero, suc(zero)), zero)),
    accept_test(sstep(if_then_else(false_, zero, suc(zero)), suc(zero))),
    
    % nested if_then_else
    accept_test(sstep(if_then_else(if_then_else(true_, true_, false_), zero, suc(zero)), if_then_else(true_, zero, suc(zero)))),
    
    % predecessor
    accept_test(sstep(pred(zero), zero)),
    accept_test(sstep(pred(suc(zero)), zero)),

    % zero check
    accept_test(sstep(iszero(zero), true_)),
    accept_test(sstep(iszero(suc(zero)), false_)),

    % Context evaluation for variables
    Context = [(var('X'), zero), (var('Y'), suc(zero))],
    accept_test(sstep(Context, var('X'), zero)),
    accept_test(sstep(Context, var('Y'), suc(zero))),

    writeln('Testing typed: Negative cases'),

    % Test successor with value non-reducable
    accept_test(sstep(suc(zero), zero)),
    % Test if_then_else with non-boolean condition
    accept_test(sstep(if_then_else(zero, zero, suc(zero)), _)),
    % Test successor with non-natural number
    accept_test(sstep(suc(true_), _)),
    % Test predecessor with non-natural number
    accept_test(sstep(pred(true_), _)),
    % Test zero check with non-natural number
    accept_test(sstep(iszero(true_), _)),
    % Context-based evaluation with non-existent variable
    Context = [(var('X'), zero), (var('Y'), suc(zero))],
    accept_test(sstep(Context, var('Z'), _)).


% Test cases for mstep predicate
test_mstep :-
    writeln('Testing mstep: Positive cases'),

    % Test with an expression that is already a value
    accept_test(mstep(true_, true_)),
    accept_test(mstep(zero, zero)),

    writeln('Testing mstep: Negative cases'),
    % Test with expressions that reduce over multiple steps, since the reduced type is value, it fails at type check
    accept_test(mstep(if_then_else(if_then_else(true_, false_, true_), zero, suc(zero)), zero)),
    % Test with a nested successor expression that reduces in multiple steps, since the reduced type is value, it fails at type check
    accept_test(mstep(suc(suc(zero)), suc(zero))).


% Test cases for tsstep predicate
test_tsstep :-
    writeln('Testing tsstep: Positive cases'),

    % Test if_then_else
    accept_test(tsstep(if_then_else(true_, zero, suc(zero)), zero, t_IfTrue)),
    accept_test(tsstep(if_then_else(false_, zero, suc(zero)), suc(zero), t_IfFalse)),

    % Test nested if_then_else
    accept_test(tsstep(if_then_else(if_then_else(true_, true_, false_), zero, suc(zero)), if_then_else(true_, zero, suc(zero)), t_If(t_IfTrue))),
    
    % Test predecessor
    accept_test(tsstep(pred(zero), zero, t_PredZero)),
    accept_test(tsstep(pred(suc(zero)), zero, t_PredSucc)),

    % Test zero check
    accept_test(tsstep(iszero(zero), true_, t_IsZeroZero)),
    accept_test(tsstep(iszero(suc(zero)), false_, t_IsZeroSucc)),

    % Test cases expected to fail
    writeln('Testing tsstep: Negative cases'),
    accept_test(tsstep(if_then_else(zero, zero, suc(zero)), _, _)), % Invalid condition type
    accept_test(tsstep(pred(true_), _, _)),                         % Invalid argument for pred
    accept_test(tsstep(suc(suc(zero)), suc(suc(zero)), t_Suc(t_NoStep))). % There is further step for suc(zero), so failed


% Test cases for typederiv predicate
test_typederiv :-
    writeln('Testing typederiv: Positive cases'),

    % basic type derivations
    accept_test(typederiv(true_, bool, t_True)),
    accept_test(typederiv(false_, bool, t_False)),
    accept_test(typederiv(zero, nat, t_Zero)),

    % successor type derivation
    accept_test(typederiv(suc(zero), nat, t_Succ(t_Zero))),
    accept_test(typederiv(suc(suc(zero)), nat, t_Succ(t_Succ(t_Zero)))),

    % predecessor type derivation
    accept_test(typederiv(pred(zero), nat, t_Pred(t_Zero))),
    accept_test(typederiv(pred(suc(zero)), nat, t_Pred(t_Succ(t_Zero)))),

    % zero check type derivation
    accept_test(typederiv(iszero(zero), bool, t_Iszero(t_Zero))),
    accept_test(typederiv(iszero(suc(zero)), bool, t_Iszero(t_Succ(t_Zero)))),

    % if_then_else type derivation
    accept_test(typederiv(if_then_else(true_, zero, zero), nat, t_If(t_True, t_Zero, t_Zero))),
    accept_test(typederiv(if_then_else(false_, true_, false_), bool, t_If(t_False, t_True, t_False))),

    writeln('Testing typederiv: Negative cases'),

    % incorrect type derivations for basic expressions
    accept_test(typederiv(true_, nat, _)),  % true_ should not derive as nat
    accept_test(typederiv(zero, bool, _)),  % zero should not derive as bool

    % incorrect successor type derivation
    accept_test(typederiv(suc(true_), nat, _)),  % true_ is not a natural number

    % incorrect predecessor type derivation
    accept_test(typederiv(pred(true_), nat, _)),  % true_ is not a natural number

    % incorrect zero check type derivation
    accept_test(typederiv(iszero(true_), bool, _)),  % true_ is not a natural number

    % incorrect if_then_else type derivation
    accept_test(typederiv(if_then_else(zero, zero, zero), nat, _)),  % zero as condition is incorrect
    accept_test(typederiv(if_then_else(true_, zero, true_), nat, _)).  % Branches with different types


% Test cases for additional predicts
test_addition :-
    writeln('Testing additional: Positive cases'),
    % typedvalue correct
    % Test with a known value and its correct type.
    accept_test(typedvalue(true_, bool)),
    accept_test(typedvalue(zero, nat)),

    % progress correct
    % Test with an expression that is a value and an expression that can take a step.
    accept_test(progress(true_, bool)),
    accept_test(progress(if_then_else(true_, zero, suc(zero)), nat)),

    % add_to_context, update_context, lookup_context Test
    % Ensures correct manipulation and lookup in variable context, basic aspect of scoping in programming.
    % Add Variable
    InitialContext = [],
    accept_test(add_to_context(InitialContext, var('X'), nat, [(var('X'), nat)|InitialContext])),

    % Update Variable
    UpdatedContext = [(var('X'), bool)],
    accept_test(update_context([(var('X'), nat)], var('X'), bool, UpdatedContext)),

    % Lookup Variable - Success and Fail
    accept_test(lookup_context([(var('X'), nat)], var('X'), nat)),

    writeln('Testing additional: Negative cases'),
    % typedvalue incorrect
    % Test with a value and an incorrect type.
    accept_test(typedvalue(true_, nat)),
    accept_test(typedvalue(zero, bool)),

    % progress incorrect
    % Test with an expression that is neither a value nor can take a step.
    accept_test(progress(if_then_else(zero, zero, zero), _)).
