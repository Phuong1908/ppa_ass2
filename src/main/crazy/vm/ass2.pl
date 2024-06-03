% Main program
go :- 
    open('input.txt', read, Stream),
    read_term(Stream, Y, []),
    close(Stream),
    open('output.txt', write, Stream1),
    set_output(Stream1),
    catch(reduce_prog(Y), Exception, process(Exception)),
    close(Stream1).

reduce_prog([Var, Func, Body]) :-
    create_env(Var, env([], 0, 0), Env), % Init env from the global env
    type_check_func(Env, Func, Env1), % Do type check in the function/procedure body
    type_check_body(Env1, Body), % Do type check in the program body
    create_runtime_env(Env1, REnv),
    reduce_stmt(config(Body, REnv), _).

% Error handling
process(type_mismatch(X)):- write('Type mismatch: '), write(X), !.
process(undeclare_identifier(X)):- write('Undeclared identifier: '), write(X), !.
process(wrong_number_of_argument(X)):- write('Wrong number of arguments: '), write(X), !.
process(redeclare_identifier(X)):- write('Redeclared identifier: '), write(X), !.
process(redeclare_function(X)):- write('Redeclared function: '), write(X), !.
process(redeclare_procedure(X)):- write('Redeclared procedure: '), write(X), !.
process(undeclare_function(X)):- write('Undeclared function: '), write(X), !.
process(undeclare_procedure(X)):- write('Undeclared procedure: '), write(X), !.
process(break_not_in_loop(X)):- write('Break not in a loop: '), write(X), !.
process(continue_not_in_loop(X)):- write('Continue not in a loop: '), write(X), !.
process(cannot_assign(X)) :- write('Cannot assign to a constant: '), write(X), !.
process(outofbound(X)):- write('Index out of bound: '), write(X), !.
process(invalid_expression(X)):- write('Invalid expression: '), write(X), !.

% Lookup in symbol table
lookup(env([], _, _), VarName, _) :- throw(undeclare_identifier(VarName)).
lookup(env([id(VarName, VarType, Value) | _], _, _), VarName, id(VarName, VarType, Value)) :- !.
lookup(env([_ | L], B, T), VarName, Result) :- lookup(env(L, B, T), VarName, Result).

% Check if X is an identifier
atom1(true) :- !, fail.
atom1(false) :- !, fail.
atom1([]) :- !, fail.
atom1(str(_)) :- !, fail.
atom1(X) :- atom(X).

% Check if X is a boolean constant
boolean(true).
boolean(false).

% Get type of expression
get_type_expression(Env, Y, T) :- atom1(Y), !, lookup(Env, Y, id(Y, _, T)).
get_type_expression(_, Y, integer) :- integer(Y), !.
get_type_expression(_, Y, real) :- float(Y), !.
get_type_expression(_, str(_), string).
get_type_expression(_, Y, boolean) :- boolean(Y), !.
get_type_expression(_, sub(Y), integer) :- integer(Y), !.
get_type_expression(_, sub(Y), float) :- float(Y), !.

% Logical expressions
get_type_expression(_, bnot(_), boolean).
get_type_expression(_, band(_, _), boolean).
get_type_expression(_, bor(_, _), boolean).

% Numerical expressions
get_type_expression(Env, add(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer), !.
get_type_expression(Env, add(X, Y), float) :- 
    (get_type_expression(Env, X, float);
    get_type_expression(Env, Y, float)), !.
get_type_expression(Env, sub(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer), !.
get_type_expression(Env, sub(X, Y), float) :- 
    (get_type_expression(Env, X, float);
    get_type_expression(Env, Y, float)), !.
get_type_expression(Env, times(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer), !.
get_type_expression(Env, times(X, Y), float) :- 
    (get_type_expression(Env, X, float);
    get_type_expression(Env, Y, float)), !.
get_type_expression(Env, rdiv(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer), !.
get_type_expression(Env, rdiv(X, Y), float) :- 
    (get_type_expression(Env, X, float);
    get_type_expression(Env, Y, float)), !.
get_type_expression(Env, idiv(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer), !.
get_type_expression(Env, imod(X, Y), integer) :- 
    get_type_expression(Env, X, integer),
    get_type_expression(Env, Y, integer), !.

% Relational expressions
get_type_expression(Env, greater(X, Y), boolean) :-
    get_type_rel_op(Env, greater, X, Y, boolean).
get_type_expression(Env, less(X, Y), boolean) :-
    get_type_rel_op(Env, less, X, Y, boolean).
get_type_expression(Env, ge(X, Y), boolean) :-
    get_type_rel_op(Env, ge, X, Y, boolean).
get_type_expression(Env, le(X, Y), boolean) :-
    get_type_rel_op(Env, le, X, Y, boolean).
get_type_expression(Env, ne(X, Y), boolean) :-
    get_type_rel_op(Env, ne, X, Y, boolean).
get_type_expression(Env, eql(X, Y), boolean) :-
    get_type_rel_op(Env, eql, X, Y, boolean).

% General get type predicate for relational operations
get_type_rel_op(Env, Op, X, Y, boolean) :-
    get_type_expression(Env, X, _),
    get_type_expression(Env, Y, _),
    rel_op(Op).

rel_op(greater).
rel_op(less).
rel_op(ge).
rel_op(le).
rel_op(ne).
rel_op(eql).

% Type check assignment
type_check_assignment(Env, T, Y, Var) :- 
    (get_type_expression(Env, Y, T) -> true ; throw(type_mismatch(assign(Var, Y)))).

% Type checking one statement
type_check_stmt(Env, assign(X, Y)) :- 
    lookup(Env, X, id(X, _, T)),
    type_check_assignment(Env, T, Y, X), !.

type_check_stmt(Env, call(Func, [X])) :- 
    (is_write_func(Func); is_write_ln_func(Func)),
    get_type_expression(Env, X, _), !.

type_check_stmt(Env, if(E, S1)) :-
    get_type_expression(Env, E, Type),
    (  Type = boolean
    -> type_check_body(Env, S1)
    ;  throw(type_mismatch(if(E, S1)))
    ), !.

type_check_stmt(Env, if(E, S1, S2)) :-
    get_type_expression(Env, E, Type),
    (  Type = boolean
    -> (  type_check_body(Env, S1),
            type_check_body(Env, S2)
        )
    ;  throw(type_mismatch(if(E, S1, S2)))
    ), !.

type_check_stmt(Env, while(E, S)) :-
    get_type_expression(Env, E, Type),
    (  Type = boolean
    -> type_check_body(Env, S)
    ;  throw(type_mismatch(while(E, S)))
    ), !.

type_check_stmt(Env, do(L, E)) :-
    type_check_body(Env, L),
    get_type_expression(Env, E, Type),
    (  Type = boolean
    -> true
    ;  throw(type_mismatch(do(L, E)))
    ), !.

type_check_stmt(Env, loop(E, S)) :-
    get_type_expression(Env, E, Type),
    (  Type = integer
    -> type_check_body(Env, S)
    ;  throw(type_mismatch(loop(E, S)))
    ), !.

% Type check one block
type_check_body(_, []) :- !.
type_check_body(env(L, B, T), [var(X, Y) | _]) :- 
    has_declared(X, env(L, B, T)), !,
    throw(redeclare_identifier(var(X, Y))).
type_check_body(env(L, B, T), [var(X, Y) | L1]) :- 
    T1 is T + 1, 
    type_check_body(env([id(X, var, Y) | L], B, T1), L1), !.
type_check_body(Env, [X | L]) :- 
    type_check_stmt(Env, X), 
    type_check_body(Env, L), !.

% Type checking a procedure
type_check_one_func(Env, proc(_, Y, Z)):- 
    create_env(Y, Env, env(L, _, T)),
    type_check_body(env(L, T, T), Z), !.

% Type checking a list of procedures
type_check_func(Env, [], Env) :- !.
type_check_func(_, [proc(X, _, _) | _], _) :- 
    is_builtin(X), !,
    throw(redeclare_procedure(X)).
type_check_func(_, [proc(X, _, _) | _], _) :- 
    has_declared(X, _), !,
    throw(redeclare_procedure(X)).
type_check_func(env(Env, B, T), [proc(X, Y, Z) | L], Env1) :- 
    T1 is T + 1,
    type_check_one_func(env([id(X, proc, proc(Y, Z)) | Env], T1, T1), proc(X, Y, Z)), !,
    type_check_func(env([id(X, proc, proc(Y, Z)) | Env], B, T1), L, Env1), !.

% Check if X has been declared in the symbol table from B+1 to T
has_declared(X, env([id(X, _, _) | _], B, T)) :- 
    T > B, !.
has_declared(X, env([_ | L], B, T)) :- 
    T1 is T - 1, 
    has_declared(X, env(L, B, T1)).

% Create a symbol table from the list of variable or constant declarations
create_env([], L, L).
create_env([var(X, Y) | _], env(_, 0, _), _) :- 
    is_builtin(X), !,
    throw(redeclare_identifier(var(X, Y))).
create_env([var(X, Y) | _], L1, _) :- 
    has_declared(X, L1), !,
    throw(redeclare_identifier(var(X, Y))).
create_env([var(X, Y) | L], env(L1, B, T), L2) :- 
    T1 is T + 1, 
    create_env(L, env([id(X, Y, _) | L1], B, T1), L2).

% Update the variable in the environment
update_var(I, V, env(L, B, T), env(L1, B, T)) :-
    update_var_list(I, V, L, L1).

% Update the variable in the list
update_var_list(I, _, [], _) :- throw(undeclare_identifier(I)).
update_var_list(I, V, [id(I, Type, _) | L], [id(I, Type, V) | L]) :- !.
update_var_list(I, V, [H | L], [H | L1]) :-
    update_var_list(I, V, L, L1), !.

% Print the environment for debugging
print_env(env(L, _, _)) :-
    writeln("Environment:"),
    print_env_list(L).

print_env_list([]).
print_env_list([id(X, Type, V) | L]) :-
    format("~w: ~w = ~w~n", [Type, X, V]),
    print_env_list(L), !.

debug_log(Message) :-
    format("DEBUG: ~w~n", [Message]).

is_builtin(readInt).
is_builtin(writeIntLn).
is_builtin(writeInt).
is_builtin(readReal).
is_builtin(writeRealLn).
is_builtin(writeReal).
is_builtin(readBool).
is_builtin(writeBoolLn).
is_builtin(writeBool).
is_builtin(writeLn).
is_builtin(writeStrLn).
is_builtin(writeStr).

% Check if the function is a write function
is_write_func(writeInt).
is_write_func(writeReal).
is_write_func(writeBool).
is_write_func(writeStr).

% Check if the function is a writeLn function
is_write_ln_func(writeIntLn).
is_write_ln_func(writeRealLn).
is_write_ln_func(writeBoolLn).
is_write_ln_func(writeStrLn).

% For runtime
create_runtime_env(X, X).

% Reduce expressions
% General reduce predicate for arithmetic operations
reduce_arith(config(Op, E1, E2), Env, R) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    arith_op(Op, V1, V2, R), !.

arith_op(add, V1, V2, R) :- R is V1 + V2.
arith_op(sub, V1, V2, R) :- R is V1 - V2.
arith_op(times, V1, V2, R) :- R is V1 * V2.
arith_op(rdiv, V1, V2, R) :- R is V1 / V2.
arith_op(idiv, V1, V2, R) :- R is V1 // V2.
arith_op(imod, V1, V2, R) :- R is mod(V1, V2).

% Reduce for unary subtraction
reduce(config(sub(E1), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    R is -V1, !.

% Specific reduce predicates using the general one
reduce(config(add(E1, E2), Env), config(R, Env)) :-
    reduce_arith(config(add, E1, E2), Env, R), !.

reduce(config(sub(E1, E2), Env), config(R, Env)) :-
    reduce_arith(config(sub, E1, E2), Env, R), !.

reduce(config(times(E1, E2), Env), config(R, Env)) :-
    reduce_arith(config(times, E1, E2), Env, R), !.

reduce(config(rdiv(E1, E2), Env), config(R, Env)) :-
    reduce_arith(config(rdiv, E1, E2), Env, R), !.

reduce(config(idiv(E1, E2), Env), config(R, Env)) :-
    reduce_arith(config(idiv, E1, E2), Env, R), !.

reduce(config(imod(E1, E2), Env), config(R, Env)) :-
    reduce_arith(config(imod, E1, E2), Env, R), !.

% Reduce for relational operations
reduce(config(less(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    get_type_expression(Env, less(V1, V2), boolean),
    (  V1 < V2
    -> R = true
    ;  R = false
    ), !.

reduce(config(greater(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    get_type_expression(Env, greater(V1, V2), boolean),
    (  V1 > V2
    -> R = true
    ;  R = false
    ), !.

reduce(config(ge(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    get_type_expression(Env, ge(V1, V2), boolean),
    (  V1 >= V2
    -> R = true
    ;  R = false
    ), !.

reduce(config(le(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    get_type_expression(Env, le(V1, V2), boolean),
    (  V1 =< V2
    -> R = true
    ;  R = false
    ), !.

reduce(config(ne(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    get_type_expression(Env, ne(V1, V2), boolean),
    (  V1 \= V2
    -> R = true
    ;  R = false
    ), !.

reduce(config(eql(E1, E2), Env), config(R, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    get_type_expression(Env, eql(V1, V2), boolean),
    (  V1 == V2
    -> R = true
    ;  R = false
    ), !.

reduce(config(I, Env), config(V, Env)) :-
    atom1(I),
    lookup(Env, I, id(I, _, V)), !.

reduce(config(E, Env), config(E, Env)) :- !.

%Helper function for reduce_loop
reduce_loop(0, _, Env, Env) :- !.
reduce_loop(Count, S, Env, Env1) :-
    Count > 0,
    reduce_stmt(config(S, Env), Env2),
    NextCount is Count - 1,
    reduce_loop(NextCount, S, Env2, Env1).

% Reduce all expressions in the list until there is no expression to reduce
reduce_all(config(V, Env), config(V, Env)) :- number(V), !.
reduce_all(config(V, Env), config(V, Env)) :- boolean(V), !.
reduce_all(config(str(V), Env), config(V, Env)) :- !.
reduce_all(config(E, Env), config(E2, Env)) :-
    reduce(config(E, Env), config(E1, Env)),
    reduce_all(config(E1, Env), config(E2, Env)), !.

% Reduce a list of expressions
reduce_all(config([], Env), config([], Env)) :- !.
reduce_all(config([S | L], Env), config([S1 | L1], Env1)) :-
    reduce_all(config(S, Env), config(S1, Env)),
    reduce_all(config(L, Env), config(L1, Env1)), !.

% Reduce a list of statements (body)
reduce_stmt(config([], Env), Env) :- !.
reduce_stmt(config([var(X, Type) | Stmts], Env), Env1) :- % Handle variable declarations
    ( has_declared(X, Env) ->
        throw(redeclare_identifier(var(X, Type)))
    ; NewEnv = env([id(X, Type, _) | Env], 0, 0), % Add the variable to the environment with the correct type
      reduce_stmt(config(Stmts, NewEnv), Env1)
    ), !.
reduce_stmt(config([Stmt | Stmts], Env), Env1) :-
    reduce_stmt(config(Stmt, Env), Env2),
    reduce_stmt(config(Stmts, Env2), Env1), !.

% Reduce an if statement
reduce_stmt(config(if(E, S1), Env), Env1) :-
    reduce_all(config(E, Env), config(V, Env)),
    (  V == true
    -> reduce_stmt(config(S1, Env), Env1)
    ;  Env1 = Env
    ), !.

% Reduce an if-else statement
reduce_stmt(config(if(E, S1, S2), Env), Env1) :-
    reduce_all(config(E, Env), config(V, Env)),
    (  V == true
    -> reduce_stmt(config(S1, Env), Env1)
    ;  reduce_stmt(config(S2, Env), Env1)
    ), !.

% Reduce an while statement
reduce_stmt(config(while(E, S), Env), Env1) :-
    reduce_all(config(E, Env), config(V, Env)),
    (  V == true
    -> reduce_stmt(config(S, Env), Env2),
       reduce_stmt(config(while(E, S), Env2), Env1)
    ;  Env1 = Env
    ), !.

% Reduce an do statement
reduce_stmt(config(do(L, E), Env), Env1) :-
    reduce_stmt(config(L, Env), Env2),
    reduce_all(config(E, Env2), config(V, Env2)),
    (  V == true
    -> reduce_stmt(config(do(L, E), Env2), Env1)
    ;  Env1 = Env2
    ), !.

% Reduce a loop statement
reduce_stmt(config(loop(E, S), Env), Env1) :-
    reduce_all(config(E, Env), config(Count, Env)),
    reduce_loop(Count, S, Env, Env1), !.

% Reduce an assignment statement
reduce_stmt(config(assign(VarName, E1), Env), Env1) :-
    lookup(Env, VarName, id(VarName, DeclaredType, _)),
    reduce_all(config(E1, Env), config(Value, Env)),
    get_type_expression(Env, Value, ValueType),
    (DeclaredType = ValueType ->
        update_var(VarName, Value, Env, Env1)
    ;
        throw(type_mismatch(assign(VarName, Value)))
    ), !.

% Handle call statements, including built-in and user-defined functions
reduce_stmt(config(call(Func, Args), Env), Env) :-
    (is_builtin(Func) -> 
        handle_builtin(Func, Args, Env); 
        handle_user_defined(Func, Args, Env)), !.

% Handle built-in functions generically
handle_builtin(Func, [X], Env) :-
    reduce_all(config(X, Env), config(V, Env)),
    (is_write_func(Func) -> write(V) ; true),
    (is_write_ln_func(Func) -> writeln(V) ; true),
    flush_output.

% Handle user-defined functions
handle_user_defined(Func, Args, Env) :-
    lookup(Env, Func, id(Func, proc, proc(Params, Body))),
    create_env(Params, env([], 0, 0), LocalEnv),
    bind_args(Params, Args, LocalEnv, Env1),
    reduce_stmt(config(Body, env(Env1, 0, 0)), _), !.

% Bind arguments to parameters in the local environment
bind_args([], [], Env, Env) :- !.
bind_args([param(P, _) | Params], [Arg | Args], LocalEnv, Env) :-
    reduce_all(config(Arg, Env), config(Value, Env)),
    create_env([var(P, Value)], LocalEnv, Env1),
    bind_args(Params, Args, Env1, Env), !.
