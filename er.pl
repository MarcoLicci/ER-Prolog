%%% TODO tutti i cut
%   Rivedere commenti prima consegna, terminologia "riconosce" non
%   adeguata nel contesto (vero se...)


%%% TODO
%   Is regexp con eccezione predicato senza argomenti e ordine

is_regexp(RE) :-
    atomic(RE),
    !.
is_regexp(RE) :-
    % Vero per qualunque termine compound
    % il cui funtore non sia un operatore,
    % accettabili compound con arita' zero
    compound_name_arguments(RE, Functor, _),
    not(is_operator(Functor)),
    !.
is_regexp(RE) :-
    % Falsifica nel caso in cui un
    % operatore non abbia argomenti
    compound_name_arguments(RE, _, []), 
    !,
    fail.
is_regexp(RE) :-
    RE =.. [seq | REs],
    !,
    is_regexp_list(REs).
is_regexp(RE) :-
    RE =.. [or | REs],
    !,
    is_regexp_list(REs).
is_regexp(RE) :-
    RE =.. [star, Inner_RE],
    !,
    is_regexp(Inner_RE).
is_regexp(RE) :-
    RE =.. [plus, Inner_RE],
    !,
    is_regexp(Inner_RE).

is_regexp_list([RE]) :-
    is_regexp(RE).
is_regexp_list([RE | REs]) :-
    is_regexp(RE),
    is_regexp_list(REs).

is_operator(seq).
is_operator(or).
is_operator(star).
is_operator(plus).

nfa_regexp_comp(FA_Id, RE) :-
    nonvar(FA_Id),
    is_regexp(RE),
    gensym(q, Initial),
    assert(nfa_initial(FA_Id, Initial)),
    gensym(q, Final),
    assert(nfa_final(FA_Id, Final)),
    nfa_regexp_comp(FA_Id, RE, Initial, Final).

% Riconosce atomi
nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    atomic(RE),
    assert(nfa_delta(FA_Id, Initial, RE, Final)).

nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    RE =.. [Op | REs],
    nfa_regexp_comp(FA_Id, Op, REs, Initial, Final).

nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    RE =.. [Op | REs],
    nfa_regexp_comp(FA_Id, Op, REs, Initial, Final).

nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    RE =.. [Op | REs],
    nfa_regexp_comp(FA_Id, Op, REs, Initial, Final).

% Riconosce compound che non siano riservati
nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    compound_name_arguments(RE, _, _),
    assert(nfa_delta(FA_Id, Initial, RE, Final)).

%%% Compound riservati con arità0 !!! Eliminare (?)
nfa_regexp_comp(_, RE, _, _) :-
    compound_name_arguments(RE, _, []),
    !,
    fail.
    %%% Throw error, NON DOVREBBE SERVIRE DOPO IS_REGEX

% Generalizzabile?
nfa_regexp_comp(FA_Id, seq, [RE], Initial, Final) :-
    nfa_regexp_comp(FA_Id, RE, Initial, Final).
nfa_regexp_comp(FA_Id, seq, [RE | REs], Initial, Final) :-
    gensym(q, Internal_Final),
    nfa_regexp_comp(FA_Id, RE, Initial, Internal_Final),
    nfa_regexp_comp(FA_Id, seq, REs, Internal_Final, Final).

nfa_regexp_comp(FA_Id, or, [RE], Initial, Final) :-
    gensym(q, Internal_Initial),
    gensym(q, Internal_Final),
    assert(nfa_delta(FA_Id, Initial, epsilon, Internal_Initial)),
    nfa_regexp_comp(FA_Id, RE, Internal_Initial, Internal_Final),
    assert(nfa_delta(FA_Id, Internal_Final, epsilon, Final)).
nfa_regexp_comp(FA_Id, or, [RE | REs], Initial, Final) :-
    nfa_regexp_comp(FA_Id, or, [RE], Initial, Final),
    nfa_regexp_comp(FA_Id, or, REs, Initial, Final).


nfa_regexp_comp(FA_Id, plus, [RE], Initial, Final) :-
    gensym(q, Internal_Initial),
    gensym(q, Internal_Final),
    assert(nfa_delta(FA_Id, Initial, epsilon, Internal_Initial)),
    nfa_regexp_comp(FA_Id, RE, Internal_Initial, Internal_Final),
    assert(nfa_delta(FA_Id, Internal_Final, epsilon, Internal_Initial)),
    assert(nfa_delta(FA_Id, Internal_Final, epsilon, Final)).

nfa_regexp_comp(FA_Id, star, [RE], Initial, Final) :-
    nfa_regexp_comp(FA_Id, plus, [RE], Initial, Final),
    assert(nfa_delta(FA_Id, Initial, epsilon, Final)).


nfa_test(FA_Id, Input) :-
    %%%% TODO Verifica input
    %%%% Throw nfa not found (?)
    nfa_initial(FA_Id, State),
    nfa_test(FA_Id, Input, State).

nfa_test(FA_Id, [Input | Inputs], State) :-
    nfa_delta(FA_Id, State, Input, Next),
    nfa_test(FA_Id, Inputs, Next).
nfa_test(FA_Id, Input, State) :-
    nfa_delta(FA_Id, State, epsilon, Next),
    nfa_test(FA_Id, Input, Next).
nfa_test(FA_Id, [], State) :-
    nfa_final(FA_Id, State).

:- dynamic
    nfa_initial/2,
    nfa_delta/4,
    nfa_final/2.