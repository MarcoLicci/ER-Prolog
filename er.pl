%%% TODO tutti i cut
%   Rivedere commenti prima consegna, terminologia "riconosce" non
%   adeguata nel contesto (vero se...)

%%%TODO
% Listing e predicati accessori

%%%
% Indentare su EMACS


%%% TODO
%   Is regexp con eccezione predicato senza argomenti e ordine

%%% !!!! ricordo male o antoniotti aveva detto che non voleva usassimo il ___ not ___? !!!

is_regexp(RE) :-
    atomic(RE),
    !.
is_regexp(RE) :-
    % Vero per qualunque termine compound
    % il cui funtore non sia un operatore,
    % accettabili compound con arita' zero
    compound_name_arguments(RE, Functor, _),
    Functor \= seq,
    Functor \= or,
    Functor \= star,
    Functor \= plus,
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
    RE =.. [star | REs],
    !,
    REs = [Inner_RE],
    is_regexp(Inner_RE).
is_regexp(RE) :-
    RE =.. [plus | REs],
    !,
    REs = [Inner_RE],
    is_regexp(Inner_RE).
% Necessario?
%is_regexp(RE) :-
%    compound(RE).

is_regexp_list([RE]) :-
    is_regexp(RE),
    !.
is_regexp_list([RE | REs]) :-
    is_regexp(RE),
    is_regexp_list(REs).

nfa_regexp_comp(FA_Id, _) :-
    nfa_initial(FA_Id, _),
    !,
    fail.

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

% Riconosce compound di arita' 0,
% il predicato non viene mai valutato
% su compound dal funtore riservato perche'
% non sono Regexp
nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    compound_name_arguments(RE, _, []),
    assert(nfa_delta(FA_Id, Initial, RE, Final)).

nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    RE =.. [Op | REs],
    nfa_regexp_comp(FA_Id, Op, REs, Initial, Final).


%%TODO nfa comp a 5 argomenti puo' essere semplificato a 4 (likely)
%% UTILE PER NON RICHIEDERE LISTA PER PLUS E STAR


% Riconosce come simbolo quasiasi compound
% che non abbia un funtore riservato
nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    compound(RE),
    assert(nfa_delta(FA_Id, Initial, RE, Final)).

nfa_regexp_comp(FA_Id, seq, [RE], Initial, Final) :-
    nfa_regexp_comp(FA_Id, RE, Initial, Final).
nfa_regexp_comp(FA_Id, seq, [RE | REs], Initial, Final) :-
    gensym(q, Internal_Final),
    nfa_regexp_comp(FA_Id, RE, Initial, Internal_Final),
    nfa_regexp_comp(FA_Id, seq, REs, Internal_Final, Final).

nfa_regexp_comp(FA_Id, or, [RE], Initial, Final) :-
    gensym(q, Internal_Initial),
    gensym(q, Internal_Final),
    assert(nfa_delta(FA_Id, Initial, Internal_Initial)),
    nfa_regexp_comp(FA_Id, RE, Internal_Initial, Internal_Final),
    assert(nfa_delta(FA_Id, Internal_Final, Final)).
nfa_regexp_comp(FA_Id, or, [RE | REs], Initial, Final) :-
    nfa_regexp_comp(FA_Id, or, [RE], Initial, Final),
    nfa_regexp_comp(FA_Id, or, REs, Initial, Final).


nfa_regexp_comp(FA_Id, plus, [RE], Initial, Final) :-
    gensym(q, Internal_Initial),
    gensym(q, Internal_Final),
    assert(nfa_delta(FA_Id, Initial, Internal_Initial)),
    nfa_regexp_comp(FA_Id, RE, Internal_Initial, Internal_Final),
    assert(nfa_delta(FA_Id, Internal_Final, Internal_Initial)),
    assert(nfa_delta(FA_Id, Internal_Final, Final)).

nfa_regexp_comp(FA_Id, star, [RE], Initial, Final) :-
    nfa_regexp_comp(FA_Id, plus, [RE], Initial, Final),
    assert(nfa_delta(FA_Id, Initial, Final)).


nfa_test(FA_Id, Input) :-
    %%%% TODO Verifica input
    %%%% Throw nfa not found (?)
    nfa_initial(FA_Id, State),
    nfa_test(FA_Id, Input, State).

nfa_test(FA_Id, [Input | Inputs], State) :-
    nfa_delta(FA_Id, State, Input, Next),
    nfa_test(FA_Id, Inputs, Next).
nfa_test(FA_Id, Input, State) :-
    nfa_delta(FA_Id, State, Next),
    nfa_test(FA_Id, Input, Next).
nfa_test(FA_Id, [], State) :-
    nfa_final(FA_Id, State).

nfa_clear(FA_Id) :-
    retractall(nfa_initial(FA_Id, _)),
    retractall(nfa_delta(FA_Id, _, _, _)),
    retractall(nfa_delta(FA_Id, _, _)),
    retractall(nfa_final(FA_Id, _)).
nfa_clear() :-
    nfa_clear(_).

nfa_list(FA_Id) :-
    listing(nfa_initial(FA_Id, _)),
    listing(nfa_delta(FA_Id, _, _, _)),
    listing(nfa_delta(FA_Id, _, _)),
    listing(nfa_final(FA_Id, _)).
nfa_list() :-
    nfa_list(_).

:- dynamic
    nfa_initial/2,
    nfa_delta/4,
    nfa_delta/3,
    nfa_final/2.