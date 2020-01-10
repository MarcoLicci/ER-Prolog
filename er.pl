% Licci Marco 844774

% Vero se per espressioni RE corrette
is_regexp(RE) :-
    atomic(RE),
    !.
% Vero per qualunque termine compound
% il cui funtore non sia un operatore.
% Accettabile compound con arita' zero
is_regexp(RE) :-
    compound_name_arguments(RE, Functor, _),
    Functor \= seq,
    Functor \= or,
    Functor \= star,
    Functor \= plus,
    !.
% Falso nel caso in cui un
% operatore non abbia argomenti
is_regexp(RE) :-
    compound_name_arguments(RE, _, []),
    !,
    fail.

% Identificato l'operatore principale della
% regexp, si verifica che abbia numero di elementi
% non nullo e che le sottoespressioni siano anch'esse regexp.
% Ne nel caso degli operatori star e plus is_regexp e' ver
% a solo se il numero di argomenti e' esattamente 1
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

is_regexp_list([RE]) :-
    is_regexp(RE),
    !.
is_regexp_list([RE | REs]) :-
    is_regexp(RE),
    is_regexp_list(REs).

%%% Compilazione
%%% Generazione basata sull'algoritmo di Thompson

% Il predicato e' falso per identificatori FA_Id
% gia' presenti nella base
nfa_regexp_comp(FA_Id, _) :-
    nfa_initial(FA_Id, _),
    !,
    fail.

% Vero se FA_Id e' legato e l'espressione RE e' valida.
% Compila un NFA per RE aggiungendo alla base di dati
% dei predicati rappresentanti le transizioni (nfa_delta/4)
% e le epsilon-transizioni (nfa_delta/3)
% Genera stati iniziali e finali attraverso gensym,
% le transizioni vengono gestite da nfa_regexp_comp/4
nfa_regexp_comp(FA_Id, RE) :-
    nonvar(FA_Id),
    is_regexp(RE),
    gensym(q, Initial),
    assert(nfa_initial(FA_Id, Initial)),
    gensym(q, Final),
    assert(nfa_final(FA_Id, Final)),
    nfa_regexp_comp(FA_Id, RE, Initial, Final).

% Produce le transizioni relative alla stringa vuota
nfa_regexp_comp(FA_Id, epsilon, Initial, Final) :-
    assert(nfa_delta(FA_Id, Initial, Final)),
    !.
% Produce le transizioni necessarie per il riconoscimento
% di simboli atomici
nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    atomic(RE),
    assert(nfa_delta(FA_Id, Initial, RE, Final)),
    !.
% Gestisce compound di arita' 0,
% il predicato non viene mai valutato
% su compound aventi funtore riservato perche'
% non sono regexp
nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    compound_name_arguments(RE, _, []),
    assert(nfa_delta(FA_Id, Initial, RE, Final)),
    !.

% Separa il funtore dagli argomenti e chiama
% l'apposito prediato
nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    RE =.. [Op | REs],
    nfa_regexp_comp(FA_Id, Op, REs, Initial, Final),
    !.
% Riconosce come simbolo quasiasi compound
% che non abbia un funtore riservato
nfa_regexp_comp(FA_Id, RE, Initial, Final) :-
    compound(RE),
    assert(nfa_delta(FA_Id, Initial, RE, Final)).

%  Operatore seq
% Genera le transizioni per l'operatore seq,
% si basa sulla possibilita' di ricondurre
% il seq n-ario ad una sua versione binaria in cui
% il primo argomento coincide con quello della versione
% n-aria ed il secondo e' costruito applicando ricorsivamente
% il predicato binario al resto degli argomenti.
nfa_regexp_comp(FA_Id, seq, [RE], Initial, Final) :-
    nfa_regexp_comp(FA_Id, RE, Initial, Final).
nfa_regexp_comp(FA_Id, seq, [RE | REs], Initial, Final) :-
    gensym(q, Internal_Final),
    nfa_regexp_comp(FA_Id, RE, Initial, Internal_Final),
    nfa_regexp_comp(FA_Id, seq, REs, Internal_Final, Final).

%  Operatore or
% Basato sullo stesso principio utilizzato nella compilazione
% dell'operatore seq, i sottoautomi pero' non sono concatenati,
% le transizioni in questo caso formano dei cammini paralleli
% aventi come stato iniziale e finali quelli dell'automa principale
% Caso base: lista da un elemento, generate epsilon-transizioni
% intermedie.
% Caso ricorsivo: chiamate ricorsive alla funzione sul primo elemento
% e sul resto delle espressioni
nfa_regexp_comp(FA_Id, or, [RE], Initial, Final) :-
    gensym(q, Internal_Initial),
    gensym(q, Internal_Final),
    assert(nfa_delta(FA_Id, Initial, Internal_Initial)),
    nfa_regexp_comp(FA_Id, RE, Internal_Initial, Internal_Final),
    assert(nfa_delta(FA_Id, Internal_Final, Final)).
nfa_regexp_comp(FA_Id, or, [RE | REs], Initial, Final) :-
    nfa_regexp_comp(FA_Id, or, [RE], Initial, Final),
    nfa_regexp_comp(FA_Id, or, REs, Initial, Final).

%  Operatore plus
% Gestione dell'operatore plus, accettato un unico argomento,
% l'automa generato dalla sottoespressione viene collegato
nfa_regexp_comp(FA_Id, plus, [RE], Initial, Final) :-
    gensym(q, Internal_Initial),
    gensym(q, Internal_Final),
    assert(nfa_delta(FA_Id, Initial, Internal_Initial)),
    nfa_regexp_comp(FA_Id, RE, Internal_Initial, Internal_Final),
    assert(nfa_delta(FA_Id, Internal_Final, Internal_Initial)),
    assert(nfa_delta(FA_Id, Internal_Final, Final)).

%  Operatore star
% Riconduce la gestione dell'operatore star a quella del plus
% aggiungendo una transizione da stato iniziale a finale
nfa_regexp_comp(FA_Id, star, [RE], Initial, Final) :-
    nfa_regexp_comp(FA_Id, plus, [RE], Initial, Final),
    assert(nfa_delta(FA_Id, Initial, Final)).


%%% Test

% Vero se l'automa accetta l'input
nfa_test(FA_Id, Input) :-
    nfa_initial(FA_Id, State),
    nfa_accept(FA_Id, State, Input, []),
    !.
% Vero se l'automa di trova in uno stato finale
% e la lista di input e' vuota
nfa_accept(FA_Id, State, [], _) :-
    nfa_final(FA_Id, State).
% Vero se e' presente nella base una transizione
% per il simbolo in input ed e' vera nfa_accept
nfa_accept(FA_Id, State, [Input | Inputs], _) :-
    nfa_delta(FA_Id, State, Input, Next),
    nfa_accept(FA_Id, Next, Inputs, []).
% Vero se esiste una epsilon transizione dallo stato
% corrente che porti all'accettazione da nfa_accept.
% Si rende necessaria una lista di stati visitati al
% fine di evitare i cammini ciclici presenti nell'automa
nfa_accept(FA_Id, State, Inputs, Visited) :-
    nfa_delta(FA_Id, State, Next),
    not_member(State, Visited),
    nfa_accept(FA_Id, Next, Inputs, [State | Visited]).

not_member(Elem, List) :-
    member(Elem, List),
    !,
    fail.
not_member(_, _).

%%% Utilita'


% Rimozione di automi per ID
nfa_clear(FA_Id) :-
    retractall(nfa_initial(FA_Id, _)),
    retractall(nfa_delta(FA_Id, _, _, _)),
    retractall(nfa_delta(FA_Id, _, _)),
    retractall(nfa_final(FA_Id, _)).
% Pulizia della base di dati
nfa_clear() :-
    nfa_clear(_).

% Lista automi per ID
nfa_list(FA_Id) :-
    listing(nfa_initial(FA_Id, _)),
    listing(nfa_delta(FA_Id, _, _, _)),
    listing(nfa_delta(FA_Id, _, _)),
    listing(nfa_final(FA_Id, _)).
% Lista tutti gli automi nella base
nfa_list() :-
    nfa_list(_).

% Predicati dinamici definiti in compilazione
:- dynamic
       nfa_initial/2,
       nfa_delta/4,
       nfa_delta/3,
       nfa_final/2.
