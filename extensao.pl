% 100032 Mateus Spencer
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- consult('dados.pl'), consult('keywords.pl'). % ficheiros a importar.

%Periodos pode ser ou um periodo normal ou um semestre
%pelo menos um desses periodos num caso de um semestre tem de tar na lista de periodos permitidos
testa_periodos(Periodos, List) :- 
    (   sub_atom(Periodos, _, _, _, '_') 
    ->
        (   sub_atom(Periodos, _, _, _, '1') 
        ->
            (member(p1,List);
            member(p2,List)
            );
            (member(p3,List);
            member(p4,List)
            )
        );
        member(Periodos, List)
    ).

eventosSemSala(Eventos) :-
    findall(E,evento(E, _, _, _, semSala),Eventos1),
    sort(Eventos1,Eventos).


eventosSemSalasDiaSemana(Dia, Eventos):-
    findall(E,(evento(E, _, _, _, semSala), horario(E, Dia, _, _, _, _)),Eventos1),
    sort(Eventos1,Eventos).

eventosSemSalasPeriodo(Periodos, Eventos):-
    findall(E,(evento(E, _, _, _, semSala), horario(E, _, _, _, _, Periodo), testa_periodos(Periodo,Periodos)),Eventos1),
    sort(Eventos1,Eventos).

organizaEventos(Lista, Periodo, SortedList):-
    organizaEventos_aux(Lista, Periodo, L),
    %!, ??
    sort(L, SortedList).
organizaEventos_aux([], _, []).
organizaEventos_aux([H|R], Periodo, [H|L]):-
    horario(H, _, _, _, _, X),
    testa_periodos(X,[Periodo]),
    !,
    organizaEventos(R, Periodo,L).
organizaEventos_aux([_|R], Periodo, L):-
    organizaEventos(R, Periodo,L).

eventosMenoresQue(Duracao, Eventos):-
    findall(E,(evento(E, _, _, _, _), horario(E, _, _, _, Dur, _), Dur =< Duracao), Eventos1),
    sort(Eventos1,Eventos).

eventosMenoresQueBool(ID, Duracao):-
    horario(ID, _, _, _, Dur, _),
    !,
    Dur =< Duracao.

procuraDisciplinas(Curso, ListaDisciplinas):-
    findall(D,(evento(ID, D, _, _, _), turno(ID, Curso, _, _)), Disciplinas1),
    sort(Disciplinas1, ListaDisciplinas).

%organizaDisciplinas/3
%horasCurso/5
%evolucaoHorasCurso/2

%(...)

%Mesa de Jantar