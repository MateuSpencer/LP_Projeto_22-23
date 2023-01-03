% 100032 Mateus Spencer
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- consult('dados.pl'), consult('keywords.pl'). % ficheiros a importar.

separaPeriodos(Periodos, List) :-
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
    findall(E,(evento(E, _, _, _, semSala), horario(E, _, _, _, _, Periodo), separaPeriodos(Periodo,Periodos)),Eventos1),
    sort(Eventos1,Eventos).

%organizaEventos/3
%eventosMenoresQue/2
%eventosMenoresQueBool/2
%procuraDisciplinas/2
%organizaDisciplinas/3
%horasCurso/5
%evolucaoHorasCurso/2

%(...)

%Mesa de Jantar