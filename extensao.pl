% 100032 Mateus Spencer
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- consult('dados.pl'), consult('keywords.pl'). % ficheiros a importar.

eventosSemSala(Eventos) :-
    findall(E,evento(E, _, _, _, semSala),Eventos).


eventosSemSalasDiaSemana(Dia, Eventos):-
    findall(E,(evento(E, _, _, _, semSala), horario(E, Dia, _, _, _, _)),Eventos).

%eventosSemSalasPeriodo/2

%organizaEventos/3
%eventosMenoresQue/2
%eventosMenoresQueBool/2
%procuraDisciplinas/2
%organizaDisciplinas/3
%horasCurso/5
%evolucaoHorasCurso/2

%(...)

%Mesa de Jantar