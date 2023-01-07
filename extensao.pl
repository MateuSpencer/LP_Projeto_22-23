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

organizaDisciplinas([], _,[[],[]]).

organizaDisciplinas([Disciplina|T], Curso, [Semestre1,Semestre2]):-
    evento(ID, Disciplina, _, _, _), 
    turno(ID, Curso, _, _),
    !,
    organizaDisciplinas(T, Curso, [Semestre1_Rec,Semestre2_Rec]),
    horario(ID, _, _, _, _, Periodo),
    (testa_periodos(Periodo, [p1,p2]) 
    -> 
        (
        append([Disciplina], Semestre1_Rec, Semestre1_Aux),
        sort(Semestre1_Aux, Semestre1),
        Semestre2 = Semestre2_Rec
        )
    ;
        (
        append([Disciplina], Semestre2_Rec, Semestre2_Aux),
        sort(Semestre2_Aux, Semestre2),
        Semestre1 = Semestre1_Rec
        )
    ).

confirma_curso_ano(ID, Curso, Ano):-
    turno(ID, Curso,Ano,_),!.
horasCurso(Periodo, Curso, Ano, TotalHoras):-
        findall(Dur ,(horario(ID, _, _, _, Dur, Periodo_aux),testa_periodos(Periodo_aux, [Periodo]), confirma_curso_ano(ID, Curso, Ano)), Duracoes),
        sum_list(Duracoes, TotalHoras).

    evolucaoHorasCurso(Curso, Evolucao):-
        findall((Ano,Periodo,TotalHoras), (
            turno(_, Curso, Ano, _),
            fixa_periodo(Periodo),
            horasCurso(Periodo, Curso, Ano, TotalHoras)), 
        Evolucao_aux),
    sort(Evolucao_aux,Evolucao).

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, _):-
    (HoraFimEvento =< HoraInicioDada;
    HoraFimDada =< HoraInicioEvento
    ), !, fail.   
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
    HoraInicioEvento >= HoraInicioDada
    ->
        (
            HoraFimEvento =< HoraFimDada
            ->
                Horas is HoraFimEvento - HoraInicioEvento
            ; Horas is HoraFimDada - HoraInicioEvento
        )
    ; 
        (
            HoraFimEvento =< HoraFimDada
            ->
                Horas is HoraFimEvento - HoraInicioDada
            ; Horas is HoraFimDada - HoraInicioDada
        ).
    
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras):-
    findall(Horas, (
        horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, Periodo_aux),
        testa_periodos(Periodo_aux,[Periodo]),
        salas(TipoSala, Salas),
        evento(ID, _, _, _, Sala),
        member(Sala,Salas),
        ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)),
        SomaHoras_aux),
    sum_list(SomaHoras_aux, SomaHoras).

ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):-
    Duracao is HoraFim - HoraInicio,
    salas(TipoSala,Salas),
    length(Salas,Len),
    Max is  Len * Duracao.
    
percentagem(SomaHoras, Max, Percentagem):-
    Percentagem is (SomaHoras / Max) * 100.

ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados):-
    findall(casosCriticos(DiaSemana,TipoSala,Percentagem), (
        salas(TipoSala, _),
        horario(_, DiaSemana, _, _,_, Periodo),
        numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
        ocupacaoMax(TipoSala, HoraInicio, HoraFim, MaxHoras),
        percentagem(SomaHoras,MaxHoras,Percentagem_aux),
        Percentagem_aux >Threshold,
        ceiling(Percentagem_aux,Percentagem)
        ),
    Resultados_aux),
    sort(Resultados_aux,Resultados).


ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) :-
    permutation(ListaPessoas, Permutacao),
    verificaRestricoes(ListaRestricoes, Permutacao),
    divideMesa(Permutacao, OcupacaoMesa),
    !.

divideMesa([X1, X2, X3, X4, X5, X6, X7, X8], [[X1, X2, X3], [X4, X5], [X6, X7, X8]]).

verificaRestricoes([], _).
verificaRestricoes([Restricao|Restricoes], OcupacaoMesa) :-
    satisfazRestricao(Restricao, OcupacaoMesa),
    !,
    verificaRestricoes(Restricoes, OcupacaoMesa).

satisfazRestricao(cab1(NomePessoa), [_, _, _, NomePessoa, _, _, _, _]).
satisfazRestricao(cab2(NomePessoa), [_, _, _, _, NomePessoa, _, _, _]).

satisfazRestricao(honra(NomePessoa1, NomePessoa2), [_, _, _, NomePessoa1, _, NomePessoa2, _, _]).
satisfazRestricao(honra(NomePessoa1, NomePessoa2), [_, _, NomePessoa2, _, NomePessoa1, _, _, _]).

satisfazRestricao(lado(NomePessoa1, _),[_, _, _, NomePessoa1, _, _, _, _]):- !, fail.
satisfazRestricao(lado(_, NomePessoa2),[_, _, _, NomePessoa2, _, _, _, _]):- !, fail.
satisfazRestricao(lado(NomePessoa1, _),[_, _, _, _, NomePessoa1, _, _, _]):- !, fail.
satisfazRestricao(lado(_, NomePessoa2),[_, _, _, _, NomePessoa2, _, _, _]):- !, fail.
satisfazRestricao(lado(NomePessoa1, NomePessoa2), [H|[H1|R]]):-
    (
        NomePessoa1 = H, NomePessoa2 = H1;
        NomePessoa2 = H, NomePessoa1 = H1
    );
    satisfazRestricao(lado(NomePessoa1, NomePessoa2), [H1|R]).
satisfazRestricao(naoLado(NomePessoa1, NomePessoa2), OcupacaoMesa):-
    \+(satisfazRestricao(lado(NomePessoa1, NomePessoa2), OcupacaoMesa)).

satisfazRestricao(frente(NomePessoa1, _),[_, _, _, NomePessoa1, _, _, _, _]):- !, fail.
satisfazRestricao(frente(_, NomePessoa2),[_, _, _, NomePessoa2, _, _, _, _]):- !, fail.
satisfazRestricao(frente(NomePessoa1, _),[_, _, _, _, NomePessoa1, _, _, _]):- !, fail.
satisfazRestricao(frente(_, NomePessoa2),[_, _, _, _, NomePessoa2, _, _, _]):- !, fail.
satisfazRestricao(frente(NomePessoa1, NomePessoa2), OcupacaoMesa):-
    nth0(Index, OcupacaoMesa, NomePessoa1),
    (   Index < 3
        ->Index2 is Index + 5
        ; Index2 is Index - 5
    ),
    nth0(Index2, OcupacaoMesa, NomePessoa2).
satisfazRestricao(naoFrente(NomePessoa1, NomePessoa2), OcupacaoMesa):-
    \+(satisfazRestricao(frente(NomePessoa1, NomePessoa2), OcupacaoMesa)).