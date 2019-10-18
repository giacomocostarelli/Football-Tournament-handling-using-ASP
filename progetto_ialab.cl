% Squadre partecipanti alla UEFA Champions League 2018/2019.

nazione(
    belgio;         francia;        germania;
    grecia;         inghilterra;    italia;
    olanda;         portogallo;     repubblica_ceca;
    russia;         serbia;         spagna;
    svizzera;       turchia;        ucraina;
).

citta(
    bruges;         monaco;         parigi;
    lione;          dortmund;       gelsenkirchen;
    valencia;       atene;          hoffenheim_sinsheim;
    londra;         liverpool;      manchester;
    milano;         napoli;         roma;
    torino;         eindhoven;      amsterdam;
    porto;          lisbona;        plzen;
    mosca;          belgrado;       madrid;
    berna;          barcellona;     monaco_di_baviera;
    istanbul;       donetsk;
).

squadra(
    club_brugge;        monaco;         paris_saint_germain;
    olympic_lione;      schalke_04;     borussia_dortmund;
    bayern_monaco;      hoffenheim;     aek;
    tottenham_hotspur;  liverpool;      manchester_city;
    manchester_united;  inter;          napoli;
    psv_eindhoven;      roma;           ajax;
    viktoria_plzen;     porto;          benfica;
    lokomotiv_mosca;    cska_mosca;     stella_rossa;
    atletico_madrid;    barcellona;     real_madrid;
    galatasaray;        valencia;       young_boys;
    shakhtar_donetsk;   juventus;
).

% snc --> SquadraNazioneCittà

snc (
    club_brugge, belgio, bruges;
    monaco, francia, monaco;
    paris_saint_germain, francia, parigi;
    olympic_lione, francia, lione;
    borussia_dortmund, germania, dortmund;
    schalke_04, germania, gelsenkirchen ;
    bayern_monaco, germania, monaco_di_baviera;
    hoffenheim, germania, hoffenheim_sinsheim;
    aek, grecia, atene;
    tottenham_hotspur, inghilterra, londra;
    liverpool, inghilterra, liverpool;
    manchester_city, inghilterra, manchester;
    manchester_united, inghilterra, manchester;
    inter, italia, milano;
    napoli, italia, napoli;
    roma, italia, roma;
    juventus, italia, torino;
    psv_eindhoven, olanda, eindhoven;
    ajax, olanda, amsterdam;
    porto, portogallo, porto;
    benfica, portogallo, lisbona;
    viktoria_plzen, repubblica_ceca, plzen;
    lokomotiv_mosca, russia, mosca;
    cska_mosca, russia, mosca;
    stella_rossa, serbia, belgrado;
    atletico_madrid, spagna, madrid;
    barcellona, spagna, barcellona;
    real_madrid, spagna, madrid;
    valencia, spagna, valencia;
    young_boys, svizzera, berna;
    galatasaray, turchia, istanbul;
    shakhtar_donetsk, ucraina, donetsk;
).

% # # # GIRONI # # #
girone (1..8).

% Un girone contiene 4_4 squadre.  ( NOTAZIONE: X_Y --> occorre almeno 'X' ed al massimo 'Y' volte ).
4 {girone_contiene_squadra(G,S,N,C) : snc(S,N,C)} 4 :- girone(G).

% Due squadre in forma di tripla snc con il secondo argomento della tripla uguale provengono dalla stessa nazione.
uguale_nazione(Squadra1, Squadra2) :-
    snc(Squadra1, Nazione1, _),
    snc(Squadra2, Nazione2, _),
    Squadra1 != Squadra2,
    Nazione1 == Nazione2.


% Due squadre in forma di tripla snc con il terzo argomento della tripla uguale giocano nella stessa città.
uguale_citta(Squadra1, Squadra2) :-
    snc(Squadra1, _, C1),
    snc(Squadra2, _, C2),
    C1 == C2,
    Squadra1 != Squadra2.

% Scartare tutti gli answer set in cui esistono due squadre della stessa nazione nello stesso girone (INTEGRITY CONSTRAINT).
:-girone_contiene_squadra(G,S,N,_), girone_contiene_squadra(G1,S1,N1,_), G == G1, uguale_nazione(S,S1).

% Scartare tutti gli answer set in cui la stessa squadra partecipa a due gironi (INTEGRITY CONSTRAINT).
:-girone_contiene_squadra(G,S,_,_),  girone_contiene_squadra(G1,S1,_,_), G != G1, S == S1.

% # # # PARTITE # # #

% In una giornata si disputano esattamente 2_2 partite per ogni girone.  ( NOTAZIONE: X_Y --> occorre almeno 'X' ed al massimo 'Y' volte ).
2 {partita_andata(Girone, Squadra1,Squadra2, 1): girone_contiene_squadra(Girone, Squadra1, _, _), girone_contiene_squadra(Girone, Squadra2, _, _ ), Squadra1 != Squadra2} 2 :- girone(Girone).
2 {partita_andata(Girone, Squadra1,Squadra2, 2): girone_contiene_squadra(Girone, Squadra1, _, _), girone_contiene_squadra(Girone, Squadra2, _, _ ), Squadra1 != Squadra2} 2 :- girone(Girone).
2 {partita_andata(Girone, Squadra1,Squadra2, 3): girone_contiene_squadra(Girone, Squadra1, _, _), girone_contiene_squadra(Girone, Squadra2, _, _ ), Squadra1 != Squadra2} 2 :- girone(Girone).

% La partita di ritorno viene definita come una partita di andata in cui le squadre giocano in casa della squadra che all'andata ha giocato in trasferta.
partita_ritorno(Girone, Squadra2,Squadra1, Giornata) :- partita_andata(Girone, Squadra1,Squadra2, Giornata).

% Scartare tutti gli answer set in cui si disputa la stessa partita di andata e ritorno nella stessa giornata.
:- partita_andata(Girone, Squadra1,Squadra2, N),  partita_ritorno(Girone, Squadra1, Squadra2, N).

% Scartare tutti gli answer set in cui, considerate due partite nella stessa giornata, una squadra le gioca entrambe. (INTEGRITY CONSTRAINT).
:-  partita_andata(Girone, Squadra1,Squadra2, N),  partita_andata(Girone, Squadra1,Squadra4, N), Squadra2 != Squadra4.
:-  partita_andata(Girone, Squadra1,Squadra2, N),  partita_andata(Girone, Squadra4,Squadra1, N), Squadra2 != Squadra4.
:-  partita_andata(Girone, Squadra1,Squadra2, N),  partita_andata(Girone, Squadra2,Squadra4, N), Squadra1 != Squadra4.
:-  partita_andata(Girone, Squadra1,Squadra2, N),  partita_andata(Girone, Squadra4,Squadra2, N), Squadra1 != Squadra4.
:-  partita_andata(Girone, Squadra1,Squadra2, N),  partita_andata(Girone, Squadra2,Squadra1, N).

% Scartare tutti gli answer set in cui la stessa partita viene disputata in più giornate.
:- partita_andata(Girone, Squadra1,Squadra2, N), partita_andata(Girone, Squadra1, Squadra2, N1), N != N1 .

% Scartare tutti gli answer set in cui una partita di andata viene disputata come una partita di ritorno in una differente giornata.
:- partita_andata(Girone, Squadra1,Squadra2, N), partita_andata(Girone, Squadra2, Squadra1, N1), N != N1.

% Scartare tutti gli answer set in cui due squadre della stessa citta giocano in casa nella stessa giornata.
:- partita_andata( _, Squadra1, _, N), partita_andata( _, Squadra2, _, N), uguale_citta(Squadra1, Squadra2).

% Una Squadra gioca in casa tre volte consecutivamente se appare come prima squadra all'interno della partita.
tre_consecutive_casa(Squadra) :-
    partita_andata(_, Squadra, _, 1),
    partita_andata(_, Squadra, _, 2),
    partita_andata(_, Squadra, _, 3).

% Una Squadra gioca in casa tre volte consecutivamente se appare come seconda squadra all'interno della partita.
tre_consecutive_trasferta(Squadra) :-
    partita_andata(_, _, Squadra, 1),
    partita_andata(_, _, Squadra, 2),
    partita_andata(_, _, Squadra, 3).

% Generalizzazione dei due concetti precedenti.
tre_consecutive_gen (Squadra) :-
    tre_consecutive_casa(Squadra),
    tre_consecutive_trasferta(Squadra).

% Scartare tutti gli answer set in cui una squadra gioca più di due partite consecutivamente in casa o in trasferta.
:- tre_consecutive_gen(Squadra).

#show partita_andata/4.
#show partita_ritorno/4.
