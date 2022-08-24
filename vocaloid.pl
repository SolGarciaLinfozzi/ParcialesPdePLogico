%VOCALOID

% PUNTO 1

vocaloid(megurineLuka).
vocaloid(hatsuneMiku).
vocaloid(gumi).
vocaloid(seeU).
vocaloid(kaito).

% cancion(nightFever).
% cancion(foreverYoung).
% cancion(tellYourWorld).
% cancion(novemberRain).


sabeCantar(megurineLuka,cancion(nightFever,4)).
sabeCantar(megurineLuka,cancion(foreverYoung,5)).
sabeCantar(hatsuneMiku,cancion(tellYourWorld,4)).
sabeCantar(gumi,cancion(foreverYoung,5)).
sabeCantar(gumi,cancion(tellYourWorld,5)).
sabeCantar(seeU,cancion(novemberRain,6)).
sabeCantar(seeU,cancion(nightFever,5)).

% PUNTO 2

esNovedoso(Vocaloid):-
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    TiempoTotal<15,
    sabeAlMenos2Canciones(Vocaloid).

sabeAlMenos2Canciones(Vocaloid):-
    sabeCantar(Vocaloid,Cancion1),
    sabeCantar(Vocaloid,Cancion2),
    Cancion1 \= Cancion2.

tiempoTotalCanciones(Vocaloid,TiempoTotal):-
    vocaloid(Vocaloid),
    findall(Tiempo,sabeCantar(Vocaloid,cancion(_,Tiempo)),Tiempos),
    sumlist(Tiempos, TiempoTotal).

% PUNTO 3
    
% NO existe una sola cancion que cante que NO dure 4 minutos o menos
% esAcelerado(Vocaloid):-
%     cantante(Vocaloid),
%     not((sabeCantar(Vocaloid,cancion(_,Tiempo)), not(Tiempo=<4))).

cantante(Vocaloid):-
    sabeCantar(Vocaloid,_).


%”No canta una canción que dure más de 4 minutos” (not/1)
esAcelerado(Vocaloid):-
    cantante(Vocaloid),
    not((sabeCantar(Vocaloid,cancion(_,Tiempo)),Tiempo > 4)).

% PUNTO 4

% conciertoGigante(CantidadCancionesMinimas,TiempoTotalMinimo)).
% conciertoMediano(TiempoTotalMaximo)).
% conciertoPequenio(TiempoMinimoDeUnaCancion)).

concierto(mikuExpo,estadosUnidos,2000,conciertoGigante(2,6)).
concierto(magicalMirai,japon,3000,conciertoGigante(3,10)).
concierto(vocalektVisions,estadosUnidos,1000,conciertoMediano(9)).
concierto(mikuFest,argentina,100,conciertoPequenio(4)).

% PUNTO 5

puedeParticiparConcierto(Vocaloid,Concierto):-
    cantante(Vocaloid),
    concierto(Concierto,_,_,TipoConcierto),
    cumpleRequisitosConcierto(Vocaloid,TipoConcierto).

sabeMasDeNCanciones(Vocaloid,Cantidad) :-
    findall(Cancion,sabeCantar(Vocaloid,Cancion), Canciones),
    length(Canciones,CantidadDeCanciones),
    CantidadDeCanciones > Cantidad.

cumpleRequisitosConcierto(Vocaloid, conciertoGigante(CantidadCancionesMinimas,TiempoTotalMinimo)):-
    sabeMasDeNCanciones(Vocaloid,CantidadCancionesMinimas),
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    TiempoTotal > TiempoTotalMinimo.


cumpleRequisitosConcierto(Vocaloid, conciertoMediano(TiempoTotalMaximo)):-
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    TiempoTotal < TiempoTotalMaximo.


cumpleRequisitosConcierto(Vocaloid, conciertoPequenio(TiempoMinimoDeUnaCancion)):-
    sabeCantar(Vocaloid, cancion(_,Tiempo)),
    Tiempo>TiempoMinimoDeUnaCancion.


%   sabeCantar(seeU,cancion(nightFever,5)).

puedeParticiparConcierto(hatsuneMiku,Concierto):-
    concierto(Concierto,_,_,_).

% fama(mergurineLuka,0).
% fama(hatsuneMiku,0).
% fama(gumi,0).
% fama(seeU,0).
% fama(kaito,0).

% famaTotal(Vocaloid,Fama) :-
%     vocaloid(Vocaloid),
%     fama(Vocaloid,0),
%     puedeParticiparConcierto(Vocaloid,Concierto),
%     concierto(Concierto,_,FamaConcierto,_),
%     Fama is Fama + FamaConcierto.


famaTotal(Vocaloid,FamaTotal) :-
    vocaloid(Vocaloid),
    findall(FamaConcierto,(puedeParticiparConcierto(Vocaloid,Concierto),concierto(Concierto,_,FamaConcierto,_)),FamaTotal),
    sumlist(FamasConciertos,FamaTotal).

% fama(Vocaloid,FamaTotal):-
%     vocaloid(Vocaloid),
%     findall(Fama,(puedeParticiparConcierto(Vocaloid,Concierto),concierto(Concierto,_,Fama,_)),Famas),
%     sumlist(Famas,FamaTotal).

vocaloidMasFamoso(Vocaloid) :-
    vocaloid(Vocaloid),
    famaTotal(Vocaloid,FamaTotal),
    not((vocaloid(Vocaloid2),famaTotal(Vocaloid2,FamaTotal2),FamaTotal2>FamaTotal)).


masFamoso(VocaloidMasFamoso):-
    famaTotal(VocaloidMasFamoso,FamaTotalFamoso),
    forall( famaTotal(_,FamaTotal), FamaTotal =< FamaTotalFamoso).

masFamoso2(VocaloidMasFamoso):-
    famaTotal(VocaloidMasFamoso,FamaTotalFamoso),
    not( (famaTotal(_,FamaTotal) , not(FamaTotal =< FamaTotalFamoso))).

masFamoso3(VocaloidMasFamoso):-
    famaTotal(VocaloidMasFamoso,FamaTotalFamoso),
    not((famaTotal(_,FamaTotal),FamaTotal >= FamaTotalFamoso)).

% encargadoDe(Encargado,Plato, Restaurante) :-
%     plato(Plato,_),
%     trabajaEn(Encargado,Restaurante),
%     sabeCocinar(Encargado,Plato,ExperienciaEncargado),
%     trabajaEn(Empleado,Restaurante),
%     sabeCocinar(Empleado, Plato,Experiencia),
%     not(Experiencia>ExperienciaEncargado).

conoceA(megurineLuka, hatsuneMiku).
conoceA(megurineLuka, gumi).
conoceA(gumi, seeU).
conoceA(seeU, kaito).

unicoParticipanteEntreConocidos(Cantante,Concierto):- 
    puedeParticipar(Cantante, Concierto),
	not((conocido(Cantante, OtroCantante), 
    puedeParticipar(OtroCantante, Concierto))).

%Conocido directo
conocido(Cantante, OtroCantante) :- 
conoce(Cantante, OtroCantante).

%Conocido indirecto
conocido(Cantante, OtroCantante) :- 
conoce(Cantante, UnCantante), 
conocido(UnCantante, OtroCantante).

