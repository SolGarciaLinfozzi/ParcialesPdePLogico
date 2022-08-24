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

puedeParticiparConcierto(hatsuneMiku,Concierto):-
    concierto(Concierto,_,_,_).

puedeParticiparConcierto(Vocaloid,Concierto):-
    cantante(Vocaloid),
    concierto(Concierto,_,_,TipoConcierto),
    cumpleRequisitosConcierto(Vocaloid,TipoConcierto).

cumpleRequisitosConcierto(Vocaloid, conciertoGigante(CantidadCancionesMinimas,TiempoTotalMinimo)):-
    sabeNCanciones(Vocaloid,CantidadCanciones),
    CantidadCanciones>CantidadCancionesMinimas,
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    TiempoTotal>TiempoTotalMinimo.

cumpleRequisitosConcierto(Vocaloid, conciertoMediano(TiempoTotalMaximo)):-
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    TiempoTotal<TiempoTotalMaximo.

cumpleRequisitosConcierto(Vocaloid, conciertoPequenio(TiempoMinimoDeUnaCancion)):-
    sabeCantar(Vocaloid, cancion(_,Tiempo)),
    Tiempo>TiempoMinimoDeUnaCancion.

sabeNCanciones(Vocaloid,CantidadCanciones):-
    cantante(Vocaloid),
    findall(Cancion,sabeCantar(Vocaloid,cancion(Cancion,_)),Canciones),
    length(Canciones,CantidadCanciones).


% %   sabeCantar(seeU,cancion(nightFever,5)).


% Conocer el vocaloid más famoso, es decir con mayor nivel de fama.

% El nivel de fama de un vocaloid se calcula como la fama total que le dan los conciertos en los cuales puede participar 

% multiplicado por la cantidad de canciones que sabe cantar.

%concierto(mikuFest,argentina,100,conciertoPequenio(4)).

fama(Vocaloid,FamaTotal):-
    vocaloid(Vocaloid),
    findall(Fama,(puedeParticiparConcierto(Vocaloid,Concierto),concierto(Concierto,_,Fama,_)),Famas),
    sumlist(Famas,FamaTotal).

% masFamoso(Vocaloid):-
%     vocaloid(Vocaloid),
%     forall(fama(Vocaloid,FamaTotal),).

% No existe fama de un cantante tal que esa fama no sea la mayor

masFamoso(VocaloidMasFamoso):-
    fama(VocaloidMasFamoso,FamaTotalFamoso),
    forall( fama(_,FamaTotal), FamaTotal =< FamaTotalFamoso).


% masFamoso(VocaloidMasFamoso):-
%     fama(VocaloidMasFamoso,FamaTotalFamoso),
%     not( (fama(_,FamaTotal) , not(FamaTotal =< FamaTotalFamoso))).

% masFamoso(VocaloidMasFamoso):-
%     fama(VocaloidMasFamoso,FamaTotalFamoso),
%     not((fama(_,FamaTotal),FamaTotal >= FamaTotalFamoso)).

% vocaloidMasFamoso(Vocaloid) :-
%     vocaloid(Vocaloid),
%     famaTotal(Vocaloid,FamaTotal),
%     not((vocaloid(Vocaloid2),famaTotal(Vocaloid2,FamaTotal2),FamaTotal2>FamaTotal)).


%PUNTO 6

% queremos verificar si un vocaloid es el único que participa de un concierto, esto se cumple si 

% ninguno de sus conocidos ya sea directo o indirectos (en cualquiera de los niveles) participa en el mismo concierto.

conoce(megurineLuka,hatsuneMiku).
conoce(megurineLuka,gumi).
conoce(gumi,seeU).
conoce(seeU,kaito).

participaSoloEnConcierto(Vocaloid):-
    puedeParticiparConcierto(Vocaloid,Concierto),
    forall(conocido(Vocaloid,Vocaloid2),not(puedeParticiparConcierto(Vocaloid2,Concierto))).

conocidoIndirecto(Vocaloid,Vocaloid3):-
    conoce(Vocaloid,Vocaloid2),
    conoce(Vocaloid2,Vocaloid3).

% tieneConocidoIndirecto(Vocaloid,Vocaloid2):-
%     conoce(Vocaloid,Vocaloid2),
%     conoce(Vocaloid2,Vocaloid3).

conocido(Vocaloid,Vocaloid2):-
    conocidoIndirecto(Vocaloid,Vocaloid2).

 conocido(Vocaloid,Vocaloid2):-
     conoce(Vocaloid,Vocaloid2).


% Vocaloid = hatsuneMiku,
% Concierto = mikuExpo ;

% Vocaloid = hatsuneMiku,
% Concierto = magicalMirai ;

% Vocaloid = hatsuneMiku,
% Concierto = vocalektVisions ;

% Vocaloid = hatsuneMiku,
% Concierto = mikuFest ;

% Vocaloid = megurineLuka,
% Concierto = mikuFest ;

% Vocaloid = gumi,
% Concierto = mikuFest ;

% Vocaloid = seeU,
% Concierto = mikuFest ;


% -----------
% Vocaloid = hatsuneMiku ;

% Vocaloid = seeU ;
