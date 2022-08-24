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

%PUNTO 2

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

% tipoConcierto(gigante(CantidadCancionesMinimas,TiempoTotalMinimo)).
% tipoConcierto(mediano(TiempoTotalMaximo)).
% tipoConcierto(pequeño(TiempoMinimoDeUnaCancion)).

concierto(mikuExpo,estadosUnidos,2000,tipoConcierto(gigante(2,6))).
concierto(magicalMirai,japon,3000,tipoConcierto(gigante(3,10))).
concierto(vocalektVisions,estadosUnidos,1000,tipoConcierto(mediano(9))).
concierto(mikuFest,argentina,100,tipoConcierto(pequenio(4))).