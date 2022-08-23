% PUNTO 1

profesor(dodain).
profesor(lucas).
profesor(juanC).
profesor(juanFdS).
profesor(leoC).
profesor(martu).
profesor(vale).

dia(lunes).
dia(martes).
dia(miercoles).
dia(jueves).
dia(viernes).
dia(sabado).
dia(domingo).

atiende(dodain,lunes,9,15).
atiende(dodain,miercoles,9,15).
atiende(dodain,viernes,9,15).
atiende(lucas,martes,10,20).
atiende(juanC,sabado,18,22).
atiende(juanC,domingo,18,22).
atiende(juanFdS,jueves,10,20).
atiende(juanFdS,viernes,12,20).
atiende(leoC,lunes,14,18).
atiende(leoC,miercoles,14,18).
atiende(martu,miercoles,23,24).

atiende(vale,Dia,Desde,Hasta):-
    atiende(dodain,Dia,Desde,Hasta).
atiende(vale,Dia,Desde,Hasta):-
    atiende(juanC,Dia,Desde,Hasta).


% para "nadie hace el mismo horario que leoC" no es necesario incorporar ninguna clàusula
% ya que  por principio de Universo Cerrado: todo lo que no este en la base de datos es Falso
% y si algo no es verdadero entonces no lo incorporo.
    
% por el mismo motivo, "maiu está pensando si hace el horario de 0 a 8 los martes y miércoles" 
% tampoco hay que incorporarlo ya que como lo està pensando, entonces todavìa no es un hecho y es falso
% por lo que no se debe incorporar a la base de datos

% PUNTO 2

quienATiende(Profesor,Dia,Hora):-
    atiende(Profesor,Dia,Desde,Hasta),
    between(Desde,Hasta,Hora).

% PUNTO 3

foreverAlone(Profesor,Dia,Hora):-
    quienATiende(Profesor,Dia,Hora),
    not(   (   quienATiende(Profesor2,Dia,Hora), Profesor \= Profesor2)).
    
% PUNTO 4

posibilidadesAtencion(Dia, Personas):-
  findall(Persona, distinct(Persona, quienAtiende(Persona, Dia, _)), PersonasPosibles),
  combinar(PersonasPosibles, Personas).

combinar([], []).
combinar([Persona|PersonasPosibles], [Persona|Personas]):-combinar(PersonasPosibles, Personas).
combinar([_|PersonasPosibles], Personas):-combinar(PersonasPosibles, Personas).

% PUNTO 5

ventas(dodain,fecha(10,8),[golosinas(1200),cigarrillos([jockey]),golosinas(50)]).

ventas(dodain,fecha(12,8),[bebidas(alcoholicas,8),bebidas(noAlcoholicas,1),golosinas(10)]).
    
ventas(martu,fecha(12,8),[golosinas(1000),cigarrillos([chesterfield, colorado , parisiennes])]).
         
ventas(lucas,fecha(11,8),[golosinas(600)]).

ventas(lucas,fecha(18,8),[bebidas(noAlcoholicas,2),cigarrillos([derby])]).

esImportante(golosinas(Dinero)):-
    Dinero>100.
    
esImportante(cigarrillos(Marcas)):-
    length(Marcas,CantidadMarcas),
    CantidadMarcas>2.
    
esImportante(bebidas(Tipo,_)):-
    Tipo==alcoholicas.
    
esImportante(bebidas(_,Cantidad)):-
    Cantidad>5.

vendedor(Profesor):-
    ventas(Profesor,_,_).

esSuertudo(Profesor):-
    vendedor(Profesor),
	forall(ventas(Profesor,_,[Venta|_]),esImportante(Venta)).