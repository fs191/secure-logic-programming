% TODO this program is not parsed by PrivaLog yet
:-type(ship(@shipname:primary public string,
            %we do not have a float type yet
            @latitude:private float,
            @longitude:private float,
            @speed:public int,
            @cargotype:private string,
            @cargoamount:private int)).
% the "primary key" constraint gives us a hint that we e.g. can substitute
%"ship(Ship,X,Y,_,_,_)" and "ship(Ship,X,Y,Speed,_,_)" with "ship(Ship,X,Y,Speed,_,_)"
% we will do this merge step e.g. after "postProcess" and before "typeInference"

:-type(port(@name:primary public string,
            @port_latitude:public float,
            @port_longitude:public float,
            @offloadcapacity:public int,
            @available:private bool)).

% the placeholder "_" denotes an unnamed variable
% we need use a special data construction for this
ship_location(Ship,X,Y) :-
    ship(Ship,X,Y,_,_,_).

ship_speed(Ship,Speed) :-
    ship(Ship,_,_,Speed,_,_).

ship_cargotype(Ship,CargoType) :-
    ship(Ship,_,_,_,CargoType,_).

ship_cargoamount(Ship,CargoAmount) :-
    ship(Ship,_,_,_,_,CargoAmount).

port_location(Port,X,Y) :-
    port(Port,X,Y,_,_).

port_offloadcapacity(Port,Capacity) :-
    port(Port,_,_,Capacity,_).

port_available(Port) :-
    port(Port,_,_,_,true).

reachability_time(Ship,Port,Time) :-
    ship_location(Ship,X1,Y1),
    ship_speed(Ship,Speed),
    port_location(Port,X2,Y2),
    %we do not have the "sqrt" and "^" operations yet
    %also, we want to distinguish float division "X / Y" and integer division "div(X,Y)"
    %ideally, the output of "/" and "sqrt" should obtain type "float", even if the inputs are of type "int"
    %  "is"   is the evaluation of RHS, or comparison if both are already evaluated (translated to Is construction)
    Time is sqrt((X1 - X2)^2 + (Y1 - Y2)^2) / Speed.

feasible_port(Ship,Port) :-
    port_available(Port),
    port_offloadcapacity(Port,Capacity),
    ship_cargoamount(Ship,Cargo),
    Capacity >= Cargo.

suitable_ship(Ship,WantedCargo) :-
   ship_cargotype(Ship, ShipCargo),
   ship_cargoamount(Ship, ShipCargoAmount),
   %  "\+" is negation. let us only allow to apply negations "to ground terms which do not contain intensional predicates"
   %  "=:=" is a comparison of two ground arithmetic terms (translated to Eq construction)
   ShipCargoAmount > 0,
   %let us asume that special cargo type "all" contains everything that may ever be needed
   %  "="   is the unification (translated to Un construction)
   %  ";"   denotes an OR operation  (translated to Or construction)
   %        let us only allow to apply OR "to ground terms which do not contain intensional predicates"
   (WantedCargo = ShipCargo ; ShipCargo = all).

arrival(Ship,Port,CargoType,Time) :-
    suitable_ship(Ship,CargoType),
    feasible_port(Ship,Port),
    reachability_time(Ship,Port,Time).

%not sure if it is a good idea to use repeating name "@cargotype" in the table and for the inputs
:-inputs([@portname_in:private string, @cargotype_in:private string]).
:-outputs([MinTime]).
?-min(arrival(Ship,@portname_in,@cargotype_in,Time),Time,MinTime).

