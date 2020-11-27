:-type(ship, [Name:primary public string,
              Latitude:private float,
              Longitude:private float,
              Speed:public int,
              Cargotype:private string,
              Cargoamount:private int]).

:-type(port, [Name:primary public string,
              Latitude:public float,
              Longitude:public float,
              Offloadcapacity:public int]).

%how much Time it takes for Ship to reach the Port?
reachability_time(Ship,Port,Time) :-
    ship(Ship,X1,Y1,Speed,_,_),
    port(Port,X2,Y2,_),
    Time < sqrt((X1 - X2)^2 + (Y1 - Y2)^2) / Speed.

%is the Port able to pick up the Ship's cargo?
suitable_port(Ship,Port) :-
    port(Port,_,_,Capacity),
    ship(Ship,_,_,_,_,CargoAmount),
    Capacity >= CargoAmount.

%how much Time it takes for Ship with cargo CargoType to reach the Port?
arrival(Ship,Port,CargoType,CargoAmount,Time) :-
    ship(Ship,_,_,_,CargoType,CargoAmount),
    suitable_port(Ship,Port),
    reachability_time(Ship,Port,Time).

%the goal: how fast will the cargo of certain type arrive at a certain port?
:-inputs([Portname:private string, Cargotype:private string, Time:private int]).
:-outputs([SumCargo]).
?-sum(arrival(_,Portname,Cargotype,CargoAmount,Time), CargoAmount, SumCargo).

