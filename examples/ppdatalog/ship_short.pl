% TODO this program is not parsed by PrivaLog yet
:-type(ship, [@name:public string,
              @latitude:private float,
              @longitude:private float,
              @speed:public int,
              @cargotype:private string,
              @cargoamount:private int]).
:-pk(ship,[@name]).

:-type(port, [@name:public string,
              @latitude:public float,
              @longitude:public float,
              @offloadcapacity:public int]).
:-pk(port,[@name]).

%how much Time it takes for Ship to reach the Port?
reachability_time(Ship,Port,Time) :-
    ship(Ship,X1,Y1,Speed,_,_),
    port(Port,X2,Y2,_,_),
    Time is sqrt((X1 - X2)^2 + (Y1 - Y2)^2) / Speed.

%is the Port able to pick up the Ship's cargo?
feasible_port(Ship,Port) :-
    port(Port,_,_,Capacity),
    ship(Ship,_,_,_,_,CargoAmount),
    Capacity >= CargoAmount.

%how much Time it takes for Ship with cargo CargoType to reach the Port?
arrival(Ship,Port,CargoType,Time) :-
    ship(Ship,_,_,_,CargoType,_),
    feasible_port(Ship,Port),
    reachability_time(Ship,Port,Time).

%how fast will the cargo of type CargoType arrive at the port Port?
fastestDelivery(Port, CargoType, MinTime) :-
    min(arrival(Ship,Port,CargoType,Time),Time,MinTime).

%the goal: how fast will the cargo of certain type arrive at a certain port?
:-inputs([@portname:private string, @cargotype:private string]).
:-outputs([MinTime]).
fastestDelivery(@portname,@cargotype,MinTime)?
