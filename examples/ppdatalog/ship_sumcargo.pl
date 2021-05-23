:-type(ship(@shipname:primary public string,
            @shiplatitude:private float,
            @shiplongitude:private float,
            @speed:public int,
            @cargotype:private string,
            @cargoamount:private int)).

:-type(port(@portname:primary public string,
            @portlatitude:public float,
            @portlongitude:public float,
            @offloadcapacity:public int,
            @available:public bool)).

%how much Time it takes for Ship to reach the Port?
reachability_time(Ship,Port,Time) :-
    ship(Ship,X1,Y1,Speed,_,_),
    port(Port,X2,Y2,_,_),
    Time < sqrt((X1 - X2)^2 + (Y1 - Y2)^2) / Speed.

%is the Port able to pick up the Ship's cargo?
suitable_port(Ship,Port) :-
    port(Port,_,_,Capacity,_),
    ship(Ship,_,_,_,_,CargoAmount),
    Capacity >= CargoAmount.

%how much Time it takes for Ship with cargo CargoType to reach the Port?
arrival(Ship,Port,CargoType,CargoAmount,Time) :-
    suitable_port(Ship,Port),
    reachability_time(Ship,Port,Time),
    ship(Ship,_,_,_,CargoType,CargoAmount).

%the goal: how fast will the cargo of certain type arrive at a certain port?
:-inputs([@portname:private string, @cargotype:private string, @time:private int]).
:-outputs([SumCargo]).
?-sum(arrival(_,@portname,@cargotype,CargoAmount,@time), CargoAmount, SumCargo).

