:-type(ship(Name : public string,
            Latitude : private float,
            Longitude : private float,
            Speed : public int,
            Cargotype : private string,
            Cargoamount : private int)).

:-type(port(Name : public string,
            Latitude : public float,
            Longitude : public float,
            Offloadcapacity : public int)).

:-inputs([Portname : private string, Cargotype : private string]).

:-outputs([Time : private float]).


arrival_fbbf( X_0 : public string
            , X_1 : private string
            , X_2 : private string
            , X_3 : private float ) : private bool :-
   ship( X_0 : public string
   , X_5 : private float
   , X_6 : private float
   , X_7 : public int
   , X_2 : private string
   , X_9 : private int ) : private bool,
   port( X_1 : private string
   , X_11 : public float
   , X_12 : public float
   , X_13 : public int ) : private bool,
   ship( X_0 : public string
   , X_15 : private float
   , X_16 : private float
   , X_17 : public int
   , X_18 : private string
   , X_19 : private int ) : public bool,
   X_13 : public int >= X_19 : private int : private bool,
   ship( X_0 : public string
   , X_23 : private float
   , X_24 : private float
   , X_25 : public int
   , X_26 : private string
   , X_27 : private int ) : public bool,
   port( X_1 : private string
   , X_29 : public float
   , X_30 : public float
   , X_31 : public int) : private bool,
   (A:private float) is ((X_23:private float - X_29:public float):private float)^2:public int:private float: public bool,
   (B:private float) is ((X_24:private float - X_30:public float):private float)^2:public int:private float: public bool,
   (S:private float is sqrt(A:private float + B:private float:private float) : private float):public bool,
   (X_3:private float is S:private float/X_25:public int:private float):public bool.


?- arrival_fbbf( __HOLE_14 : public string
, Portname : private string
, Cargotype : private string
, Time : private float ) : private bool.
