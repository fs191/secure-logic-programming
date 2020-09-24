import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



void main ()
{
    pd_shared3p xor_uint8 [[1]] portname = argument("portname");
    pd_shared3p xor_uint8 [[1]] cargotype = argument("cargotype");
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8>  arg1 = constColumn(portname);
    public relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8>  arg2 = constColumn(cargotype);
    public in_arrival_fbbf< public
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  args1;
    args1.b = trueColumn();
    args1.arg1 = arg1;
    args1.arg2 = arg2;
    public out_arrival_fbbf< public
    ,relColumn< D0,T0,S0> 
    ,relColumn< D3,T3,S3> >  res1 = getTable_arrival_fbbf(ds, args1);
    public out_arrival_fbbf< pd_shared3p
    ,relColumn< pd_shared3p,T0,S0> 
    ,relColumn< pd_shared3p
    ,T3
    ,S3> >  resUnique1 = deduplicate_arrival_fbbf(res1);
    public relColumn< pd_shared3p,T0,S0>  __HOLE_23 = resUnique1.arg0;
    public relColumn< pd_shared3p,T3,S3>  Time = resUnique1.arg3;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    public uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "Time", filterTrue(pi, n, Time));
}