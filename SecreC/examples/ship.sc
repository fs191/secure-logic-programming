import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



struct out_ship
{
    public bool [[1]] b;
    public relColumn< public,uint32,uint8>  arg0;
    public relColumn< pd_shared3p,T1,S1>  arg1;
    public relColumn< pd_shared3p,T2,S2>  arg2;
    public relColumn< public,int32,int32>  arg3;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg4;
    public relColumn< pd_shared3p,int32,int32>  arg5;
}

struct out_port
{
    public bool [[1]] b;
    public relColumn< public,uint32,uint8>  arg0;
    public relColumn< public,T1,S1>  arg1;
    public relColumn< public,T2,S2>  arg2;
    public relColumn< public,int32,int32>  arg3;
    public relColumn< pd_shared3p,bool,bool>  arg4;
}

out_ship getTable_ship ( public string ds
, public uint m
, public uint mi
, public uint ni )
{
    public out_ship result;
    result.b = trueColumn(m);
    result.arg0 = getDBColumn(ds, "ship", 0, m, mi, ni);
    result.arg1 = getDBColumn(ds, "ship", 1, m, mi, ni);
    result.arg2 = getDBColumn(ds, "ship", 2, m, mi, ni);
    result.arg3 = getDBColumn(ds, "ship", 3, m, mi, ni);
    result.arg4 = getDBColumn(ds, "ship", 4, m, mi, ni);
    result.arg5 = getDBColumn(ds, "ship", 5, m, mi, ni);
    return result;
}

out_port getTable_port ( public string ds
, public uint m
, public uint mi
, public uint ni )
{
    public out_port result;
    result.b = trueColumn(m);
    result.arg0 = getDBColumn(ds, "port", 0, m, mi, ni);
    result.arg1 = getDBColumn(ds, "port", 1, m, mi, ni);
    result.arg2 = getDBColumn(ds, "port", 2, m, mi, ni);
    result.arg3 = getDBColumn(ds, "port", 3, m, mi, ni);
    result.arg4 = getDBColumn(ds, "port", 4, m, mi, ni);
    return result;
}

void main ()
{
    pd_shared3p xor_uint8 [[1]] portname = argument("portname");
    pd_shared3p xor_uint8 [[1]] cargotype = argument("cargotype");
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8>  arg0 = constColumn(portname);
    public relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8>  arg1 = constColumn(cargotype);
    public in_fastestDelivery_bbf< public
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  args1;
    args1.b = trueColumn();
    args1.arg0 = arg0;
    args1.arg1 = arg1;
    public out_fastestDelivery_bbf< public
    ,relColumn< D2,T2,S2> >  res1 = getTable_fastestDelivery_bbf(ds, args1);
    public out_fastestDelivery_bbf< pd_shared3p
    ,relColumn< pd_shared3p
    ,T2
    ,S2> >  resUnique1 = deduplicate_fastestDelivery_bbf(res1);
    public relColumn< pd_shared3p,T2,S2>  MinTime = resUnique1.arg2;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    public uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "MinTime", filterTrue(pi, n, MinTime));
}