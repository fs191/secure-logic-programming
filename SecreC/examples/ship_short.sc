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
    return result;
}

template< domain D,type T1,type T2> 
struct in_arrival_fbbf
{
    D bool [[1]] b;
    public T1 arg1;
    public T2 arg2;
}

template< domain D,type T0,type T3> 
struct out_arrival_fbbf
{
    D bool [[1]] b;
    public T0 arg0;
    public T3 arg3;
}

template< type T0,type T1> 
T0 extend_arrival_fbbf ( public T1 result
, public uint m
, public uint mi
, public uint ni )
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg1 = extendColumn(result.arg1, m, mi, ni);
    result.arg2 = extendColumn(result.arg2, m, mi, ni);
    return result;
}

out_arrival_fbbf< pd_shared3p
,relColumn< public,uint32,uint8> 
,relColumn< D3,int32,int32> >  goal_arrival_fbbf_0 ( public string ds
, public in_arrival_fbbf< public
,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m1 = tdbGetRowCount(ds, "ship");
    public uint m2 = tdbGetRowCount(ds, "port");
    public uint m3 = tdbGetRowCount(ds, "ship");
    public uint m5 = tdbGetRowCount(ds, "ship");
    public uint m6 = tdbGetRowCount(ds, "port");
    public uint m = m0* m1* m2* m3* m5* m6;
    public uint n0 = (m) / (m0);
    public uint n1 = (m) / (m0* m1);
    public uint n2 = (m) / (m0* m1* m2);
    public uint n3 = (m) / (m0* m1* m2* m3);
    public uint n5 = (m) / (m0* m1* m2* m3* m5);
    public uint n6 = (m) / (m0* m1* m2* m3* m5* m6);
    
    //extend the initial args to appropriate size
    public in_arrival_fbbf< public
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8> >  table0 = extend_arrival_fbbf(args, m, m0, n0);
    
    //evaluate all underlying predicates
    public out_ship table1 = getTable_ship(ds, m, m1, n1);
    public out_port table2 = getTable_port(ds, m, m2, n2);
    public out_ship table3 = getTable_ship(ds, m, m3, n3);
    public out_ship table5 = getTable_ship(ds, m, m5, n5);
    public out_port table6 = getTable_port(ds, m, m6, n6);
    
    //assign input variables
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_1 = table0.arg1;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_2 = table0.arg2;
    
    //evaluate the clause body
    
    //q1
    
    //q1000
    public relColumn< public,uint32,uint8>  X_0 = table1.arg0;
    public bool [[1]] b1000 = constColumn(true, m);
    
    //q1001
    public relColumn< pd_shared3p,T,S>  X_5 = table1.arg1;
    public bool [[1]] b1001 = constColumn(true, m);
    
    //q1002
    public relColumn< pd_shared3p,T,S>  X_6 = table1.arg2;
    public bool [[1]] b1002 = constColumn(true, m);
    
    //q1003
    public relColumn< public,int32,int32>  X_7 = table1.arg3;
    public bool [[1]] b1003 = constColumn(true, m);
    
    //q1004
    pd_shared3p bool [[1]] b1004 = bop("==", X_2, table1.arg4);
    
    //q1005
    public relColumn< pd_shared3p,int32,int32>  X_9 = table1.arg5;
    public bool [[1]] b1005 = constColumn(true, m);
    pd_shared3p bool [[1]] b1 = b1000& b1001& b1002& b1003& b1004& b1005;
    
    //q2
    
    //q2000
    public bool [[1]] b2000 = bop("==", X_1, table2.arg0);
    
    //q2001
    public relColumn< public,T,S>  X_11 = table2.arg1;
    public bool [[1]] b2001 = constColumn(true, m);
    
    //q2002
    public relColumn< public,T,S>  X_12 = table2.arg2;
    public bool [[1]] b2002 = constColumn(true, m);
    
    //q2003
    public relColumn< public,int32,int32>  X_13 = table2.arg3;
    public bool [[1]] b2003 = constColumn(true, m);
    public bool [[1]] b2 = b2000& b2001& b2002& b2003;
    
    //q3
    
    //q3000
    public bool [[1]] b3000 = bop("==", X_0, table3.arg0);
    
    //q3001
    public relColumn< pd_shared3p,T,S>  X_15 = table3.arg1;
    public bool [[1]] b3001 = constColumn(true, m);
    
    //q3002
    public relColumn< pd_shared3p,T,S>  X_16 = table3.arg2;
    public bool [[1]] b3002 = constColumn(true, m);
    
    //q3003
    public relColumn< public,int32,int32>  X_17 = table3.arg3;
    public bool [[1]] b3003 = constColumn(true, m);
    
    //q3004
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_18 = table3.arg4;
    public bool [[1]] b3004 = constColumn(true, m);
    
    //q3005
    public relColumn< pd_shared3p,int32,int32>  X_19 = table3.arg5;
    public bool [[1]] b3005 = constColumn(true, m);
    public bool [[1]] b3 = b3000& b3001& b3002& b3003& b3004& b3005;
    
    //q4
    pd_shared3p bool [[1]] b4 = bop(">=", X_13, X_19);
    
    //q5
    
    //q5000
    public bool [[1]] b5000 = bop("==", X_0, table5.arg0);
    
    //q5001
    public relColumn< pd_shared3p,T,S>  X_23 = table5.arg1;
    public bool [[1]] b5001 = constColumn(true, m);
    
    //q5002
    public relColumn< pd_shared3p,T,S>  X_24 = table5.arg2;
    public bool [[1]] b5002 = constColumn(true, m);
    
    //q5003
    public relColumn< public,int32,int32>  X_25 = table5.arg3;
    public bool [[1]] b5003 = constColumn(true, m);
    
    //q5004
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_26 = table5.arg4;
    public bool [[1]] b5004 = constColumn(true, m);
    
    //q5005
    public relColumn< pd_shared3p,int32,int32>  X_27 = table5.arg5;
    public bool [[1]] b5005 = constColumn(true, m);
    public bool [[1]] b5 = b5000& b5001& b5002& b5003& b5004& b5005;
    
    //q6
    
    //q6000
    D bool [[1]] b6000 = bop("==", X_1, table6.arg0);
    
    //q6001
    public relColumn< public,T,S>  X_29 = table6.arg1;
    public bool [[1]] b6001 = constColumn(true, m);
    
    //q6002
    public relColumn< public,T,S>  X_30 = table6.arg2;
    public bool [[1]] b6002 = constColumn(true, m);
    
    //q6003
    public relColumn< public,int32,int32>  X_31 = table6.arg3;
    public bool [[1]] b6003 = constColumn(true, m);
    
    //q6004
    public relColumn< D,T,S>  X_32 = table6.arg4;
    public bool [[1]] b6004 = constColumn(true, m);
    D bool [[1]] b6 = b6000& b6001& b6002& b6003& b6004;
    
    //q7
    public relColumn< D,int32,int32>  X_3 = aop( "/"
    , aop( "sqrt"
    , aop( "+"
    , aop("pow", (float32)aop("-", X_23, X_29), (float32)constColumn(2, m))
    , aop("pow", (float32)aop("-", X_24, X_30), (float32)constColumn(2, m)) ) )
    , X_25 );
    public bool [[1]] b7 = constColumn(true, m);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3& b4& b5& b6& b7);
    public out_arrival_fbbf< pd_shared3p
    ,relColumn< public,uint32,uint8> 
    ,relColumn< D3,int32,int32> >  result;
    result.b = b;
    result.arg0 = X_0;
    result.arg3 = X_3;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_arrival_fbbf (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg0 = myCat(t1.arg0, t2.arg0);
    t0.arg3 = myCat(t1.arg3, t2.arg3);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_arrival_fbbf (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg0 = applyPermutation(t.arg0, pi);
    result.arg3 = applyPermutation(t.arg3, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_arrival_fbbf (public string ds, public T1 args)
{
    public T0 result;
    public out_arrival_fbbf< pd_shared3p
    ,relColumn< public,uint32,uint8> 
    ,relColumn< D,int32,int32> >  result0 = goal_arrival_fbbf_0(ds, args);
    result = cat_arrival_fbbf(result, result0);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_arrival_fbbf (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg0 = copyColumn(t.arg0);
    result.arg3 = copyColumn(t.arg3);
    pi = countSortPermutation(result.b);
    result = permute_arrival_fbbf(result, pi);
    pi = quickSortPermutation(result.arg0);
    result = permute_arrival_fbbf(result, pi);
    pi = quickSortPermutation(result.arg3);
    result = permute_arrival_fbbf(result, pi);
    result.b = (result.b) & (!((findRepeating( result.arg3 )) & (findRepeating( result.arg0 ))));
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
    public out_arrival_fbbf< pd_shared3p
    ,relColumn< public,uint32,uint8> 
    ,relColumn< D3,int32,int32> >  res1 = getTable_arrival_fbbf(ds, args1);
    public out_arrival_fbbf< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p
    ,int32
    ,int32> >  resUnique1 = deduplicate_arrival_fbbf(res1);
    public relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8>  __HOLE_14 = resUnique1.arg0;
    public relColumn< pd_shared3p,int32,int32>  Time = resUnique1.arg3;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    pd_shared3p uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "Time", filterTrue(pi, n, Time));
}