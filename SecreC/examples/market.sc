import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



struct out_sells
{
    public bool [[1]] b;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg0;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg1;
    public relColumn< pd_shared3p,int32,int32>  arg2;
}

struct out_buys
{
    public bool [[1]] b;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg0;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg1;
    public relColumn< pd_shared3p,int32,int32>  arg2;
}

out_sells getTable_sells ( public string ds
, public uint m
, public uint mi
, public uint ni )
{
    public out_sells result;
    result.b = trueColumn(m);
    result.arg0 = getDBColumn(ds, "sells", 0, m, mi, ni);
    result.arg1 = getDBColumn(ds, "sells", 1, m, mi, ni);
    result.arg2 = getDBColumn(ds, "sells", 2, m, mi, ni);
    return result;
}

out_buys getTable_buys ( public string ds
, public uint m
, public uint mi
, public uint ni )
{
    public out_buys result;
    result.b = trueColumn(m);
    result.arg0 = getDBColumn(ds, "buys", 0, m, mi, ni);
    result.arg1 = getDBColumn(ds, "buys", 1, m, mi, ni);
    result.arg2 = getDBColumn(ds, "buys", 2, m, mi, ni);
    return result;
}

template< domain D,type T0> 
struct in_bargain_bff
{
    D bool [[1]] b;
    public T0 arg0;
}

template< domain D,type T1,type T2> 
struct out_bargain_bff
{
    D bool [[1]] b;
    public T1 arg1;
    public T2 arg2;
}

template< type T0,type T1> 
T0 extend_bargain_bff ( public T1 result
, public uint m
, public uint mi
, public uint ni )
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg0 = extendColumn(result.arg0, m, mi, ni);
    return result;
}

out_bargain_bff< pd_shared3p
,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
,relColumn< pd_shared3p
,xor_uint32
,xor_uint8> >  goal_bargain_bff_0 ( public string ds
, public in_bargain_bff< public,relColumn< public,uint32,uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m1 = tdbGetRowCount(ds, "buys");
    public uint m2 = tdbGetRowCount(ds, "sells");
    public uint m = m0* m1* m2;
    public uint n0 = (m) / (m0);
    public uint n1 = (m) / (m0* m1);
    public uint n2 = (m) / (m0* m1* m2);
    
    //extend the initial args to appropriate size
    public in_bargain_bff< public
    ,relColumn< public,uint32,uint8> >  table0 = extend_bargain_bff( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    public out_buys table1 = getTable_buys(ds, m, m1, n1);
    public out_sells table2 = getTable_sells(ds, m, m2, n2);
    
    //assign input variables
    public relColumn< public,uint32,uint8>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    
    //q1000
    pd_shared3p bool [[1]] b1000 = bop("==", X_0, table1.arg0);
    
    //q1001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_2 = table1.arg1;
    public bool [[1]] b1001 = constColumn(true, m);
    
    //q1002
    public relColumn< pd_shared3p,int32,int32>  X_5 = table1.arg2;
    public bool [[1]] b1002 = constColumn(true, m);
    pd_shared3p bool [[1]] b1 = b1000& b1001& b1002;
    
    //q2
    
    //q2000
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_1 = table2.arg0;
    public bool [[1]] b2000 = constColumn(true, m);
    
    //q2001
    pd_shared3p bool [[1]] b2001 = bop("==", X_2, table2.arg1);
    
    //q2002
    public relColumn< pd_shared3p,int32,int32>  X_8 = table2.arg2;
    public bool [[1]] b2002 = constColumn(true, m);
    pd_shared3p bool [[1]] b2 = b2000& b2001& b2002;
    
    //q3
    pd_shared3p bool [[1]] b3 = bop(">=", X_5, X_8);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3);
    public out_bargain_bff< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result;
    result.b = b;
    result.arg1 = X_1;
    result.arg2 = X_2;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_bargain_bff (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg1 = myCat(t1.arg1, t2.arg1);
    t0.arg2 = myCat(t1.arg2, t2.arg2);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_bargain_bff (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg1 = applyPermutation(t.arg1, pi);
    result.arg2 = applyPermutation(t.arg2, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_bargain_bff (public string ds, public T1 args)
{
    public T0 result;
    public out_bargain_bff< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8> >  result0 = goal_bargain_bff_0(ds, args);
    result = cat_bargain_bff(result, result0);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_bargain_bff (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg1 = copyColumn(t.arg1);
    result.arg2 = copyColumn(t.arg2);
    pi = countSortPermutation(result.b);
    result = permute_bargain_bff(result, pi);
    pi = quickSortPermutation(result.arg1);
    result = permute_bargain_bff(result, pi);
    pi = quickSortPermutation(result.arg2);
    result = permute_bargain_bff(result, pi);
    result.b = (result.b) & (!((findRepeating( result.arg2 )) & (findRepeating( result.arg1 ))));
    return result;
}

void main ()
{
    public string x1 = argument("x1");
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< public,uint32,uint8>  arg0 = constColumn(x1);
    public in_bargain_bff< public,relColumn< public,uint32,uint8> >  args1;
    args1.b = trueColumn();
    args1.arg0 = arg0;
    public out_bargain_bff< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8> >  res1 = getTable_bargain_bff(ds, args1);
    public out_bargain_bff< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8> >  resUnique1 = deduplicate_bargain_bff(res1);
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  Y = resUnique1.arg1;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X2 = resUnique1.arg2;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    pd_shared3p uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "Y", filterTrue(pi, n, Y));
}