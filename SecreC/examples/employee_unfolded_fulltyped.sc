import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



struct out_eds
{
    public bool [[1]] b;
    public relColumn< public,uint32,uint8>  arg0;
    public relColumn< public,uint32,uint8>  arg1;
    public relColumn< pd_shared3p,int32,int32>  arg2;
}

struct out_dm
{
    public bool [[1]] b;
    public relColumn< public,uint32,uint8>  arg0;
    public relColumn< public,uint32,uint8>  arg1;
}

out_eds getTable_eds ( public string ds
, public uint m
, public uint mi
, public uint ni )
{
    public out_eds result;
    result.b = trueColumn(m);
    result.arg0 = getDBColumn(ds, "eds", 0, m, mi, ni);
    result.arg1 = getDBColumn(ds, "eds", 1, m, mi, ni);
    result.arg2 = getDBColumn(ds, "eds", 2, m, mi, ni);
    return result;
}

out_dm getTable_dm ( public string ds
, public uint m
, public uint mi
, public uint ni )
{
    public out_dm result;
    result.b = trueColumn(m);
    result.arg0 = getDBColumn(ds, "dm", 0, m, mi, ni);
    result.arg1 = getDBColumn(ds, "dm", 1, m, mi, ni);
    return result;
}

template< domain D,type T2> 
struct in_viewESM_ffb
{
    D bool [[1]] b;
    public T2 arg2;
}

template< domain D,type T0,type T1> 
struct out_viewESM_ffb
{
    D bool [[1]] b;
    public T0 arg0;
    public T1 arg1;
}

template< type T0,type T1> 
T0 extend_viewESM_ffb ( public T1 result
, public uint m
, public uint mi
, public uint ni )
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg2 = extendColumn(result.arg2, m, mi, ni);
    return result;
}

out_viewESM_ffb< pd_shared3p
,relColumn< public,uint32,uint8> 
,relColumn< pd_shared3p,int32,int32> >  goal_viewESM_ffb_0 ( public string ds
, public in_viewESM_ffb< public,relColumn< public,uint32,uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m1 = tdbGetRowCount(ds, "eds");
    public uint m3 = tdbGetRowCount(ds, "dm");
    public uint m = m0* m1* m3;
    public uint n0 = (m) / (m0);
    public uint n1 = (m) / (m0* m1);
    public uint n3 = (m) / (m0* m1* m3);
    
    //extend the initial args to appropriate size
    public in_viewESM_ffb< public
    ,relColumn< public,uint32,uint8> >  table0 = extend_viewESM_ffb( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    public out_eds table1 = getTable_eds(ds, m, m1, n1);
    public out_dm table3 = getTable_dm(ds, m, m3, n3);
    
    //assign input variables
    public relColumn< public,uint32,uint8>  X_2 = table0.arg2;
    
    //evaluate the clause body
    
    //q1
    
    //q1000
    public relColumn< public,uint32,uint8>  X_0 = table1.arg0;
    public bool [[1]] b1000 = constColumn(true, m);
    
    //q1001
    public relColumn< public,uint32,uint8>  X_4 = table1.arg1;
    public bool [[1]] b1001 = constColumn(true, m);
    
    //q1002
    public relColumn< pd_shared3p,int32,int32>  X_1 = table1.arg2;
    public bool [[1]] b1002 = constColumn(true, m);
    public bool [[1]] b1 = b1000& b1001& b1002;
    
    //q2
    pd_shared3p bool [[1]] b2 = bop("<", X_1, constColumn(2500, m));
    
    //q3
    
    //q3000
    public relColumn< public,uint32,uint8>  X_7 = table3.arg0;
    public bool [[1]] b3000 = constColumn(true, m);
    
    //q3001
    public bool [[1]] b3001 = bop("==", X_2, table3.arg1);
    public bool [[1]] b3 = b3000& b3001;
    
    //q4
    public bool [[1]] b4 = bop("==", X_4, X_7);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3& b4);
    public out_viewESM_ffb< pd_shared3p
    ,relColumn< public,uint32,uint8> 
    ,relColumn< pd_shared3p,int32,int32> >  result;
    result.b = b;
    result.arg0 = X_0;
    result.arg1 = X_1;
    return result;
}

out_viewESM_ffb< pd_shared3p
,relColumn< public,uint32,uint8> 
,relColumn< pd_shared3p,int32,int32> >  goal_viewESM_ffb_1 ( public string ds
, public in_viewESM_ffb< public,relColumn< public,uint32,uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m2 = tdbGetRowCount(ds, "eds");
    public uint m4 = tdbGetRowCount(ds, "dm");
    public uint m = m0* m2* m4;
    public uint n0 = (m) / (m0);
    public uint n2 = (m) / (m0* m2);
    public uint n4 = (m) / (m0* m2* m4);
    
    //extend the initial args to appropriate size
    public in_viewESM_ffb< public
    ,relColumn< public,uint32,uint8> >  table0 = extend_viewESM_ffb( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    public out_eds table2 = getTable_eds(ds, m, m2, n2);
    public out_dm table4 = getTable_dm(ds, m, m4, n4);
    
    //assign input variables
    public relColumn< public,uint32,uint8>  X_2 = table0.arg2;
    
    //evaluate the clause body
    
    //q1
    public relColumn< pd_shared3p,int32,int32>  X_1 = constColumn(0, m);
    public bool [[1]] b1 = constColumn(true, m);
    
    //q2
    
    //q2000
    public relColumn< public,uint32,uint8>  X_0 = table2.arg0;
    public bool [[1]] b2000 = constColumn(true, m);
    
    //q2001
    public relColumn< public,uint32,uint8>  X_5 = table2.arg1;
    public bool [[1]] b2001 = constColumn(true, m);
    
    //q2002
    public relColumn< pd_shared3p,int32,int32>  X_6 = table2.arg2;
    public bool [[1]] b2002 = constColumn(true, m);
    public bool [[1]] b2 = b2000& b2001& b2002;
    
    //q3
    pd_shared3p bool [[1]] b3 = bop("<=", constColumn(2500, m), X_6);
    
    //q4
    
    //q4000
    public relColumn< public,uint32,uint8>  X_8 = table4.arg0;
    public bool [[1]] b4000 = constColumn(true, m);
    
    //q4001
    public bool [[1]] b4001 = bop("==", X_2, table4.arg1);
    public bool [[1]] b4 = b4000& b4001;
    
    //q5
    public bool [[1]] b5 = bop("==", X_5, X_8);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3& b4& b5);
    public out_viewESM_ffb< pd_shared3p
    ,relColumn< public,uint32,uint8> 
    ,relColumn< pd_shared3p,int32,int32> >  result;
    result.b = b;
    result.arg0 = X_0;
    result.arg1 = X_1;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_viewESM_ffb (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg0 = myCat(t1.arg0, t2.arg0);
    t0.arg1 = myCat(t1.arg1, t2.arg1);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_viewESM_ffb (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg0 = applyPermutation(t.arg0, pi);
    result.arg1 = applyPermutation(t.arg1, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_viewESM_ffb (public string ds, public T1 args)
{
    public T0 result;
    public out_viewESM_ffb< pd_shared3p
    ,relColumn< public,uint32,uint8> 
    ,relColumn< pd_shared3p,int32,int32> >  result0 = goal_viewESM_ffb_0( ds
    , args );
    public out_viewESM_ffb< pd_shared3p
    ,relColumn< public,uint32,uint8> 
    ,relColumn< pd_shared3p,int32,int32> >  result1 = goal_viewESM_ffb_1( ds
    , args );
    result = cat_viewESM_ffb(result, result0);
    result = cat_viewESM_ffb(result, result1);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_viewESM_ffb (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg0 = copyColumn(t.arg0);
    result.arg1 = copyColumn(t.arg1);
    pi = countSortPermutation(result.b);
    result = permute_viewESM_ffb(result, pi);
    pi = quickSortPermutation(result.arg0);
    result = permute_viewESM_ffb(result, pi);
    pi = quickSortPermutation(result.arg1);
    result = permute_viewESM_ffb(result, pi);
    result.b = (result.b) & (!((findRepeating( result.arg1 )) & (findRepeating( result.arg0 ))));
    return result;
}

void main ()
{
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< public,uint32,uint8>  arg2 = constColumn("manager1");
    public in_viewESM_ffb< public,relColumn< public,uint32,uint8> >  args1;
    args1.b = trueColumn();
    args1.arg2 = arg2;
    public out_viewESM_ffb< pd_shared3p
    ,relColumn< public,uint32,uint8> 
    ,relColumn< pd_shared3p,int32,int32> >  res1 = getTable_viewESM_ffb( ds
    , args1 );
    public out_viewESM_ffb< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> 
    ,relColumn< pd_shared3p
    ,int32
    ,int32> >  resUnique1 = deduplicate_viewESM_ffb(res1);
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  Y1 = resUnique1.arg0;
    public relColumn< pd_shared3p,int32,int32>  Y2 = resUnique1.arg1;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    pd_shared3p uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "Y1", filterTrue(pi, n, Y1));
    publishCol(1, "Y2", filterTrue(pi, n, Y2));
}