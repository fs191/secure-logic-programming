import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



template< domain D,type T0,type T1> 
struct in_length_bbf
{
    D bool [[1]] b;
    public T0 arg0;
    public T1 arg1;
}

template< domain D,type T2> 
struct out_length_bbf
{
    D bool [[1]] b;
    public T2 arg2;
}

template< type T0,type T1> 
T0 extend_length_bbf ( public T1 result
, public uint m
, public uint mi
, public uint ni )
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg0 = extendColumn(result.arg0, m, mi, ni);
    result.arg1 = extendColumn(result.arg1, m, mi, ni);
    return result;
}

out_length_bbf< public
,relColumn< D2,T2,S2> >  goal_length_bbf_0 ( public string ds
, public in_length_bbf< public
,relColumn< public,int32,int32> 
,relColumn< public,int32,int32> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_length_bbf< public
    ,relColumn< public,int32,int32> 
    ,relColumn< public,int32,int32> >  table0 = extend_length_bbf( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< public,int32,int32>  X_0 = table0.arg0;
    public relColumn< public,int32,int32>  X_1 = table0.arg1;
    
    //evaluate the clause body
    
    //q1
    public relColumn< D,T,S>  X_2 = aop( "sqrt"
    , aop( "+"
    , aop( "pow"
    , (float32)aop("-", constColumn(0, m), X_0)
    , (float32)constColumn(2, m) )
    , aop( "pow"
    , (float32)aop("-", constColumn(0, m), X_1)
    , (float32)constColumn(2, m) ) ) );
    public bool [[1]] b1 = constColumn(true, m);
    
    //output the updated predicate arguments
    public bool [[1]] b = (table0.b) & (b1);
    public out_length_bbf< public,relColumn< D2,T2,S2> >  result;
    result.b = b;
    result.arg2 = X_2;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_length_bbf (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg2 = myCat(t1.arg2, t2.arg2);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_length_bbf (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg2 = applyPermutation(t.arg2, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_length_bbf (public string ds, public T1 args)
{
    public T0 result;
    public out_length_bbf< public
    ,relColumn< D,T,S> >  result0 = goal_length_bbf_0(ds, args);
    result = cat_length_bbf(result, result0);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_length_bbf (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg2 = copyColumn(t.arg2);
    pi = countSortPermutation(result.b);
    result = permute_length_bbf(result, pi);
    pi = quickSortPermutation(result.arg2);
    result = permute_length_bbf(result, pi);
    result.b = (result.b) & (!(findRepeating(result.arg2)));
    return result;
}

void main ()
{
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< public,int32,int32>  arg0 = constColumn(4);
    public relColumn< public,int32,int32>  arg1 = constColumn(3);
    public in_length_bbf< public
    ,relColumn< public,int32,int32> 
    ,relColumn< public,int32,int32> >  args1;
    args1.b = trueColumn();
    args1.arg0 = arg0;
    args1.arg1 = arg1;
    public out_length_bbf< public
    ,relColumn< D2,T2,S2> >  res1 = getTable_length_bbf(ds, args1);
    public out_length_bbf< pd_shared3p
    ,relColumn< pd_shared3p,T2,S2> >  resUnique1 = deduplicate_length_bbf(res1);
    public relColumn< pd_shared3p,T2,S2>  L = resUnique1.arg2;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    public uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "L", filterTrue(pi, n, L));
}