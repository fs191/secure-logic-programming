import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



template< domain D,type T0,type T2> 
struct in_zeroSum_bfbf
{
    D bool [[1]] b;
    public T0 arg0;
    public T2 arg2;
}

template< domain D,type T1,type T3> 
struct out_zeroSum_bfbf
{
    D bool [[1]] b;
    public T1 arg1;
    public T3 arg3;
}

template< type T0,type T1> 
T0 extend_zeroSum_bfbf ( public T1 result
, public uint m
, public uint mi
, public uint ni )
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg0 = extendColumn(result.arg0, m, mi, ni);
    result.arg2 = extendColumn(result.arg2, m, mi, ni);
    return result;
}

out_zeroSum_bfbf< public
,relColumn< D1,T1,S1> 
,relColumn< D3,T3,S3> >  goal_zeroSum_bfbf_0 ( public string ds
, public in_zeroSum_bfbf< public
,relColumn< D0,T0,S0> 
,relColumn< D2,T2,S2> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_zeroSum_bfbf< public
    ,relColumn< D0,T0,S0> 
    ,relColumn< D2,T2,S2> >  table0 = extend_zeroSum_bfbf(args, m, m0, n0);
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< D,T,S>  X_0 = table0.arg0;
    public relColumn< D,T,S>  X_2 = table0.arg2;
    
    //evaluate the clause body
    
    //q1
    public bool [[1]] b1 = bop("==", aop("+", X_0, X_1), aop("+", X_2, X_3));
    
    //output the updated predicate arguments
    public bool [[1]] b = (table0.b) & (b1);
    public out_zeroSum_bfbf< public
    ,relColumn< D1,T1,S1> 
    ,relColumn< D3,T3,S3> >  result;
    result.b = b;
    result.arg1 = X_1;
    result.arg3 = X_3;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_zeroSum_bfbf (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg1 = myCat(t1.arg1, t2.arg1);
    t0.arg3 = myCat(t1.arg3, t2.arg3);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_zeroSum_bfbf (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg1 = applyPermutation(t.arg1, pi);
    result.arg3 = applyPermutation(t.arg3, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_zeroSum_bfbf (public string ds, public T1 args)
{
    public T0 result;
    public out_zeroSum_bfbf< public
    ,relColumn< D,T,S> 
    ,relColumn< D,T,S> >  result0 = goal_zeroSum_bfbf_0(ds, args);
    result = cat_zeroSum_bfbf(result, result0);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_zeroSum_bfbf (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg1 = copyColumn(t.arg1);
    result.arg3 = copyColumn(t.arg3);
    pi = countSortPermutation(result.b);
    result = permute_zeroSum_bfbf(result, pi);
    pi = quickSortPermutation(result.arg1);
    result = permute_zeroSum_bfbf(result, pi);
    pi = quickSortPermutation(result.arg3);
    result = permute_zeroSum_bfbf(result, pi);
    result.b = (result.b) & (!((findRepeating( result.arg3 )) & (findRepeating( result.arg1 ))));
    return result;
}

void main ()
{
    D T x1 = argument("x1");
    D T y1 = argument("y1");
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< D0,T0,S0>  arg0 = constColumn(x1);
    public relColumn< D2,T2,S2>  arg2 = constColumn(y1);
    public in_zeroSum_bfbf< public
    ,relColumn< D0,T0,S0> 
    ,relColumn< D2,T2,S2> >  args1;
    args1.b = trueColumn();
    args1.arg0 = arg0;
    args1.arg2 = arg2;
    public out_zeroSum_bfbf< public
    ,relColumn< D1,T1,S1> 
    ,relColumn< D3,T3,S3> >  res1 = getTable_zeroSum_bfbf(ds, args1);
    public out_zeroSum_bfbf< pd_shared3p
    ,relColumn< pd_shared3p,T1,S1> 
    ,relColumn< pd_shared3p
    ,T3
    ,S3> >  resUnique1 = deduplicate_zeroSum_bfbf(res1);
    public relColumn< pd_shared3p,T1,S1>  X2 = resUnique1.arg1;
    public relColumn< pd_shared3p,T3,S3>  Y2 = resUnique1.arg3;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    public uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "X2", filterTrue(pi, n, X2));
    publishCol(1, "Y2", filterTrue(pi, n, Y2));
}