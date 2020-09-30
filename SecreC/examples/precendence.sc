import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



template< domain D,type T0,type T1,type T2,type T3> 
struct in_f_bbbbf
{
    D bool [[1]] b;
    public T0 arg0;
    public T1 arg1;
    public T2 arg2;
    public T3 arg3;
}

template< domain D,type T4> 
struct out_f_bbbbf
{
    D bool [[1]] b;
    public T4 arg4;
}

template< type T0,type T1> 
T0 extend_f_bbbbf ( public T1 result
, public uint m
, public uint mi
, public uint ni )
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg0 = extendColumn(result.arg0, m, mi, ni);
    result.arg1 = extendColumn(result.arg1, m, mi, ni);
    result.arg2 = extendColumn(result.arg2, m, mi, ni);
    result.arg3 = extendColumn(result.arg3, m, mi, ni);
    return result;
}

out_f_bbbbf< public,float32>  goal_f_bbbbf_0 ( public string ds
, public in_f_bbbbf< public
,relColumn< public,int32,int32> 
,relColumn< public,int32,int32> 
,relColumn< public,int32,int32> 
,relColumn< public,int32,int32> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_f_bbbbf< public
    ,relColumn< public,int32,int32> 
    ,relColumn< public,int32,int32> 
    ,relColumn< public,int32,int32> 
    ,relColumn< public,int32,int32> >  table0 = extend_f_bbbbf(args, m, m0, n0);
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< public,int32,int32>  X_0 = table0.arg0;
    public relColumn< public,int32,int32>  X_1 = table0.arg1;
    public relColumn< public,int32,int32>  X_2 = table0.arg2;
    public relColumn< public,int32,int32>  X_3 = table0.arg3;
    
    //evaluate the clause body
    
    //q1
    public float32 X_4 = aop( "pow"
    , aop( "sqrt"
    , aop( "+"
    , aop("pow", aop("-", X_0, X_2), constColumn(2, m))
    , aop("pow", aop("-", X_1, X_3), constColumn(2, m)) ) )
    , aop("pow", constColumn(2, m), constColumn(3, m)) );
    public bool [[1]] b1 = constColumn(true, m);
    
    //output the updated predicate arguments
    public bool [[1]] b = (table0.b) & (b1);
    public out_f_bbbbf< public,float32>  result;
    result.b = b;
    result.arg4 = X_4;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_f_bbbbf (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg4 = myCat(t1.arg4, t2.arg4);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_f_bbbbf (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg4 = applyPermutation(t.arg4, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_f_bbbbf (public string ds, public T1 args)
{
    public T0 result;
    public out_f_bbbbf< public,float32>  result0 = goal_f_bbbbf_0(ds, args);
    result = cat_f_bbbbf(result, result0);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_f_bbbbf (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg4 = copyColumn(t.arg4);
    pi = countSortPermutation(result.b);
    result = permute_f_bbbbf(result, pi);
    pi = quickSortPermutation(result.arg4);
    result = permute_f_bbbbf(result, pi);
    result.b = (result.b) & (!(findRepeating(result.arg4)));
    return result;
}

void main ()
{
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< public,int32,int32>  arg0 = constColumn(1);
    public relColumn< public,int32,int32>  arg1 = constColumn(2);
    public relColumn< public,int32,int32>  arg2 = constColumn(4);
    public relColumn< public,int32,int32>  arg3 = constColumn(6);
    public in_f_bbbbf< public
    ,relColumn< public,int32,int32> 
    ,relColumn< public,int32,int32> 
    ,relColumn< public,int32,int32> 
    ,relColumn< public,int32,int32> >  args1;
    args1.b = trueColumn();
    args1.arg0 = arg0;
    args1.arg1 = arg1;
    args1.arg2 = arg2;
    args1.arg3 = arg3;
    public out_f_bbbbf< public,float32>  res1 = getTable_f_bbbbf(ds, args1);
    public out_f_bbbbf< pd_shared3p
    ,relColumn< pd_shared3p,T4,S4> >  resUnique1 = deduplicate_f_bbbbf(res1);
    public relColumn< pd_shared3p,T4,S4>  D = resUnique1.arg4;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    public uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "D", filterTrue(pi, n, D));
}