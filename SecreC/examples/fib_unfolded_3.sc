import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



template< domain D,type T0> 
struct in_fib_bf
{
    D bool [[1]] b;
    public T0 arg0;
}

template< domain D,type T1> 
struct out_fib_bf
{
    D bool [[1]] b;
    public T1 arg1;
}

template< type T0,type T1> 
T0 extend_fib_bf ( public T1 result
, public uint m
, public uint mi
, public uint ni )
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg0 = extendColumn(result.arg0, m, mi, ni);
    return result;
}

out_fib_bf< pd_shared3p
,relColumn< public,int32,int32> >  goal_fib_bf_0 ( public string ds
, public in_fib_bf< public,relColumn< pd_shared3p,int32,int32> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_fib_bf< public
    ,relColumn< pd_shared3p,int32,int32> >  table0 = extend_fib_bf( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< pd_shared3p,int32,int32>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    pd_shared3p bool [[1]] b1 = bop("==", X_0, constColumn(0, m));
    
    //q2
    public bool [[1]] b2 = bop("==", X_1, constColumn(1, m));
    
    //q3
    public bool [[1]] b3 = reshape(true, m);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3);
    public out_fib_bf< pd_shared3p,relColumn< public,int32,int32> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

out_fib_bf< pd_shared3p
,relColumn< public,int32,int32> >  goal_fib_bf_1 ( public string ds
, public in_fib_bf< public,relColumn< pd_shared3p,int32,int32> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_fib_bf< public
    ,relColumn< pd_shared3p,int32,int32> >  table0 = extend_fib_bf( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< pd_shared3p,int32,int32>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    pd_shared3p bool [[1]] b1 = bop("==", X_0, constColumn(1, m));
    
    //q2
    public bool [[1]] b2 = bop("==", X_1, constColumn(1, m));
    
    //q3
    public bool [[1]] b3 = reshape(true, m);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3);
    public out_fib_bf< pd_shared3p,relColumn< public,int32,int32> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

out_fib_bf< pd_shared3p
,relColumn< public,int32,int32> >  goal_fib_bf_2 ( public string ds
, public in_fib_bf< public,relColumn< pd_shared3p,int32,int32> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_fib_bf< public
    ,relColumn< pd_shared3p,int32,int32> >  table0 = extend_fib_bf( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< pd_shared3p,int32,int32>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    pd_shared3p bool [[1]] b1 = bop(">", X_0, constColumn(1, m));
    
    //q2
    public relColumn< pd_shared3p,int32,int32>  X_3 = aop( "-"
    , X_0
    , constColumn(1, m) );
    public bool [[1]] b2 = constColumn(true, m);
    
    //q3
    public relColumn< pd_shared3p,int32,int32>  X_5 = aop( "-"
    , X_0
    , constColumn(2, m) );
    public bool [[1]] b3 = constColumn(true, m);
    
    //q4
    pd_shared3p bool [[1]] b4 = bop("==", X_3, constColumn(0, m));
    
    //q5
    public relColumn< public,int32,int32>  X_8 = constColumn(1, m);
    public bool [[1]] b5 = constColumn(true, m);
    
    //q6
    pd_shared3p bool [[1]] b6 = bop("==", X_5, constColumn(1, m));
    
    //q7
    public relColumn< public,int32,int32>  X_10 = constColumn(1, m);
    public bool [[1]] b7 = constColumn(true, m);
    
    //q8
    public relColumn< public,int32,int32>  X_1 = aop("+", X_8, X_10);
    public bool [[1]] b8 = constColumn(true, m);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3& b4& b5& b6& b7& b8);
    public out_fib_bf< pd_shared3p,relColumn< public,int32,int32> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

out_fib_bf< pd_shared3p
,relColumn< public,int32,int32> >  goal_fib_bf_3 ( public string ds
, public in_fib_bf< public,relColumn< pd_shared3p,int32,int32> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_fib_bf< public
    ,relColumn< pd_shared3p,int32,int32> >  table0 = extend_fib_bf( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< pd_shared3p,int32,int32>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    pd_shared3p bool [[1]] b1 = bop(">", X_0, constColumn(1, m));
    
    //q2
    public relColumn< pd_shared3p,int32,int32>  X_3 = aop( "-"
    , X_0
    , constColumn(1, m) );
    public bool [[1]] b2 = constColumn(true, m);
    
    //q3
    public relColumn< pd_shared3p,int32,int32>  X_5 = aop( "-"
    , X_0
    , constColumn(2, m) );
    public bool [[1]] b3 = constColumn(true, m);
    
    //q4
    pd_shared3p bool [[1]] b4 = bop("==", X_3, constColumn(1, m));
    
    //q5
    public relColumn< public,int32,int32>  X_8 = constColumn(1, m);
    public bool [[1]] b5 = constColumn(true, m);
    
    //q6
    pd_shared3p bool [[1]] b6 = bop(">", X_5, constColumn(1, m));
    
    //q7
    public relColumn< pd_shared3p,int32,int32>  X_10 = aop( "-"
    , X_5
    , constColumn(1, m) );
    public bool [[1]] b7 = constColumn(true, m);
    
    //q8
    public relColumn< pd_shared3p,int32,int32>  X_12 = aop( "-"
    , X_5
    , constColumn(2, m) );
    public bool [[1]] b8 = constColumn(true, m);
    
    //q9
    pd_shared3p bool [[1]] b9 = bop("==", X_10, constColumn(0, m));
    
    //q10
    public relColumn< public,int32,int32>  X_15 = constColumn(1, m);
    public bool [[1]] b10 = constColumn(true, m);
    
    //q11
    pd_shared3p bool [[1]] b11 = bop("==", X_12, constColumn(1, m));
    
    //q12
    public relColumn< public,int32,int32>  X_17 = constColumn(1, m);
    public bool [[1]] b12 = constColumn(true, m);
    
    //q13
    public relColumn< public,int32,int32>  X_18 = aop("+", X_15, X_17);
    public bool [[1]] b13 = constColumn(true, m);
    
    //q14
    public relColumn< public,int32,int32>  X_1 = aop("+", X_8, X_18);
    public bool [[1]] b14 = constColumn(true, m);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3& b4& b5& b6& b7& b8& b9& b10& b11& b12& b13& b14);
    public out_fib_bf< pd_shared3p,relColumn< public,int32,int32> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

out_fib_bf< pd_shared3p
,relColumn< public,int32,int32> >  goal_fib_bf_4 ( public string ds
, public in_fib_bf< public,relColumn< pd_shared3p,int32,int32> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_fib_bf< public
    ,relColumn< pd_shared3p,int32,int32> >  table0 = extend_fib_bf( args
    , m
    , m0
    , n0 );
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< pd_shared3p,int32,int32>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    pd_shared3p bool [[1]] b1 = bop(">", X_0, constColumn(1, m));
    
    //q2
    public relColumn< pd_shared3p,int32,int32>  X_3 = aop( "-"
    , X_0
    , constColumn(1, m) );
    public bool [[1]] b2 = constColumn(true, m);
    
    //q3
    public relColumn< pd_shared3p,int32,int32>  X_5 = aop( "-"
    , X_0
    , constColumn(2, m) );
    public bool [[1]] b3 = constColumn(true, m);
    
    //q4
    pd_shared3p bool [[1]] b4 = bop(">", X_3, constColumn(1, m));
    
    //q5
    public relColumn< pd_shared3p,int32,int32>  X_8 = aop( "-"
    , X_3
    , constColumn(1, m) );
    public bool [[1]] b5 = constColumn(true, m);
    
    //q6
    public relColumn< pd_shared3p,int32,int32>  X_10 = aop( "-"
    , X_3
    , constColumn(2, m) );
    public bool [[1]] b6 = constColumn(true, m);
    
    //q7
    pd_shared3p bool [[1]] b7 = bop("==", X_8, constColumn(0, m));
    
    //q8
    public relColumn< public,int32,int32>  X_13 = constColumn(1, m);
    public bool [[1]] b8 = constColumn(true, m);
    
    //q9
    pd_shared3p bool [[1]] b9 = bop("==", X_10, constColumn(1, m));
    
    //q10
    public relColumn< public,int32,int32>  X_15 = constColumn(1, m);
    public bool [[1]] b10 = constColumn(true, m);
    
    //q11
    public relColumn< public,int32,int32>  X_16 = aop("+", X_13, X_15);
    public bool [[1]] b11 = constColumn(true, m);
    
    //q12
    pd_shared3p bool [[1]] b12 = bop(">", X_5, constColumn(1, m));
    
    //q13
    public relColumn< pd_shared3p,int32,int32>  X_20 = aop( "-"
    , X_5
    , constColumn(1, m) );
    public bool [[1]] b13 = constColumn(true, m);
    
    //q14
    public relColumn< pd_shared3p,int32,int32>  X_22 = aop( "-"
    , X_5
    , constColumn(2, m) );
    public bool [[1]] b14 = constColumn(true, m);
    
    //q15
    pd_shared3p bool [[1]] b15 = bop("==", X_20, constColumn(1, m));
    
    //q16
    public relColumn< public,int32,int32>  X_25 = constColumn(1, m);
    public bool [[1]] b16 = constColumn(true, m);
    
    //q17
    pd_shared3p bool [[1]] b17 = bop(">", X_22, constColumn(1, m));
    
    //q18
    public relColumn< pd_shared3p,int32,int32>  X_27 = aop( "-"
    , X_22
    , constColumn(1, m) );
    public bool [[1]] b18 = constColumn(true, m);
    
    //q19
    public relColumn< pd_shared3p,int32,int32>  X_29 = aop( "-"
    , X_22
    , constColumn(2, m) );
    public bool [[1]] b19 = constColumn(true, m);
    
    //q20
    pd_shared3p bool [[1]] b20 = bop("==", X_27, constColumn(0, m));
    
    //q21
    public relColumn< public,int32,int32>  X_32 = constColumn(1, m);
    public bool [[1]] b21 = constColumn(true, m);
    
    //q22
    pd_shared3p bool [[1]] b22 = bop("==", X_29, constColumn(1, m));
    
    //q23
    public relColumn< public,int32,int32>  X_34 = constColumn(1, m);
    public bool [[1]] b23 = constColumn(true, m);
    
    //q24
    public relColumn< public,int32,int32>  X_35 = aop("+", X_32, X_34);
    public bool [[1]] b24 = constColumn(true, m);
    
    //q25
    public relColumn< public,int32,int32>  X_38 = aop("+", X_25, X_35);
    public bool [[1]] b25 = constColumn(true, m);
    
    //q26
    public relColumn< public,int32,int32>  X_1 = aop("+", X_16, X_38);
    public bool [[1]] b26 = constColumn(true, m);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3& b4& b5& b6& b7& b8& b9& b10& b11& b12& b13& b14& b15& b16& b17& b18& b19& b20& b21& b22& b23& b24& b25& b26);
    public out_fib_bf< pd_shared3p,relColumn< public,int32,int32> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_fib_bf (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg1 = myCat(t1.arg1, t2.arg1);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_fib_bf (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg1 = applyPermutation(t.arg1, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_fib_bf (public string ds, public T1 args)
{
    public T0 result;
    public out_fib_bf< pd_shared3p
    ,relColumn< public,int32,int32> >  result0 = goal_fib_bf_0(ds, args);
    public out_fib_bf< pd_shared3p
    ,relColumn< public,int32,int32> >  result1 = goal_fib_bf_1(ds, args);
    public out_fib_bf< pd_shared3p
    ,relColumn< public,int32,int32> >  result2 = goal_fib_bf_2(ds, args);
    public out_fib_bf< pd_shared3p
    ,relColumn< public,int32,int32> >  result3 = goal_fib_bf_3(ds, args);
    public out_fib_bf< pd_shared3p
    ,relColumn< public,int32,int32> >  result4 = goal_fib_bf_4(ds, args);
    result = cat_fib_bf(result, result0);
    result = cat_fib_bf(result, result1);
    result = cat_fib_bf(result, result2);
    result = cat_fib_bf(result, result3);
    result = cat_fib_bf(result, result4);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_fib_bf (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg1 = copyColumn(t.arg1);
    pi = countSortPermutation(result.b);
    result = permute_fib_bf(result, pi);
    pi = quickSortPermutation(result.arg1);
    result = permute_fib_bf(result, pi);
    result.b = (result.b) & (!(findRepeating(result.arg1)));
    return result;
}

void main ()
{
    pd_shared3p int32 x = argument("x");
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< pd_shared3p,int32,int32>  arg0 = constColumn(x);
    public in_fib_bf< public,relColumn< pd_shared3p,int32,int32> >  args1;
    args1.b = trueColumn();
    args1.arg0 = arg0;
    public out_fib_bf< pd_shared3p
    ,relColumn< public,int32,int32> >  res1 = getTable_fib_bf(ds, args1);
    public out_fib_bf< pd_shared3p
    ,relColumn< pd_shared3p
    ,int32
    ,int32> >  resUnique1 = deduplicate_fib_bf(res1);
    public relColumn< pd_shared3p,int32,int32>  Y = resUnique1.arg1;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    pd_shared3p uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "Y", filterTrue(pi, n, Y));
}