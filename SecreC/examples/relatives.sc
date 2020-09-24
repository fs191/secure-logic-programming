import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



struct out_par
{
    public bool [[1]] b;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg0;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg1;
}

out_par getTable_par ( public string ds
, public uint m
, public uint mi
, public uint ni )
{
    public out_par result;
    result.b = trueColumn(m);
    result.arg0 = getDBColumn(ds, "par", 0, m, mi, ni);
    result.arg1 = getDBColumn(ds, "par", 1, m, mi, ni);
    return result;
}

template< domain D,type T0> 
struct in_sg_bf
{
    D bool [[1]] b;
    public T0 arg0;
}

template< domain D,type T1> 
struct out_sg_bf
{
    D bool [[1]] b;
    public T1 arg1;
}

template< type T0,type T1> 
T0 extend_sg_bf ( public T1 result
, public uint m
, public uint mi
, public uint ni )
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg0 = extendColumn(result.arg0, m, mi, ni);
    return result;
}

out_sg_bf< public
,relColumn< public,uint32,uint8> >  goal_sg_bf_0 ( public string ds
, public in_sg_bf< public,relColumn< public,uint32,uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m = m0;
    public uint n0 = (m) / (m0);
    
    //extend the initial args to appropriate size
    public in_sg_bf< public
    ,relColumn< public,uint32,uint8> >  table0 = extend_sg_bf(args, m, m0, n0);
    
    //evaluate all underlying predicates
    
    //assign input variables
    public relColumn< public,uint32,uint8>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    public relColumn< public,uint32,uint8>  X_1 = X_0;
    public bool [[1]] b1 = constColumn(true, m);
    
    //output the updated predicate arguments
    public bool [[1]] b = (table0.b) & (b1);
    public out_sg_bf< public,relColumn< public,uint32,uint8> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

out_sg_bf< pd_shared3p
,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  goal_sg_bf_1 ( public string ds
, public in_sg_bf< public,relColumn< public,uint32,uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m1 = tdbGetRowCount(ds, "par");
    public uint m2 = tdbGetRowCount(ds, "par");
    public uint m = m0* m1* m2;
    public uint n0 = (m) / (m0);
    public uint n1 = (m) / (m0* m1);
    public uint n2 = (m) / (m0* m1* m2);
    
    //extend the initial args to appropriate size
    public in_sg_bf< public
    ,relColumn< public,uint32,uint8> >  table0 = extend_sg_bf(args, m, m0, n0);
    
    //evaluate all underlying predicates
    public out_par table1 = getTable_par(ds, m, m1, n1);
    public out_par table2 = getTable_par(ds, m, m2, n2);
    
    //assign input variables
    public relColumn< public,uint32,uint8>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    
    //q1000
    pd_shared3p bool [[1]] b1000 = bop("==", X_0, table1.arg0);
    
    //q1001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_3 = table1.arg1;
    public bool [[1]] b1001 = constColumn(true, m);
    pd_shared3p bool [[1]] b1 = b1000& b1001;
    
    //q2
    
    //q2000
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_1 = table2.arg0;
    public bool [[1]] b2000 = constColumn(true, m);
    
    //q2001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_5 = table2.arg1;
    public bool [[1]] b2001 = constColumn(true, m);
    public bool [[1]] b2 = b2000& b2001;
    
    //q3
    pd_shared3p bool [[1]] b3 = bop("==", X_3, X_5);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3);
    public out_sg_bf< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

out_sg_bf< pd_shared3p
,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  goal_sg_bf_2 ( public string ds
, public in_sg_bf< public,relColumn< public,uint32,uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m1 = tdbGetRowCount(ds, "par");
    public uint m2 = tdbGetRowCount(ds, "par");
    public uint m3 = tdbGetRowCount(ds, "par");
    public uint m4 = tdbGetRowCount(ds, "par");
    public uint m = m0* m1* m2* m3* m4;
    public uint n0 = (m) / (m0);
    public uint n1 = (m) / (m0* m1);
    public uint n2 = (m) / (m0* m1* m2);
    public uint n3 = (m) / (m0* m1* m2* m3);
    public uint n4 = (m) / (m0* m1* m2* m3* m4);
    
    //extend the initial args to appropriate size
    public in_sg_bf< public
    ,relColumn< public,uint32,uint8> >  table0 = extend_sg_bf(args, m, m0, n0);
    
    //evaluate all underlying predicates
    public out_par table1 = getTable_par(ds, m, m1, n1);
    public out_par table2 = getTable_par(ds, m, m2, n2);
    public out_par table3 = getTable_par(ds, m, m3, n3);
    public out_par table4 = getTable_par(ds, m, m4, n4);
    
    //assign input variables
    public relColumn< public,uint32,uint8>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    
    //q1000
    pd_shared3p bool [[1]] b1000 = bop("==", X_0, table1.arg0);
    
    //q1001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_3 = table1.arg1;
    public bool [[1]] b1001 = constColumn(true, m);
    pd_shared3p bool [[1]] b1 = b1000& b1001;
    
    //q2
    
    //q2000
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_1 = table2.arg0;
    public bool [[1]] b2000 = constColumn(true, m);
    
    //q2001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_5 = table2.arg1;
    public bool [[1]] b2001 = constColumn(true, m);
    public bool [[1]] b2 = b2000& b2001;
    
    //q3
    
    //q3000
    pd_shared3p bool [[1]] b3000 = bop("==", X_3, table3.arg0);
    
    //q3001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_7 = table3.arg1;
    public bool [[1]] b3001 = constColumn(true, m);
    pd_shared3p bool [[1]] b3 = b3000& b3001;
    
    //q4
    
    //q4000
    pd_shared3p bool [[1]] b4000 = bop("==", X_5, table4.arg0);
    
    //q4001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_9 = table4.arg1;
    public bool [[1]] b4001 = constColumn(true, m);
    pd_shared3p bool [[1]] b4 = b4000& b4001;
    
    //q5
    pd_shared3p bool [[1]] b5 = bop("==", X_7, X_9);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3& b4& b5);
    public out_sg_bf< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

out_sg_bf< pd_shared3p
,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  goal_sg_bf_3 ( public string ds
, public in_sg_bf< public,relColumn< public,uint32,uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m1 = tdbGetRowCount(ds, "par");
    public uint m2 = tdbGetRowCount(ds, "par");
    public uint m3 = tdbGetRowCount(ds, "par");
    public uint m4 = tdbGetRowCount(ds, "par");
    public uint m5 = tdbGetRowCount(ds, "par");
    public uint m6 = tdbGetRowCount(ds, "par");
    public uint m = m0* m1* m2* m3* m4* m5* m6;
    public uint n0 = (m) / (m0);
    public uint n1 = (m) / (m0* m1);
    public uint n2 = (m) / (m0* m1* m2);
    public uint n3 = (m) / (m0* m1* m2* m3);
    public uint n4 = (m) / (m0* m1* m2* m3* m4);
    public uint n5 = (m) / (m0* m1* m2* m3* m4* m5);
    public uint n6 = (m) / (m0* m1* m2* m3* m4* m5* m6);
    
    //extend the initial args to appropriate size
    public in_sg_bf< public
    ,relColumn< public,uint32,uint8> >  table0 = extend_sg_bf(args, m, m0, n0);
    
    //evaluate all underlying predicates
    public out_par table1 = getTable_par(ds, m, m1, n1);
    public out_par table2 = getTable_par(ds, m, m2, n2);
    public out_par table3 = getTable_par(ds, m, m3, n3);
    public out_par table4 = getTable_par(ds, m, m4, n4);
    public out_par table5 = getTable_par(ds, m, m5, n5);
    public out_par table6 = getTable_par(ds, m, m6, n6);
    
    //assign input variables
    public relColumn< public,uint32,uint8>  X_0 = table0.arg0;
    
    //evaluate the clause body
    
    //q1
    
    //q1000
    pd_shared3p bool [[1]] b1000 = bop("==", X_0, table1.arg0);
    
    //q1001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_3 = table1.arg1;
    public bool [[1]] b1001 = constColumn(true, m);
    pd_shared3p bool [[1]] b1 = b1000& b1001;
    
    //q2
    
    //q2000
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_1 = table2.arg0;
    public bool [[1]] b2000 = constColumn(true, m);
    
    //q2001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_5 = table2.arg1;
    public bool [[1]] b2001 = constColumn(true, m);
    public bool [[1]] b2 = b2000& b2001;
    
    //q3
    
    //q3000
    pd_shared3p bool [[1]] b3000 = bop("==", X_3, table3.arg0);
    
    //q3001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_7 = table3.arg1;
    public bool [[1]] b3001 = constColumn(true, m);
    pd_shared3p bool [[1]] b3 = b3000& b3001;
    
    //q4
    
    //q4000
    pd_shared3p bool [[1]] b4000 = bop("==", X_5, table4.arg0);
    
    //q4001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_9 = table4.arg1;
    public bool [[1]] b4001 = constColumn(true, m);
    pd_shared3p bool [[1]] b4 = b4000& b4001;
    
    //q5
    
    //q5000
    pd_shared3p bool [[1]] b5000 = bop("==", X_7, table5.arg0);
    
    //q5001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_11 = table5.arg1;
    public bool [[1]] b5001 = constColumn(true, m);
    pd_shared3p bool [[1]] b5 = b5000& b5001;
    
    //q6
    
    //q6000
    pd_shared3p bool [[1]] b6000 = bop("==", X_9, table6.arg0);
    
    //q6001
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_13 = table6.arg1;
    public bool [[1]] b6001 = constColumn(true, m);
    pd_shared3p bool [[1]] b6 = b6000& b6001;
    
    //q7
    pd_shared3p bool [[1]] b7 = bop("==", X_11, X_13);
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1& b2& b3& b4& b5& b6& b7);
    public out_sg_bf< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result;
    result.b = b;
    result.arg1 = X_1;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_sg_bf (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg1 = myCat(t1.arg1, t2.arg1);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_sg_bf (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg1 = applyPermutation(t.arg1, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_sg_bf (public string ds, public T1 args)
{
    public T0 result;
    public out_sg_bf< public
    ,relColumn< public,uint32,uint8> >  result0 = goal_sg_bf_0(ds, args);
    public out_sg_bf< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result1 = goal_sg_bf_1( ds
    , args );
    public out_sg_bf< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result2 = goal_sg_bf_2( ds
    , args );
    public out_sg_bf< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result3 = goal_sg_bf_3( ds
    , args );
    result = cat_sg_bf(result, result0);
    result = cat_sg_bf(result, result1);
    result = cat_sg_bf(result, result2);
    result = cat_sg_bf(result, result3);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_sg_bf (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg1 = copyColumn(t.arg1);
    pi = countSortPermutation(result.b);
    result = permute_sg_bf(result, pi);
    pi = quickSortPermutation(result.arg1);
    result = permute_sg_bf(result, pi);
    result.b = (result.b) & (!(findRepeating(result.arg1)));
    return result;
}

void main ()
{
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< public,uint32,uint8>  arg0 = constColumn("chris");
    public in_sg_bf< public,relColumn< public,uint32,uint8> >  args1;
    args1.b = trueColumn();
    args1.arg0 = arg0;
    public out_sg_bf< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  res1 = getTable_sg_bf( ds
    , args1 );
    public out_sg_bf< pd_shared3p
    ,relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8> >  resUnique1 = deduplicate_sg_bf(res1);
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  Y = resUnique1.arg1;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    pd_shared3p uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "Y", filterTrue(pi, n, Y));
}