import stdlib;
import shared3p;
import shared3p_string;
import shared3p_table_database;
import table_database;

import lp_essentials;



struct out_db
{
    public bool [[1]] b;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg0;
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  arg1;
}

out_db getTable_db ( public string ds
, public uint m
, public uint mi
, public uint ni )
{
    public out_db result;
    result.b = trueColumn(m);
    result.arg0 = getDBColumn(ds, "db", 0, m, mi, ni);
    result.arg1 = getDBColumn(ds, "db", 1, m, mi, ni);
    return result;
}

template< domain D,type T1> 
struct in_f_fb
{
    D bool [[1]] b;
    public T1 arg1;
}

template< domain D,type T0> 
struct out_f_fb
{
    D bool [[1]] b;
    public T0 arg0;
}

template< type T0,type T1> 
T0 extend_f_fb (public T1 result, public uint m, public uint mi, public uint ni)
{
    result.b = extendColumn(result.b, m, mi, ni);
    result.arg1 = extendColumn(result.arg1, m, mi, ni);
    return result;
}

out_f_fb< pd_shared3p
,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  goal_f_fb_0 ( public string ds
, public in_f_fb< public,relColumn< public,uint32,uint8> >  args )
{
    
    //compute the number of solutions in used predicates
    public uint m0 = size(args.b);
    public uint m1 = tdbGetRowCount(ds, "db");
    public uint m = m0* m1;
    public uint n0 = (m) / (m0);
    public uint n1 = (m) / (m0* m1);
    
    //extend the initial args to appropriate size
    public in_f_fb< public
    ,relColumn< public,uint32,uint8> >  table0 = extend_f_fb(args, m, m0, n0);
    
    //evaluate all underlying predicates
    public out_db table1 = getTable_db(ds, m, m1, n1);
    
    //assign input variables
    public relColumn< public,uint32,uint8>  X_1 = table0.arg1;
    
    //evaluate the clause body
    
    //q1
    
    //q1000
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  X_0 = table1.arg0;
    public bool [[1]] b1000 = constColumn(true, m);
    
    //q1001
    pd_shared3p bool [[1]] b1001 = bop("==", X_1, table1.arg1);
    pd_shared3p bool [[1]] b1 = b1000& b1001;
    
    //output the updated predicate arguments
    pd_shared3p bool [[1]] b = (table0.b) & (b1);
    public out_f_fb< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result;
    result.b = b;
    result.arg0 = X_0;
    return result;
}

template< type T0,type T1,type T2> 
T0 cat_f_fb (public T1 t1, public T2 t2)
{
    public T0 t0;
    t0.b = myCat(t1.b, t2.b);
    t0.arg0 = myCat(t1.arg0, t2.arg0);
    return t0;
}

template< domain D,type T,type T0,type T1> 
T0 permute_f_fb (public T1 t, D T [[1]] pi)
{
    public T0 result;
    result.b = applyPermutation(t.b, pi);
    result.arg0 = applyPermutation(t.arg0, pi);
    return result;
}

template< type T0,type T1> 
T0 getTable_f_fb (public string ds, public T1 args)
{
    public T0 result;
    public out_f_fb< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  result0 = goal_f_fb_0( ds
    , args );
    result = cat_f_fb(result, result0);
    return result;
}

template< type T0,type T1> 
T0 deduplicate_f_fb (public T1 t)
{
    pd_shared3p uint32 [[1]] pi;
    public T0 result;
    result.b = t.b;
    result.arg0 = copyColumn(t.arg0);
    pi = countSortPermutation(result.b);
    result = permute_f_fb(result, pi);
    pi = quickSortPermutation(result.arg0);
    result = permute_f_fb(result, pi);
    result.b = (result.b) & (!(findRepeating(result.arg0)));
    return result;
}

void main ()
{
    public string b = argument("b");
    public string ds = "DS1";
    tdbOpenConnection(ds);
    public relColumn< public,uint32,uint8>  arg1 = constColumn(b);
    public in_f_fb< public,relColumn< public,uint32,uint8> >  args1;
    args1.b = trueColumn();
    args1.arg1 = arg1;
    public out_f_fb< pd_shared3p
    ,relColumn< pd_shared3p,xor_uint32,xor_uint8> >  res1 = getTable_f_fb( ds
    , args1 );
    public out_f_fb< pd_shared3p
    ,relColumn< pd_shared3p
    ,xor_uint32
    ,xor_uint8> >  resUnique1 = deduplicate_f_fb(res1);
    public relColumn< pd_shared3p,xor_uint32,xor_uint8>  A = resUnique1.arg0;
    pd_shared3p bool [[1]] b1 = resUnique1.b;
    tdbCloseConnection(ds);
    public uint32 n = declassifyIfNeed(sum((uint32)b1));
    pd_shared3p uint32 [[1]] pi = lpShuffle(b1);
    publishCol(0, "A", filterTrue(pi, n, A));
}