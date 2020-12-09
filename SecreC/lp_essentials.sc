module lp_essentials;

import stdlib;
import shared3p;
import shared3p_random;
import shared3p_string;

import shared3p_table_database;
import table_database;

domain pd_shared3p shared3p;

// the main data structure of LP, one column of a relation
// we work with transposed relations to support different column types
template<domain D, type T, type S>
struct relColumn{
    uint tableRef;   //reference to the parent table
    D T [[1]] val;   //column as a value  (used in computation and comparison, e.g. string hash)
    D S [[2]] str;   //column as a string (used in printing and returning the final result e.g. string itself)
}

template<domain D, type T>
relColumn<D,T,T> copyColumn(relColumn<D,T,T> a){
    relColumn<D,T,T> b;
    b.tableRef = a.tableRef;
    b.str = a.str;
    b.val = a.val;
    return b;
}

template<domain D, type T>
relColumn<D,T,T> copyColumn(relColumn<public,T,T> a){
    relColumn<D,T,T> b;
    b.tableRef = a.tableRef;
    b.str = a.str;
    b.val = a.val;
    return b;
}

relColumn<pd_shared3p,xor_uint32,xor_uint8> copyColumn(relColumn<pd_shared3p,xor_uint32,xor_uint8> a){
    relColumn<pd_shared3p,xor_uint32,xor_uint8> b;
    b.tableRef = a.tableRef;
    b.str = a.str;
    b.val = a.val;
    return b;
}

relColumn<public,uint32,uint8> copyColumn(relColumn<public,uint32,uint8> a){
    relColumn<public,uint32,uint8> b;
    b.tableRef = a.tableRef;
    b.str = a.str;
    b.val = a.val;
    return b;
}

relColumn<pd_shared3p,xor_uint32,xor_uint8> copyColumn(relColumn<public,uint32,uint8> a){
    relColumn<pd_shared3p,xor_uint32,xor_uint8> b;
    b.tableRef = a.tableRef;
    b.str = a.str;
    b.val = a.val;
    return b;
}

template <domain D, type T, type S>
uint colSize(relColumn<D,T,S> x){
    return size(x.val);
}

//the core rearrangement function
template <type T, type S, dim M, dim N>
pd_shared3p T [[N]] partialRearrange(pd_shared3p T [[M]] a, pd_shared3p T [[N]] b, S [[1]] source, S [[1]] target){
    assert(size(source) == size(target));
    pd_shared3p T [[1]] temp (size(source));
    __syscall("shared3p::gather_$T\_vec",  __domainid(pd_shared3p), a, temp, __cref (uint)source);
    __syscall("shared3p::scatter_$T\_vec", __domainid(pd_shared3p), temp, b, __cref (uint)target);
    return b;
}

template <type T, type S>
public T [[1]] partialRearrange(public T [[1]] a, public T [[1]] b, S [[1]] source, S [[1]] target){
    assert(size(source) == size(target));
    for (uint i = 0; i < size(source); i++){
        b[target[i]] = a[source[i]];
    }
    return b;
}

template <type T, type S>
public T [[2]] partialRearrange(public T [[2]] a, public T [[2]] b, S [[1]] source, S [[1]] target){
    assert(size(source) == size(target));
    T [[1]] a_flat = reshape(a,size(a));
    T [[1]] b_flat = reshape(b,size(b));
    for (uint i = 0; i < size(source); i++){
        b_flat[target[i]] = a_flat[source[i]];
    }
    return reshape(b_flat, shape(b)[0], shape(b)[1]);
}

//generalize indices to a second dimension
uint [[1]] widen_indices (uint [[1]] indices, uint m){
    uint k = 0;
    uint [[1]] result (size(indices) * m);
    indices = indices * m;
    for (uint i = 0; i < size(indices); i++){
        for (uint j = 0; j < m; j++){
             result[k] = indices[i] + j;
             k = k + 1;
        }
    }
    return result;
}


//sublist (of a list) by indices
template <domain D, type T>
D T [[1]] select(D T [[1]] a, uint [[1]] src){

    uint n = size(src);
    uint [[1]] tgt = iota(n);
    D T [[1]] b (n);
    return partialRearrange(a, b, src, tgt);
}

template <domain D, type T>
D T [[2]] select(D T [[2]] a, uint [[1]] src){

    uint m0 = shape(a)[0];
    uint m1 = shape(a)[1];
    uint n = size(src);

    uint [[1]] srcw = widen_indices(src, m1);
    uint [[1]] tgtw = iota(n*m1);

    D T [[2]] b (n, m1);
    return partialRearrange(a, b, srcw, tgtw);
}

template <domain D>
D bool [[1]] select(D bool [[1]] b, uint [[2]] is){
    return select(b, is[shape(is)[0] - 1, :]);
}

template <domain D, type T, type S>
relColumn<D,T,S> select(relColumn<D,T,S> a, uint [[2]] is){
    a.val = select(a.val, is[a.tableRef,:]);
    a.str = select(a.str, is[a.tableRef,:]);
    return a;
}

//sublist (of a list) by a bitmask
template <type T>
T [[1]] filter(T [[1]] a, bool [[1]] bitmask){
    uint m = size(bitmask);
    uint n = sum((uint)bitmask);
    T [[1]] b (n);
    uint j = 0;
    for (uint i = 0; i < m; i++){
        if (bitmask[i]){
            b[j] = a[i];
            j = j + 1;
        }
    }
    return b;
}

template <type T>
T [[2]] filter(T [[2]] a, bool [[1]] bitmask){
    uint m0 = shape(a)[0];
    uint m1 = shape(a)[1];
    uint n0 = sum((uint)bitmask);
    T [[2]] b (n0,m1);
    uint j = 0;
    for (uint i = 0; i < m0; i++){
        if (bitmask[i]){
            for (uint k = 0; k < m1; k++){
                b[j,k] = a[i,k];
            }
            j = j + 1;
        }
    }
    return b;
}

template <domain D, type T>
D T [[1]] filter(D T [[1]] a, bool [[1]] bitmask){
    uint m = size(bitmask);
    uint n = sum((uint)bitmask);
    uint [[1]] src = iota(m);
    src = filter(src, bitmask);
    uint [[1]] tgt = iota(n);
    D T [[1]] b (n);
    return partialRearrange(a, b, src, tgt);
}

template <domain D, type T>
D T [[2]] filter(D T [[2]] a, bool [[1]] bitmask){
    uint m0 = shape(a)[0];
    uint m1 = shape(a)[1];
    uint n0 = sum((uint)bitmask);
    uint [[1]] src = iota(m0);
    src = widen_indices(filter(src, bitmask), m1);
    uint [[1]] tgt = iota(n0*m1);
    D T [[2]] b (n0,m1);
    return partialRearrange(a, b, src, tgt);
}

template <domain D, type T, type S>
relColumn<D,T,S> filter(relColumn<D,T,S> a, bool [[1]] bitmask){
    a.val = filter(a.val, bitmask);
    a.str = filter(a.str, bitmask);
    return a;
}

template <type T>
T [[2]] filterIndices(T [[2]] a, bool [[1]] bitmask){
    uint m0 = shape(a)[0];
    uint m1 = shape(a)[1];
    uint n1 = sum((uint)bitmask);
    T [[2]] b (m0,n1);

    for (uint k = 0; k < m0; k++){
        uint j = 0;
        for (uint i = 0; i < m1; i++){
            if (bitmask[i]){
                b[k,j] = a[k,i];
                j = j + 1;
            }
        }
    }
    return b;
}

//joins two tables by indices
//assuming that the first table rows are indexed 0...m, and the second table is a multiset of 0 ... m
// e.g. join of {{0,1,2,3,4}} and {1,2,2,4,4,4} is {{1,2,2,4,4,4},{0,1,2,3,4,5}}
//      join of {{1,2,2,4,4,4},{0,1,2,3,4,5}} and {0,0,3,5} is {{1,1,4,4},{0,0,3,5},{0,1,2,3}}
template <type T>
T [[2]] mergeTableIndices (T [[2]] iss, T [[1]] is){

    uint n = shape(iss)[0];
    uint m = shape(is)[0];

    //we are left with as many rows as the new constraint sets
    T [[2]] new_iss (n+1, m);

    //update the first table
    uint [[1]] tgt = iota(m);
    for (uint i = 0; i < n; i++){
        new_iss[0,:] = partialRearrange(iss[0,:], new_iss[0,:], is, tgt);
    }
    //add a new column that corresponds to the second table
    new_iss[n,:] = tgt;
    return new_iss;

}

//clones table indices to represent a non-deterministic choice
//the "chosen" variable is assigned the last index
template <type T>
T [[2]] cloneTableIndices (T [[2]] iss, int _k){

    uint k = (uint)_k;

    uint m = shape(iss)[0];
    uint n = shape(iss)[1];

    uint [[2]] new_iss (m+1, k*n);

    for(uint i = 0; i < k; i++){
        new_iss[0:m,i*n:(i+1)*n] = iss;
    }
    new_iss[m,:] = iota(k*n);

    return new_iss;

}

//measures dimensionality of indices
uint dims(uint [[2]] is){
    return shape(is)[0];
}

//replicates each variable in a block n times
//ms - differences between block starting points (steps)
//ns - times to replicate each variable in the corresponding block
template <type T>
T [[1]] myReplicate(T [[1]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));
    if (size(a) == 0) return a;

    T [[1]] b (sum(ms * ns));
    uint t = 0;
    uint s = 0;

    for (uint k = 0; k < size(ms); k++){
        uint m = ms[k];
        uint n = ns[k];
        for (uint i = 0; i < m; i++){
           for (uint j = 0; j < n; j++){
               b[t] = a[s + i];
               t = t + 1;
           }
       }
       s = s + m;
    }
    return b;
}

template <domain D, type T>
D T [[1]] myReplicate(D T [[1]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));

    uint [[1]] indices = iota(size(a));
    uint [[1]] source = myReplicate(indices, ms, ns);
    uint [[1]] target = iota(size(source));
    D T [[1]] b (size(source));
    b = partialRearrange(a, b, source, target);
    return b;
}

template <type T>
T [[2]] myReplicate(T [[2]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));

    T [[2]] b (sum(ms * ns), shape(a)[1]);
    uint t = 0;
    uint s = 0;

    for (uint k = 0; k < size(ms); k++){
        uint m = ms[k];
        uint n = ns[k];
        for (uint i = 0; i < m; i++){
           for (uint j = 0; j < n; j++){
               b[t,:] = a[s + i,:];
               t = t + 1;
           }
       }
       s = s + m;
    }
    return b;
}

template <domain D, type T>
D T [[2]] myReplicate(D T [[2]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));

    uint [[1]] indices = iota(shape(a)[0]);

    uint [[1]] source = widen_indices(myReplicate(indices, ms, ns), shape(a)[1]);
    uint [[1]] target = iota(size(source));
    D T [[2]] b (sum(ms * ns), shape(a)[1]);
    b = partialRearrange(a, b, source, target);
    return b;
}

//copies an entire block n times
//ms - differences between block starting points (steps)
//ns - times to copy the corresponding block
template <type T>
T [[1]] copyBlock(T [[1]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));
    if (size(a) == 0) return a;

    T [[1]] b (sum(ms * ns));
    uint32 [[1]] source (size(b));
    uint32 [[1]] target (size(b));

    uint l = 0;
    uint start = 0;

    for (uint k = 0; k < size(ms); k++){
        uint m = ms[k];
        uint n = ns[k];
        for (uint j = 0; j < n; j++){
            for (uint i = 0; i < m; i++){
               b[l] = a[start + i];
               l = l + 1;
           }
       }
       start = start + m;
    }
    return b;
}

template <domain D, type T>
D T [[1]] copyBlock(D T [[1]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));

    uint [[1]] indices = iota(size(a));
    uint [[1]] source = copyBlock(indices, ms, ns);
    uint [[1]] target = iota(size(source));
    D T  [[1]] b (sum(ms * ns));
    return partialRearrange(a, b, source, target);
}

template <type T>
T [[2]] copyBlock(T [[2]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));

    T [[2]] b (sum(ms * ns), shape(a)[1]);
    uint32 [[1]] source (size(b));
    uint32 [[1]] target (size(b));

    uint l = 0;
    uint start = 0;

    for (uint k = 0; k < size(ms); k++){
        uint m = ms[k];
        uint n = ns[k];
        for (uint j = 0; j < n; j++){
            for (uint i = 0; i < m; i++){
               b[l,:] = a[start + i,:];
               l = l + 1;
           }
       }
       start = start + m;
    }
    return b;
}

template <domain D, type T>
D T [[2]] copyBlock(D T [[2]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));

    uint [[1]]indices = iota(shape(a)[0]);
    uint [[1]] source = widen_indices(copyBlock(indices, ms, ns), shape(a)[1]);
    uint [[1]] target = iota(size(source));
    D T [[2]] b (sum(ms * ns), shape(a)[1]);
    return partialRearrange(a, b, source, target);
}

//equivalent of stdlib 'flatten'
template <domain D : shared3p, type T, dim N >
D T[[1]] myFlatten (D T[[N]] X){
    D T[[1]] Y (size(X));
    uint [[1]] indices = iota(size(X));
    Y = partialRearrange(X,Y,indices,indices);
    return Y;
}

//reverse an array (more efficient than stdlib version for public data)
template <type T>
T[[1]] myReverse (T[[1]] X){
    uint m = size(X);
    T [[1]] Y (m);
    for (uint i = 0; i < m; i++){
        Y[m - i - 1] = X[i];
    }
    return Y;
}

template <domain D, type T>
D T[[1]] myReverse (D T[[1]] X){
    uint [[1]] indices = iota(size(X));
    D T [[1]] Y (size(X));
    return partialRearrange(X,Y,indices,myReverse(indices));
}

//reverse several arrays in parallel
template <type T>
T[[2]] myReversePar (T[[2]] X){
    uint m = shape(X)[0];
    uint n = shape(X)[1];
    uint [[2]] Y (m, n);
    for (uint i = 0; i < m; i++){
        Y[i,:] = myReverse(X[i,:]);
    }
    return Y;
}

template <domain D, type T>
D T[[2]] myReversePar (D T[[2]] X){
    uint m = shape(X)[0];
    uint n = shape(X)[1];
    uint [[1]] source = iota(m * n); 
    uint [[1]] target = flatten(myReversePar(reshape(source,m,n)));
    D T [[2]] Y (m, n);
    Y = partialRearrange(X,Y,source,target);
    return Y;
}

//inverse permutation
template <type T>
T [[1]] inversePermutation (T [[1]] permutation){
    T [[1]] permutation_inverse(size(permutation));
    for (uint i = 0; i < size(permutation); i++){
        permutation_inverse[(uint)permutation[i]] = (T)i;
    }
    return permutation_inverse;
}

//apply public permutation
template <type T>
T [[1]] applyPermutation(T [[1]] A, uint [[1]] pi){
    assert(shape(A)[0] == size(pi));

    uint m = shape(pi)[0];
    T [[1]] B (m);
    for (uint i = 0; i < m; ++i) {
         B[i] = A[pi[i]];
    }
    return B;
}

template <type T>
T [[2]] applyPermutation(T [[2]] A, uint [[1]] pi){
    assert(shape(A)[0] == size(pi));

    uint m = shape(pi)[0];
    uint n = shape(A)[1];
    T [[2]] B (m, n);
    for (uint i = 0; i < m; ++i) {
        for (uint j = 0; j < n; ++j) {
            B[i,j] = A[pi[i],j];
        }
    }
    return B;
}

//apply public permutation to private data
template <domain D, type T>
D T [[1]] applyPermutation(D T [[1]] a, uint [[1]] pi){
    assert(size(a) == size(pi));

    uint n = shape(pi)[0];
    uint [[1]] source = pi;
    uint [[1]] target = iota(n);
    D T[[1]] b (n);
    b = partialRearrange(a, b, source, target);
    return b;
}

template <domain D, type T>
D T [[2]] applyPermutation(D T [[2]] A, uint [[1]] pi){
    assert(shape(A)[0] == size(pi));

    uint m = shape(pi)[0];
    uint n = shape(A)[1];
    uint [[1]] source (m * n);
    uint [[1]] target = iota(m * n);
    for (uint i = 0; i < m; ++i) {
        for (uint j = 0; j < n; ++j) {
            source[i * n + j] = pi[i] * shape(A)[1] + j;
        }
    }
    D T[[2]] B (m,n);
    B = partialRearrange(A, B, source, target);
    return B;
}

//apply private permutation
template <domain D0, domain D, type T, type U>
D T [[1]] applyPermutation (D0 T [[1]] a, D U [[1]] pi){
    assert(size(a) == size(pi));

    D uint8 [[1]] key(32);
    key = randomize(key);

    uint [[1]] tau = (uint)declassify(shuffle(pi, key));
    D T [[1]] a0 = a;
    a0 = applyPermutation(a0, tau);
    return inverseShuffle(a0, key);
}

template <domain D0, domain D, type T, type U>
D T [[2]] applyPermutation(D0 T [[2]] A, D U [[1]] pi){
    assert(shape(A)[0] == size(pi));

    D uint8 [[1]] key(32);
    key = randomize(key);

    uint [[1]] tau = (uint)declassify(shuffle(pi, key));
    D T [[2]] A0 = A;
    A0 = applyPermutation(A0, tau);
    return inverseShuffleRows(A0, key);
}

//unapply public permutation
template <domain D, type T>
D T [[1]] unapplyPermutation(D T [[1]] a, uint [[1]] pi){
    assert(size(a) == size(pi));

    uint n = shape(pi)[0];
    uint [[1]] source = iota(n);
    uint [[1]] target = pi;
    D T[[1]] b (n);
    b = partialRearrange(a, b, source, target);
    return b;
}

template <domain D, type T>
D T [[2]] unapplyPermutation(D T [[2]] A, uint [[1]] pi){
    assert(shape(A)[0] == size(pi));

    uint m = shape(pi)[0];
    uint n = shape(A)[1];
    uint [[1]] source = iota(m * n);
    uint [[1]] target (m * n);
    for (uint i = 0; i < m; ++i) {
        for (uint j = 0; j < n; ++j) {
            target[i * n + j] = pi[i] * n + j;
        }
    }
    D T[[2]] B (m,n);
    B = partialRearrange(A, B, source, target);
    return B;
}

//unapply private permutation
template <domain D, type T, type U>
D T [[1]] unapplyPermutation (D T [[1]] a, D U [[1]] pi){
    assert(size(a) == size(pi));

    D uint8 [[1]] key(32);
    key = randomize(key);

    a = shuffle(a, key);                                                 
    uint [[1]] tau = (uint)declassify(shuffle(pi, key));
    return applyPermutation(a, inversePermutation(tau));
}

template <domain D, type T, type U>
D T [[2]] unapplyPermutation(D T [[2]] A, D U [[1]] pi){
    assert(shape(A)[0] == size(pi));

    D uint8 [[1]] key(32);
    key = randomize(key);

    A = shuffleRows(A, key);                                                 
    uint [[1]] tau = (uint)declassify(shuffle(pi, key));
    return applyPermutation(A, inversePermutation(tau));
}

//lifting applyPermutation to column structure
template <domain D, type T, type S, type U>
relColumn<D,T,S> applyPermutation (relColumn<D,T,S> a, D U [[1]] pi){
    relColumn<D,T,S> b;
    b.tableRef  = a.tableRef;
    b.str = applyPermutation(a.str, pi);
    b.val = applyPermutation(a.val, pi);
    return b;
}

template <domain D, type T, type S, type U>
relColumn<D,T,S> applyPermutation (relColumn<D,T,S> a, U [[1]] pi){
    relColumn<D,T,S> b;
    b.tableRef = a.tableRef;
    b.str = applyPermutation(b.str, pi);
    b.val = applyPermutation(b.val, pi);
    return b;
}

template <domain D, type T, type S, type U>
relColumn<D,T,S> applyPermutation (relColumn<public,T,S> a, D U [[1]] pi){
    relColumn<D,T,S> b;
    b.tableRef = a.tableRef;
    b.str = applyPermutation(b.str, pi);
    b.val = applyPermutation(b.val, pi);
    return b;
}


//equivalent of stdlib 'cat'
template <type T>
T[[1]] myCat(T[[1]] X, T[[1]] Y) {
    return cat(X, Y);
}

template <type T>
T[[2]] myCat(T[[2]] X, T[[2]] Y) {
    return cat(X, Y, 0);
}

template <domain D: shared3p, type T>
D T[[1]] myCat(D T[[1]] X, D T[[1]] Y) {
    //if (size(X) > 0 && size(Y) > 0){
        D T [[1]] Z (size(X) + size(Y));
        uint32 [[1]] indices_X (size(X));
        uint32 [[1]] indices_Y (size(Y));
        for (uint i = 0; i < size(X); i++){
            indices_X[i] = (uint32)i;
        }
        for (uint i = 0; i < size(Y); i++){
            indices_Y[i] = (uint32)i;
        }
        Z = partialRearrange(X,Z,indices_X,indices_X);
        Z = partialRearrange(Y,Z,indices_Y,indices_Y + (uint32)size(X));
        return Z;
    //}else if (size(X) > 0){
    //    return X;
    //}else{
    //    return Y;
    //}
}

template <domain D: shared3p, type T>
D T[[2]] myCat(D T[[2]] X, D T[[2]] Y, int d) {
    //if (size(X) > 0 && size(Y) > 0){
        D T [[2]] Z;
        uint offset;
        if (d == 0){
            assert(shape(X)[1] == shape(Y)[1]);
            offset = shape(X)[1];
            D T [[2]] Z_aux (shape(X)[0] + shape(Y)[0], shape(X)[1]);
            Z = Z_aux;
        }else{
            assert(shape(X)[0] == shape(Y)[0]);
            offset = shape(X)[1] + shape(Y)[1];
            D T [[2]] Z_aux (shape(X)[0], shape(X)[1] + shape(Y)[1]);
            Z = Z_aux;
        }
        uint32 [[1]] source_X (size(X));
        uint32 [[1]] source_Y (size(Y));
        uint32 [[1]] target_X (size(X));
        uint32 [[1]] target_Y (size(Y));
        for (uint i = 0; i < shape(X)[0]; i++){
           for (uint j = 0; j < shape(X)[1]; j++){
               source_X[i * shape(X)[1] + j] = (uint32)(i * shape(X)[1] + j);
               target_X[i * shape(X)[1] + j] = (uint32)(i * offset + j);
           }
        }
        for (uint i = 0; i < shape(Y)[0]; i++){
           for (uint j = 0; j < shape(Y)[1]; j++){
               source_Y[i * shape(Y)[1] + j] = (uint32)(i * shape(Y)[1] + j);
               target_Y[i * shape(Y)[1] + j] = (uint32)(i * offset + j);
           }
        }
        Z = partialRearrange(X,Z,source_X,target_X);
        uint32 [[1]] offset_Y (size(Y));
        if (d == 0){
            offset_Y = (uint32)size(X);
        }else{
            offset_Y = (uint32)shape(X)[1];
        }
        Z = partialRearrange(Y,Z,source_Y,target_Y + offset_Y);
        return Z;
    //}else if (size(X) > 0){
    //    return X;
    //}else{
    //    return Y;
    //}
}

template <domain D: shared3p, type T>
D T[[2]] myCat(D T[[2]] X, D T[[2]] Y) {
    return myCat(X, Y, 0);
}

uint [[1]] _slice_indices(uint m, uint n, uint lb1, uint ub1, uint lb2, uint ub2, uint lb3, uint ub3) {
    assert(lb2 <= m);
    assert(lb3 <= n);
    assert(ub2 <= m);
    assert(ub3 <= n);
    uint [[3]] indices = reshape(iota(ub1 * m * n), ub1, m, n);
    return flatten(indices[lb1 : ub1, lb2 : ub2, lb3 : ub3]);
}

uint [[1]] _slice_indices(uint m, uint lb1, uint ub1, uint lb2, uint ub2) {
    assert(lb2 <= m);
    assert(ub2 <= m);
    uint [[2]] indices = reshape(iota(ub1 * m), ub1, m);
    return flatten(indices[lb1 : ub1, lb2 : ub2]);
}

template<domain D, type T, type T1, type T2, type T3, type T4 >
D T[[2]] mySlice(D T[[2]] X, T1 _lb1, T2 _ub1, T3 _lb2, T4 _ub2) {

    uint lb1 = (uint)(_lb1); uint ub1 = (uint)(_ub1);
    uint lb2 = (uint)(_lb2); uint ub2 = (uint)(_ub2);

    D T[[2]] Y (ub1 -  lb1, ub2 - lb2);
    uint [[1]] source = _slice_indices(shape(X)[1], lb1, ub1, lb2, ub2);
    uint [[1]] target = iota(size(Y));
    Y = partialRearrange(X,Y,source,target);
    return Y;
}

template<domain D, type T, type T1, type T2 >
D T[[2]] mySlice(D T[[2]] X, T1 _lb1, T2 _ub1) {

    uint lb1 = (uint)(_lb1); uint ub1 = (uint)(_ub1);
    uint lb2 = 0;            uint ub2 = shape(X)[1];

    D T[[2]] Y (ub1 -  lb1, ub2 - lb2);
    uint [[1]] source = _slice_indices(shape(X)[1], lb1, ub1, lb2, ub2);
    uint [[1]] target = iota(size(Y));
    Y = partialRearrange(X,Y,source,target);
    return Y;
}

template<type T, type T1, type T2 >
T[[1]] mySlice(T[[1]] X, T1 _lb, T2 _ub) {
    uint lb = (uint)(_lb); uint ub = (uint)(_ub);
    return X[lb:ub];
}

template<domain D, type T, type T1, type T2 >
D T[[1]] mySlice(D T[[1]] X, T1 _lb, T2 _ub) {

    uint lb = (uint)(_lb); uint ub = (uint)(_ub);

    D T[[1]] Y (ub - lb);
    uint [[1]] source = iota(ub - lb) + lb;
    uint [[1]] target = iota(size(Y));
    Y = partialRearrange(X,Y,source,target);
    return Y;
}

//equivalent of slicing assignments X[lb1:ub1, lb2:ub2, lb3:ub3] = .....
template<type T, type T1, type T2, type T3, type T4 >
T[[2]] mySetSlice(T[[2]] X, T[[2]] Y, T1 _lb1, T2 _ub1, T3 _lb2, T4 _ub2) {

    uint lb1 = (uint)(_lb1); uint ub1 = (uint)(_ub1);
    uint lb2 = (uint)(_lb2); uint ub2 = (uint)(_ub2);

    Y[lb1:ub1, lb2:ub2] = X;
    return Y;
}

template<domain D, type T, type T1, type T2, type T3, type T4 >
D T[[2]] mySetSlice(D T[[2]] X, D T[[2]] Y, T1 _lb1, T2 _ub1, T3 _lb2, T4 _ub2) {

    uint lb1 = (uint)(_lb1); uint ub1 = (uint)(_ub1);
    uint lb2 = (uint)(_lb2); uint ub2 = (uint)(_ub2);

    uint [[1]] source = iota(size(X));
    uint [[1]] target = _slice_indices(shape(Y)[1], lb1, ub1, lb2, ub2);
    Y = partialRearrange(X,Y,source,target);
    return Y;
}

//equivalent of stdlib 'reshape'
template <type T, type T1, dim N>
T[[1]] myReshape (T[[N]] X, T1 _m){
    uint m = (uint)(_m);
    assert(size(X) == m);
    return reshape(X, m);
}

template <domain D, type T, type T1, dim N>
D T[[1]] myReshape (D T[[N]] X, T1 _m){
    uint m = (uint)(_m);
    assert(size(X) == m);
    return partialReshape(X, m);
}

template <type T, type T1, type T2, dim N>
T[[2]] myReshape (T[[N]] X, T1 _m, T2 _n){
    uint m = (uint)(_m); uint n = (uint)(_n);
    assert(size(X) == m * n);
    return reshape(X, m, n);
}

template <domain D, type T, type T1, type T2, dim N>
D T[[2]] myReshape (D T[[N]] X, T1 _m, T2 _n){
    uint m = (uint)(_m); uint n = (uint)(_n);
    assert(size(X) == m * n);
    return partialReshape(X, m, n);
}

//a weaker variant of stdlib 'reshape', which allows non-exact fitting
template <domain D, type T, type T1, dim N>
D T[[1]] partialReshape (D T[[N]] X, T1 _m){
    uint m = (uint)(_m);
    D T[[1]] Y (m);
    uint [[1]] indices = iota(m);
    Y = partialRearrange(X,Y,indices,indices);
    return Y;
}

template <domain D, type T, type T1, type T2, dim N>
D T[[2]] partialReshape (D T[[N]] X, T1 _m, T2 _n){
    uint m = (uint)(_m); uint n = (uint)(_n);
    D T[[2]] Y (m,n);
    uint [[1]] indices = iota(m * n);
    Y = partialRearrange(X,Y,indices,indices);
    return Y;
}

//equivalent of .... = X[i,:] and .... = X[:,i]
// projects to the coordinate i of dimension k
template<domain D, type T, type T1, type T2 >
D T[[1]] myGetProj(D T[[2]] X, T1 _i, T2 _k) {

    uint i = (uint)_i;
    uint k = (uint)_k;
    assert(k < 2);

    uint lb1 = 0; uint ub1 = shape(X)[0];
    uint lb2 = 0; uint ub2 = shape(X)[1];
    uint m = 0;

    if (k == 0) {
        m = (ub2 - lb2);
        lb1 = i; ub1 = i + 1;
    } else {
        m = (ub1 - lb1);
        lb2 = i; ub2 = i + 1;
    }

    D T [[2]] Z = mySlice(X, lb1, ub1, lb2, ub2);
    D T [[1]] Y = myReshape(Z,m);
    return Y;
}

// turns [x1,x2,...,xn] to [x1, x1+x2, ... , x1+...+xn]
// defined for additive sharing only
template <domain D : shared3p, type T>
D T [[1]] prefixSum(D T [[1]] src) {
    if( size(src) <= 1 )
        return src;
    uint halfSize = size(src) / 2;
    uint [[1]] gatherEven = 2 * iota(halfSize);
    uint [[1]] gatherOdd = gatherEven + 1;
    D T [[1]] srcEven(halfSize), srcOdd(halfSize);
    __syscall("shared3p::gather_$T\_vec", __domainid(D), src, srcEven, __cref gatherEven);
    __syscall("shared3p::gather_$T\_vec", __domainid(D), src, srcOdd, __cref gatherOdd);
    D T [[1]] prefOdd = prefixSum(srcEven + srcOdd);
    D T [[1]] srcFstElem(1); srcFstElem[0] = src[0];
    D T [[1]] prefEven = myCat(srcFstElem, mySlice(prefOdd, (uint)0, halfSize - 1) + mySlice(srcEven, (uint)1, halfSize));
    D T [[1]] res(size(src));
    __syscall("shared3p::scatter_$T\_vec", __domainid(D), prefEven, res, __cref gatherEven);
    __syscall("shared3p::scatter_$T\_vec", __domainid(D), prefOdd, res, __cref gatherOdd);
    if(size(src) > 2 * halfSize)
        res[2 * halfSize] = prefOdd[halfSize - 1] + src[2 * halfSize];
    return res;
}

// turns each row of the input matrix [x1,x2,...,xn] to [x1, x1+x2, ... , x1+...+xn]
// defined for additive sharing only
template <domain D : shared3p, type T>
D T [[2]] prefixSumPar(D T [[2]] src) {
    uint m = shape(src)[0];
    uint n = shape(src)[1];
    if(n <= 1)
        return src;
    uint halfSize = n / 2;

    uint [[1]] gatherEven (m * halfSize);
    for (uint i = 0; i < m; i++){
        gatherEven[halfSize*i : halfSize*(i+1)] = n * i + 2 * iota(halfSize);
    }

    uint [[1]] gatherOdd = gatherEven + 1;
    D T [[2]] srcEven(m, halfSize), srcOdd(m, halfSize);
    __syscall("shared3p::gather_$T\_vec", __domainid(D), src, srcEven, __cref gatherEven);
    __syscall("shared3p::gather_$T\_vec", __domainid(D), src, srcOdd, __cref gatherOdd);
    D T [[2]] prefOdd = prefixSumPar(srcEven + srcOdd);

    D T [[2]] srcFstElem(m,1); srcFstElem = mySetSlice(mySlice(src,0,m,0,1), srcFstElem, 0,m,0,1);

    D T [[2]] prefEven = myCat(srcFstElem, mySlice(prefOdd, 0 :: uint, m, 0 :: uint, halfSize - 1) + mySlice(srcEven, 0 :: uint, m, 1 :: uint, halfSize), 1);
    D T [[2]] res(m,n);

    __syscall("shared3p::scatter_$T\_vec", __domainid(D), prefEven, res, __cref gatherEven);
    __syscall("shared3p::scatter_$T\_vec", __domainid(D), prefOdd,  res, __cref gatherOdd);
    if(n > 2 * halfSize)
        res = mySetSlice(mySlice(prefOdd,0,m,halfSize - 1,halfSize) + mySlice(src,0,m,2 * halfSize,2 * halfSize + 1), res, 0, m, 2 * halfSize, 2 * halfSize + 1);
    return res;
}

//sorting permutation
template <domain D, type T>
D uint32 [[1]] quickSortPermutation (D T[[1]] X) {

    uint64 [[1]] blocks = {0 :: uint64, size(X)};
    uint64 [[1]] sigma  = iota(size(X));
    D uint [[1]] pi     = sigma;

    D xor_uint64 [[1]] dummy = sigma;

    D uint8 [[1]] key(32);
    key = randomize(key);

    X     = shuffle(X,key);
    pi    = shuffle(pi,key);
    dummy = shuffle(dummy,key);

    bool ascend = true;
    __syscall("shared3p::stable_sort_$T\_vec", __domainid(D), X, dummy, ascend, __cref blocks, __ref sigma);

    pi = applyPermutation(pi, sigma);
    return (uint32)pi;
}

//lifting quicksort to columns
template <domain D, type T, type S>
D uint32 [[1]] quickSortPermutation (relColumn<D,T,S> col) {
    return quickSortPermutation(col.val);
}

template <type T>
T [[1]] inversePermutation (T [[1]] permutation){
    T [[1]] permutation_inverse(size(permutation));
    for (uint i = 0; i < size(permutation); i++){
        permutation_inverse[(uint)permutation[i]] = (T)i;
    }
    return permutation_inverse;
}

//sorting permutation of public data
//template <type T>
//uint32 [[1]] quickSortPermutation(T[[1]] vec) {
//    uint64 [[1]] sigma(size(vec));
//    __syscall("shared3p::public_sort_$T\_vec",  __domainid (pd_shared3p), __ref sigma, __cref vec);
//    return (uint32)(permutation_inverse(sigma));
//}

//a workaround to use before getting a newer instance of Sharemind
// TODO it is interesting to check whether we have public sort on the cluster now
template <type T>
uint32 [[1]] quickSortPermutation(T[[1]] vec) {
    pd_shared3p T [[1]] prvec = vec;
    pd_shared3p uint32 [[1]] sigma = quickSortPermutation(prvec);
    return declassify(sigma);
}

// shuffle
template <domain D, type T>
D T [[2]] shufflePar (D T [[2]] vectors, D uint8 [[1]] key){

    uint m = shape(vectors)[0];
    uint n = shape(vectors)[1];

    uint [[1]] indices = iota(m) * n;
    uint [[1]] counts (m); counts = n;

    D T [[1]] data = myFlatten(vectors);
    data = shuffleBlocks(data, indices, counts, key);
    vectors = myReshape(data, m, n);
    return vectors;
}

template <domain D, type T>
D T [[1]] shuffleBlocks (D T [[1]] data, uint [[1]] indices, uint [[1]] counts, D uint8 [[1]] key){

    uint m = size(indices);
    uint l = size(data);

    uint [[1]] tags = iota(m);
    uint [[1]] steps (m); steps = 1;

    uint [[1]] publicIndices = myReplicate(tags,    steps, counts);
    uint [[1]] publicOffsets = myReplicate(indices, steps, counts);

    D uint [[1]] privateIndices = publicIndices;
    D uint [[1]] privateOffsets = publicOffsets;

    if (!inverse) data = shuffle(data, key);
    publicIndices = (uint)declassify(shuffle(privateIndices, key));
    publicOffsets = (uint)declassify(shuffle(privateOffsets, key));

    uint [[1]] permutedIndices (l);
    uint [[1]] counters (m);
    for (uint i = 0; i < l; i++){
        permutedIndices[i] = publicOffsets[i] + counters[publicIndices[i]];
        counters[publicIndices[i]]++;
    }
    return unapplyPermutation(data, permutedIndices); 
}

// functions related to key-value count sort
template <domain D, type T, type S>
D S [[1]] _countIndices (D T [[1]] key) {

    D S[[1]] sortCol = (S) key;
    D S[[1]] isortCol = 1 - sortCol;

    uint n = shape(key)[0];
    {
        D S[[1]] pTrue = prefixSum(isortCol) - isortCol;
        isortCol *= pTrue;
    }

    {
        D S[[1]] reverseSortCol = -myReverse(sortCol);
        D S[[1]] pFalse = myReverse(prefixSum(reverseSortCol) - reverseSortCol) + (S)(n - 1);
        sortCol *= pFalse;
    }
    return (sortCol + isortCol);
}

template <domain D, type T, type S>
D S[[2]] _countIndicesPar(D T [[2]] key) {

    D S[[2]] sortCol = (S) key;
    D S[[2]] isortCol = 1 - sortCol;

    uint m = shape(key)[0];
    uint n = shape(key)[1];
    {
        D S[[2]] pTrue = prefixSumPar(isortCol) - isortCol;
        isortCol *= pTrue;
    }

    {
        D S[[2]] reverseSortCol = -myReversePar(sortCol);
        D S[[2]] pFalse = myReversePar(prefixSumPar(reverseSortCol) - reverseSortCol) + (S)(n - 1);
        sortCol *= pFalse;
    }

    return sortCol + isortCol;
}

template <type T>
uint32 [[1]] countSortPermutation(T [[1]] key) {

    assert(4294967296 >= size(key));
    uint n = shape(key)[0];

    uint32 [[1]] b = (uint32)key;
    uint32 [[1]] pi (n);
    uint32 zeroes = 0;
    uint32 ones = 0;

    for (uint i = 0; i < n; i++){
        if (key[i] == (T)0) {
            pi[i] = zeroes;           
            zeroes = zeroes + 1;
        } else {
            pi[i] = ones;
            ones = ones + 1;
        }
    }
    pi = pi + b * zeroes;
    return pi;
}

template <domain D, type T>
D uint32 [[1]] countSortPermutation(D T [[1]] key) {

    assert(4294967296 >= size(key));

    uint n = shape(key)[0];

    //we start from the identity permutation
    uint32 [[1]] ident = (uint32)iota(n);
    D uint32 [[1]] pi = ident;

    D uint32 [[1]] indices = _countIndices(key);

    D uint8 [[1]] shuffle_key (32);
    shuffle_key = randomize(shuffle_key);

    indices = shuffle(indices, shuffle_key);
    pi      = shuffle(pi,      shuffle_key);

    uint[[1]] publishedIndices = (uint)declassify(indices);
    pi                         = unapplyPermutation(pi, publishedIndices);

    return pi;
}



template<domain D, type T1, type T2, dim N>
T2 [[N]] declassifyIfNeed(D T1 [[N]] x){
    return declassify(x);
}

template<domain D, type T, dim N>
D T [[N]] declassifyIfNeed(D T [[N]] x){
    return x;
}


template<type T, dim N>
T [[N]] choose(bool [[N]] b, T [[N]] x, T [[N]] y){
    T [[N]] bn = (T)b;
    return bn * x + (1 - bn) * y;
}

template<domain D, dim N>
D int32 [[N]] choose(bool [[N]] b, D int32 [[N]] x, D int32 [[N]] y){
    int32 [[N]] bn = (int32)b;
    return bn * x + (1 - bn) * y;
}
template<domain D, dim N>
D int32 [[N]] choose(bool [[N]] b, int32 [[N]] x, D int32 [[N]] y){
    int32 [[N]] bn = (int32)b;
    return bn * x + (1 - bn) * y;
}
template<domain D, dim N>
D int32 [[N]] choose(bool [[N]] b, D int32 [[N]] x, int32 [[N]] y){
    int32 [[N]] bn = (int32)b;
    return bn * x + (1 - bn) * y;
}


template<domain D, type T, dim N>
D T [[N]] choose(D bool [[N]] b, T [[N]] x, T [[N]] y){
    D T [[N]] bn = (T)b;
    return bn * x + (1 - bn) * y;
}

template<domain D, dim N>
D int32 [[N]] choose(D bool [[N]] b, D int32 [[N]] x, D int32 [[N]] y){
    D int32 [[N]] bn = (int32)b;
    return bn * x + (1 - bn) * y;
}
template<domain D, dim N>
D int32 [[N]] choose(D bool [[N]] b, int32 [[N]] x, D int32 [[N]] y){
    int32 [[N]] bn = (int32)b;
    return bn * x + (1 - bn) * y;
}
template<domain D, dim N>
D int32 [[N]] choose(D bool [[N]] b, D int32 [[N]] x, int32 [[N]] y){
    int32 [[N]] bn = (int32)b;
    return bn * x + (1 - bn) * y;
}


template<domain D, dim N>
D xor_uint32 [[N]] choose(bool [[N]] b, D xor_uint32 [[N]] x, D xor_uint32 [[N]] y){
    uint32 [[N]] bn = 4294967295 * (uint32)b;
    return bn & x ^ (4294967295 - bn) & y;
}
template<domain D, dim N>
D xor_uint32 [[N]] choose(bool [[N]] b, uint32 [[N]] x, D xor_uint32 [[N]] y){
    uint32 [[N]] bn = 4294967295 * (uint32)b;
    return bn & x ^ (4294967295 - bn) & y;
}
template<domain D, dim N>
D xor_uint32 [[N]] choose(bool [[N]] b, D xor_uint32 [[N]] x, uint32 [[N]] y){
    uint32 [[N]] bn = 4294967295 * (uint32)b;
    return bn & x ^ (4294967295 - bn) & y;
}


template<domain D, dim N>
D xor_uint8 [[N]] choose(bool [[N]] b, D xor_uint8 [[N]] x, D xor_uint8 [[N]] y){
    uint8 [[N]] bn = 255 * (uint8)b;
    return bn & x ^ (255 - bn) & y;
}
template<domain D, dim N>
D xor_uint8 [[N]] choose(bool [[N]] b, uint8 [[N]] x, D xor_uint8 [[N]] y){
    uint8 [[N]] bn = 255 * (uint8)b;
    return bn & x ^ (255 - bn) & y;
}
template<domain D, dim N>
D xor_uint8 [[N]] choose(bool [[N]] b, D xor_uint8 [[N]] x, uint8 [[N]] y){
    uint8 [[N]] bn = 255 * (uint8)b;
    return bn & x ^ (255 - bn) & y;
}

/*
template<domain D, type T, dim N>
D T [[N]] choose(bool [[N]] b, T [[N]] x, D T [[N]] y){
    T [[N]] bn = (T)b;
    return bn * x + (1 - bn) * y;
}

template<domain D, type T, dim N>
D T [[N]] choose(bool [[N]] b, D T [[N]] x, D T [[N]] y){
    T [[N]] bn = (T)b;
    return bn * x + (1 - bn) * y;
}
*/

/*
template<domain D>
D int32 [[1]] getIntColumn(string ds, string tableName, uint colIndex, uint m, uint mi, uint ni){
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    D int32 [[1]] col = tdbReadColumn(ds, tableName, colIndex);
    col = copyBlock(myReplicate(col, ms, ns), {mi * ni}, {m / (mi * ni)});
    return col;
}
*/

uint tableRowCount(string ds, string tableName, uint limit){
    //print("reportedRowCount:",min(limit, tdbGetRowCount(ds, tableName)));
    return min(limit, tdbGetRowCount(ds, tableName));
}

template<domain D>
D bool [[1]] extendColumn(D bool [[1]] x, uint m, uint mi, uint ni){
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;
    return copyBlock(myReplicate(x,  ms, ns), {mi * ni}, {m / (mi * ni)});
}

uint [[1]] extendColumn(uint [[1]] x, uint m, uint mi, uint ni){
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;
    return copyBlock(myReplicate(x,  ms, ns), {mi * ni}, {m / (mi * ni)});
}

template<domain D, type T, type S>
relColumn<D, T, S> extendColumn(relColumn<D, T, S> x, uint m, uint mi, uint ni){
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    public relColumn<D, T, S> result;
    result.tableRef = x.tableRef;
    result.val = copyBlock(myReplicate(x.val, ms, ns), {mi * ni}, {m / (mi * ni)});
    result.str = copyBlock(myReplicate(x.str, ms, ns), {mi * ni}, {m / (mi * ni)});
    return result;

}

template<domain D, type T, type T0>
relColumn<D, T, T> getDBColumn(string ds, string tableName, T0 _colIndex, uint m, uint mi, uint ni, T0 _tableIndex){
    uint colIndex = (uint)_colIndex;
    uint tableIndex = (uint)_tableIndex;

    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    D T [[1]] col = tdbReadColumn(ds, tableName, colIndex);
    col = mySlice(col,0,min(size(col),mi));

    col = copyBlock(myReplicate(col, ms, ns), {mi * ni}, {m / (mi * ni)});

    public relColumn<D, T, T> result;

    result.tableRef = tableIndex;
    result.val = col;
    result.str = reshape((T)0, m, 0);
    return result;
}

relColumn<pd_shared3p, xor_uint32, xor_uint8> getDBColumn(string ds, string tableName, int64 _colIndex, uint m, uint mi, uint ni, int64 _tableIndex){
    uint colIndex = (uint)_colIndex;
    uint tableIndex = (uint)_tableIndex;
    uint rv;
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    pd_shared3p xor_uint32 [[1]] col     = reshape(0,mi);
    pd_shared3p xor_uint8 [[2]] col_str = reshape(0,mi,0);
    rv = tdbReadColumn(ds, tableName, colIndex);
    for (uint i = 0; i < mi; i++){
        pd_shared3p xor_uint8 [[1]] temp = tdbVmapGetVlenValue(rv, "values", i);

        col[i] = CRC32(temp);
        uint n = size(temp);

        pd_shared3p xor_uint8 [[2]] temp2 = reshape(0, shape(col_str)[0], max(shape(col_str)[1], n));
        temp2 = mySetSlice(col_str, temp2, 0, shape(col_str)[0], 0, shape(col_str)[1]);
        pd_shared3p xor_uint8 [[1]] temp3 = declassifyIfNeed(temp);
        temp2 = mySetSlice(myReshape(temp3, 1, n), temp2, i, i+1, 0, n);
        col_str = temp2;
    }
    col     = copyBlock(myReplicate(col, ms, ns), {mi * ni}, {m / (mi * ni)});
    col_str = copyBlock(myReplicate(col_str, ms, ns), {mi * ni}, {m / (mi * ni)});

    public relColumn<pd_shared3p, xor_uint32, xor_uint8> result;

    result.tableRef = tableIndex;
    result.val = col;
    result.str = col_str;
    return result;
}

template<type T>
relColumn<public, uint32, uint8> getDBColumn(string ds, string tableName, T _colIndex, uint m, uint mi, uint ni, T _tableIndex){

    uint colIndex = (uint)_colIndex;
    uint tableIndex = (uint)_tableIndex;
    uint rv;
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    public uint32 [[1]] col     = reshape(0,mi);
    public uint8 [[2]] col_str = reshape(0,mi,0);
    rv = tdbReadColumn(ds, tableName, colIndex);
    for (uint i = 0; i < mi; i++){
        //TODO we hope to find a better solution for public columns (need a public CRC32 function)
        uint8 [[1]] _temp = tdbVmapGetVlenValue(rv, "values", i);
        pd_shared3p xor_uint8 [[1]] temp = _temp;

        col[i] = declassify(CRC32(temp));
        uint n = size(temp);

        public uint8 [[2]] temp2 = reshape(0, shape(col_str)[0], max(shape(col_str)[1], n));
        temp2 = mySetSlice(col_str, temp2, 0, shape(col_str)[0], 0, shape(col_str)[1]);
        public uint8 [[1]] temp3 = declassifyIfNeed(temp);
        temp2 = mySetSlice(myReshape(temp3, 1, n), temp2, i, i+1, 0, n);
        col_str = temp2;
    }
    col     = copyBlock(myReplicate(col, ms, ns), {mi * ni}, {m / (mi * ni)});
    col_str = copyBlock(myReplicate(col_str, ms, ns), {mi * ni}, {m / (mi * ni)});

    public relColumn<public, uint32, uint8> result;

    result.tableRef = tableIndex;
    result.val = col;
    result.str = col_str;
    return result;
}

template<domain D, type T, type S, type T0>
relColumn<D, T, S> getDBColumn(string ds, string tableName, T0 colIndex, uint mi, T0 tableIndex){
    return getDBColumn(ds, tableName, colIndex, mi, mi, 1 :: uint, tableIndex);
}

uint [[1]] getExtensionIndices(uint m, uint mi, uint ni){

    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    uint [[1]] col = iota(mi);
    return copyBlock(myReplicate(col, ms, ns), {mi * ni}, {m / (mi * ni)});
}

// write public data to secret-shared database (create table step)
// this is meant for testing only
// colDataType:
// - 0: bool
// - 1: int32
// - 2: float32
// - 3: string
template<type T>
void createTable(string ds, string tableName, T [[1]] _colDataType, T [[1]] _colDomain, string headers, T [[1]] _ns) {

    uint [[1]] colDataType = (uint)_colDataType;
    uint [[1]] colDomain   = (uint)_colDomain;
    uint [[1]] ns = (uint)_ns;

    pd_shared3p bool      private_bool;
    pd_shared3p int32     private_int32;
    pd_shared3p float32   private_float32;
    pd_shared3p xor_uint8 private_string;

    bool    public_bool;
    int32   public_int32;
    float32 public_float32;
    uint8   public_string;

    uint params;

    if (tdbTableExists(ds, tableName)) {
        //print("Deleting existing table: " + tableName);
        tdbTableDelete(ds, tableName);
    }
    //print("Start creating the new table: " + tableName);

    params = tdbVmapNew();
    uint n = size(ns);
    uint offset = 0;
    for (uint j = 0; j < n; j++){
        if (colDataType[j] == 0){
            if (colDomain[j] == 0){
                tdbVmapAddType(params, "types", public_bool);
            }else{
                tdbVmapAddType(params, "types", private_bool);
            }
        }else if (colDataType[j] == 1){
            if (colDomain[j] == 0){
                tdbVmapAddType(params, "types", public_int32);
            }else{
                tdbVmapAddType(params, "types", private_int32);
            }
        }else if (colDataType[j] == 2){
            if (colDomain[j] == 0){
                tdbVmapAddType(params, "types", public_float32);
            }else{
                tdbVmapAddType(params, "types", private_float32);
            }
        }else{
            if (colDomain[j] == 0){
                tdbVmapAddVlenType(params, "types", public_string);
            }else{
                tdbVmapAddVlenType(params, "types", private_string);
            }
        }
        tdbVmapAddString(params, "names", substring(headers, offset, offset + ns[j]));
        offset = offset + ns[j];
    }
    tdbTableCreate(ds, tableName, params);
    tdbVmapDelete(params);
}

// write public data to secret-shared database (write a batch of data step)
// this is meant for testing only
template<type T, type T0, type T1, type T2>
void writePublicToTable(string ds, string tableName, T [[1]] _colDataType, T [[1]] _colDomain,
                        T0 [[1]] _boolData,
                        T1 [[1]] _intData,
                        T2 [[1]] _floatData,
                        string strData, T [[1]] _ms) {

    uint [[1]] colDataType = (uint)_colDataType;
    uint [[1]] colDomain   = (uint)_colDomain;
    uint [[1]] ms = (uint)_ms;
    bool    [[1]] boolData  = (bool)_boolData;
    int32   [[1]] intData   = (int32)_intData;
    float32 [[1]] floatData = (float32)_floatData;

    uint params;

    params = tdbVmapNew();
    uint strCnt = 0;
    uint intCnt = 0;
    uint boolCnt = 0;
    uint floatCnt = 0;

    //uint m = size(ms);
    uint n = size(colDataType);
    uint offset = 0;
    while (true){
        for (uint j = 0; j < n; j++){
            if (colDataType[j] == 0){
                if (colDomain[j] == 0){
                  bool temp = boolData[boolCnt];
                  tdbVmapAddValue(params, "values", temp);
                }else{
                  pd_shared3p bool temp = boolData[boolCnt];
                  tdbVmapAddValue(params, "values", temp);
                }
                boolCnt++;
            }else if (colDataType[j] == 1){
                if (colDomain[j] == 0){
                  int32 temp = intData[intCnt];
                  tdbVmapAddValue(params, "values", temp);
                }else{
                  pd_shared3p int32 temp = intData[intCnt];
                  tdbVmapAddValue(params, "values", temp);
                }
                intCnt++;
            }else if (colDataType[j] == 2){
                if (colDomain[j] == 0){
                  float32 temp = floatData[floatCnt];
                  tdbVmapAddValue(params, "values", temp);
                }else{
                  pd_shared3p float32 temp = floatData[floatCnt];
                  tdbVmapAddValue(params, "values", temp);
                }
                floatCnt++;
            } else {
                if (colDomain[j] == 0){
                  uint8 [[1]] temp = __bytes_from_string(substring(strData, offset, offset + ms[strCnt]));
                  tdbVmapAddVlenValue(params, "values", temp);
                }else{
                  pd_shared3p xor_uint8 [[1]] temp = __bytes_from_string(substring(strData, offset, offset + ms[strCnt]));
                  tdbVmapAddVlenValue(params, "values", temp);
                }
                offset = offset + ms[strCnt];
                strCnt++;
            }
        }
        tdbInsertRow(ds,tableName, params);
        tdbVmapClear(params);
        if (strCnt == size(ms) & floatCnt == size(floatData) & intCnt == size(intData) & boolCnt == size(boolData)) break;
    }
    //print("batch finished");
}

//creation of constant columns

//string column
//convert a string to uint32 hash

//public->public
relColumn<public, uint32, uint8> constStrColumn(string arg, uint m, uint n){
    pd_shared3p xor_uint8 [[1]] blstr = bl_str(arg);
    uint8 [[1]] str = declassify(blstr);
    uint32 val = declassify(CRC32(blstr));
    relColumn<public, uint32, uint8> result;
    result.val = reshape(val,m);
    result.str = reshape(copyBlock(str,{size(str)},{m}),m,size(str));

    result.tableRef = n-1;

    return result;
}

//private->private
relColumn<pd_shared3p, xor_uint32, xor_uint8> constStrColumn(pd_shared3p xor_uint8 [[1]] arg, uint m, uint n){
    pd_shared3p xor_uint32 val = CRC32(arg);
    relColumn<pd_shared3p, xor_uint32, xor_uint8> result;
    result.val = reshape(val,m);
    result.str = reshape(copyBlock(arg,{size(arg)},{m}),m,size(arg));

    result.tableRef = n-1;

    return result;
}

/*
//public->private
relColumn<pd_shared3p, xor_uint32, xor_uint8> constStrColumn(string arg, uint m, uint n){
    pd_shared3p xor_uint8 [[1]] str = bl_str(arg);
    pd_shared3p xor_uint32 val = CRC32(str);
    relColumn<pd_shared3p, xor_uint32, xor_uint8> result;
    result.val = reshape(val,m);
    result.str = reshape(copyBlock(str,{size(str)},{m}),m,size(str));

    result.tableRef = n-1;

    return result;
}
*/

//int column

//public->public
//private->private
template <domain D, type T>
relColumn<D, int32, int32> constIntColumn(D T arg0, uint m, uint n){
    D int32 arg = (int32)arg0;
    relColumn<D, int32, int32> result;
    result.val = reshape(arg,m);
    result.str = reshape(0,m,0);

    result.tableRef = n-1;

    return result;
}

//public->private
template <domain D, type T>
relColumn<D, int32, int32> constIntColumn(T arg0, uint m, uint n){
    D int32 arg = (int32)arg0;
    relColumn<D, int32, int32> result;
    result.val = reshape(arg,m);
    result.str = reshape(0,m,0);

    result.tableRef = n-1;

    return result;
}

//float column

//public->public
//private->private
template <domain D, type T>
relColumn<D, float32, float32> constFloatColumn(D T arg0, uint m, uint n){
    D float32 arg = (float32)arg0;
    relColumn<D, float32, float32> result;
    result.val = reshape(arg,m);
    result.str = reshape(0,m,0);

    result.tableRef = n-1;

    return result;
}

//public->private
template <domain D, type T>
relColumn<D, float32, float32> constFloatColumn(T arg0, uint m, uint n){
    D float32 arg = (float32)arg0;
    relColumn<D, float32, float32> result;
    result.val = reshape(arg,m);
    result.str = reshape(0,m,0);

    result.tableRef = n-1;

    return result;
}

//bool column

//public->public
//private->private
template <domain D, type T>
relColumn<D, bool, bool> constBoolColumn(D T arg0, uint m, uint n){
    D bool arg = (bool)arg0;
    relColumn<D, bool, bool> result;
    result.val = reshape(arg,m);
    result.str = reshape(false,m,0);

    result.tableRef = n-1;

    return result;
}

//public->private
template <domain D, type T>
relColumn<D, bool, bool> constBoolColumn(T arg0, uint m, uint n){
    D bool arg = (bool)arg0;
    relColumn<D, bool, bool> result;
    result.val = reshape(arg,m);
    result.str = reshape(false,m,0);

    result.tableRef = n-1;

    return result;
}

//dynamic type column (determined by the output type)

//public->public
//private->private
template <domain D, type T, type T0>
relColumn<D, T, T> constColumn(D T0 arg0, uint m, uint n){
    D T arg = (T)arg0;
    relColumn<D, T, T> result;
    result.val = reshape(arg,m);
    result.str = reshape(0,m,0);

    result.tableRef = n-1;

    return result;
}

relColumn<public, uint32, uint8> constColumn(string arg, uint m, uint n){
    return constStrColumn(arg, m, n);
}

relColumn<pd_shared3p, xor_uint32, xor_uint8> constColumn(pd_shared3p xor_uint8 [[1]] arg, uint m, uint n){
    return constStrColumn(arg, m, n);
}

//public->private
template <domain D, type T>
relColumn<D, T, T> constColumn(float32 arg0){
    D T arg = (T)arg0;
    relColumn<D, T, T> result;
    result.val = reshape(arg,1);
    result.str = reshape((T)0,1,0);
    result.tableRef = 0;

    return result;
}

template <domain D, type T, type T0>
relColumn<D, T, T> constColumn(T0 arg0){
    D T arg = (T)arg0;
    relColumn<D, T, T> result;
    result.val = reshape(arg,1);
    result.str = reshape((T)0,1,0);
    result.tableRef = 0;

    return result;
}

//relColumn<pd_shared3p, xor_uint32, xor_uint8> constColumn(string arg, uint m){
//    return constStrColumn(arg, m);
//}

//if column size is not specified, we assume that it is 1
template <domain D, type T0, type T, dim N>
T0 constColumn(D T [[N]] arg){
    return constColumn(arg, 1 :: uint, 1 :: uint);
}

template <domain D, type T0, type T, dim N>
T0 constColumn(D T [[N]] arg, uint m){
    return constColumn(arg, m, 1 :: uint);
}

//a column of booleans
//used for non-prolog internal SecreC booleans only
template<domain D>
D bool [[1]] trueColumn(uint m){
    return reshape(true,m);
}

template<domain D>
D bool [[1]] trueColumn(){
    return trueColumn(1 :: uint);
}

template<domain D, type T, type S>
relColumn<D, T, S> myCat(relColumn<D, T, S> x, relColumn<D, T, S> y){

    x.val = myCat(x.val, y.val);
    if (shape(x.str)[0] == 0){
        x.str = y.str;
    } else if (shape(y.str)[0] == 0) {
        x.str = x.str;
    } else {
        x.str = myCat(x.str, y.str);
    }
    return x;
}

template<domain D, type T0, type S0, type T1, type S1>
relColumn<D, T1, S1> myCat(relColumn<public, T0, S0> x, relColumn<D, T1, S1> y){
    D T1 [[1]] xval = x.val;
    D S1 [[2]] xstr = x.str;
    relColumn<D, T1, S1> z;

    z.tableRef = x.tableRef;

    z.val = myCat(xval, y.val);
    if (shape(x.str)[0] == 0){
        z.str = y.str;
    } else if (shape(y.str)[0] == 0) {
        z.str = xstr;
    } else {
        z.str = myCat(xstr, y.str);
    }
    return z;
}

template<domain D, type T0, type S0, type T1, type S1>
relColumn<D, T0, S0> myCat(relColumn<D, T0, S0> x, relColumn<public, T1, S1> y){
    D T0 [[1]] yval = y.val;
    D S0 [[2]] ystr = y.str;
    relColumn<D, T0, S0> z;

    z.tableRef = x.tableRef;

    z.val = myCat(x.val, yval);
    if (shape(x.str)[0] == 0){
        z.str = ystr;
    } else if (shape(y.str)[0] == 0) {
        z.str = x.str;
    } else {
        z.str = myCat(x.str, ystr);
    }
    return z;
}

template<type T, dim N>
pd_shared3p T [[N]] myCat(pd_shared3p T [[N]] x, public T [[N]] y){
    pd_shared3p T [[N]] y_pr = y;
    return myCat(x, y_pr);
}

template<type T, dim N>
pd_shared3p T [[N]] myCat(public T [[N]] x, pd_shared3p T [[N]] y){
    pd_shared3p T [[N]] x_pr = x;
    return myCat(x_pr, y);
}

template<domain D, type T, type S, domain D0, type T0, type S0, domain D1, type T1, type S1>
relColumn<D, T, S> myCat(relColumn<D0, T0, S0> x, relColumn<D1, T1, S1> y, uint index){
    relColumn<D, T, S> z = myCat(x,y);
    z.tableRef = index;
    return z;
}
//postprocessing
template<domain D>
D uint32 [[1]] lpShuffle(D bool [[1]] b){
    publish("does solution exist:", any(b));
    D uint32 [[1]] pi  = countSortPermutation(!b);
    return pi;
}

template<domain D>
D uint32 [[1]] lpShuffle(bool [[1]] b){
    publish("does solution exist:", any(b));
    uint32 [[1]] pi  = countSortPermutation(!b);
    D uint32 [[1]] pr_pi = pi;
    return pr_pi;
}

//postprocessing
template<domain D, type T, type S>
D bool [[1]] findRepeating(relColumn<public,T,S> col){
    D bool [[1]] eq0  = col.val[0:size(col.val)-1] == col.val[1:size(col.val)];
    D bool [[1]] eq   = cat(eq0, {false});
    return eq;
}

template<domain D, type T, type S>
D bool [[1]] findRepeating(relColumn<D,T,S> col){
    D bool [[1]] eq0  = (mySlice(col.val, 0, size(col.val)-1) == mySlice(col.val, 1, size(col.val)));
    D bool [[1]] eq   = myCat(eq0, {false});
    return eq;
}

/*
template<domain D0, domain D, type T, dim N>
D0 T [[N]] filterTrue(D0 uint32 [[1]] pi, uint32 n, D T [[N]] val){
    return mySlice(applyPermutation(val,pi), 0, n);
}

template<domain D, type T0, type T, type S>
void publishArg(T0 i0, string vname, D T [[1]] val, D S [[2]] str){
    uint i = (uint)i0;
    publish("var" + tostring(i), vname);
    if (shape(str)[1] >= 1){
        publish("len" + tostring(i), shape(str)[1]);
        publish("val" + tostring(i), str);
    } else {
        publish("val" + tostring(i), val);
    }
}
*/

template<domain D, type T, type S>
relColumn<D,T,S> filterTrue(D uint32 [[1]] pi, uint32 n, relColumn<D,T,S> col){
    relColumn<D,T,S> result;
    result.val = mySlice(applyPermutation(col.val,pi), 0, n);
    result.str = mySlice(applyPermutation(col.str,pi), 0, n);
    return result;
}

template<domain D, type T, type S>
relColumn<D,T,S> filterTrue(D uint32 [[1]] pi, uint32 n, relColumn<public,T,S> col){
    relColumn<D,T,S> result;
    result.val = mySlice(applyPermutation(col.val,pi), 0, n);
    result.str = mySlice(applyPermutation(col.str,pi), 0, n);
    return result;
}

template<domain D, type T, type S>
relColumn<D,T,S> filterTrue(uint32 [[1]] pi, uint32 n, relColumn<D,T,S> col){
    relColumn<D,T,S> result;
    result.val = mySlice(applyPermutation(col.val,pi), 0, n);
    result.str = mySlice(applyPermutation(col.str,pi), 0, n);
    return result;
}

template<domain D, type T0, type T, type S>
void publishCol(T0 i0, string vname, relColumn<D,T,S> col){
    uint i = (uint)i0;
    publish("var" + tostring(i), vname);
    if (shape(col.str)[1] >= 1){
        publish("len" + tostring(i), shape(col.str)[1]);
        publish("val" + tostring(i), col.str);
    } else {
        publish("val" + tostring(i), col.val);
    }
}

template<domain D, type T0, type T>
void publishVal(T0 i0, string vname, D T val){
    uint i = (uint)i0;
    publish("var" + tostring(i), vname);
    publish("val" + tostring(i), val);
}

//aggregations (as used in DES Datalog Educational System)

//count
template<domain D, domain D0, type T0, type S0>
D uint32 aggr_count(D bool [[1]] b, relColumn<D0, T0, S0> x){
    D uint32 [[1]] bn = (uint32)b;
    return sum(bn);
}

//sum
template<domain D, domain D0, type T0, type S0>
D0 T0 aggr_sum(D bool [[1]] b, relColumn<D0, T0, S0> x){
    D0 T0 [[1]] empty (size(b)); empty = 0;
    D0 T0 [[1]] filtered = choose(b, x.val, empty);
    return sum(filtered);
}

//min
template<domain D, domain D0, type T0, type S0>
D0 T0 aggr_min(D bool [[1]] b, relColumn<D0, T0, S0> x){
    D0 T0 [[1]] empty (size(b)); empty = 0;
    D0 T0 [[1]] filtered = choose(b, x.val, empty);
    return min(filtered);
}

//max
template<domain D, domain D0, type T0, type S0>
D0 T0 aggr_max(D bool [[1]] b, relColumn<D0, T0, S0> x){
    D0 T0 [[1]] empty (size(b)); empty = 0;
    D0 T0 [[1]] filtered = choose(b, x.val, empty);
    return max(filtered);
}

//times
template<domain D, domain D0, type T0, type S0>
D0 T0 aggr_times(D bool [[1]] b, relColumn<D0, T0, S0> x){
    D0 T0 [[1]] empty (size(b)); empty = 0;
    D0 T0 [[1]] filtered = choose(b, x.val, empty);
    return product(filtered);
}


//different blackbox operations lifted to relColumn
//produce error if we try to valuate a free variable

//boolean operations
template<domain D>
D bool [[1]] apply_bop(string s, uint32 [[1]] x, D xor_uint32 [[1]] y){
    D bool [[1]] b;
    //this gives an error for (x == y) for some type derivation reasons
    if (s == "==") b = (y == x);
    else assert(false);
    return b;
}

template<domain D>
D bool [[1]] apply_bop(string s, D xor_uint32 [[1]] x, uint32 [[1]] y){
    D bool [[1]] b;
    if (s == "==") b = (x == y);
    else assert(false);
    return b;
}

template<domain D, domain D0, domain D1>
D bool [[1]] apply_bop(string s, D0 bool [[1]] x, D1 bool [[1]] y){
    D bool [[1]] b;
    if (s == "==") b = (y == x);
    if (s == "&")  b = (y & x);
    if (s == "|")  b = (y | x);
    else assert(false);
    return b;
}

template<domain D, domain D0, domain D1>
D bool [[1]] apply_bop(string s, D0 int32 [[1]] x, D1 float32 [[1]] y){
    D bool [[1]] b;
    if      (s == "==") b = ((float32)x == y);
    else if (s == "<=") b = ((float32)x <= y);
    else if (s == "<")  b = ((float32)x <  y);
    else if (s == ">=") b = ((float32)x >= y);
    else if (s == ">")  b = ((float32)x >  y);
    else assert(false);
    return b;
}

template<domain D, domain D0, domain D1>
D bool [[1]] apply_bop(string s, D0 float32 [[1]] x, D1 int32 [[1]] y){
    D bool [[1]] b;
    if      (s == "==") b = (x == (float32)y);
    else if (s == "<=") b = (x <= (float32)y);
    else if (s == "<")  b = (x <  (float32)y);
    else if (s == ">=") b = (x >= (float32)y);
    else if (s == ">")  b = (x >  (float32)y);
    else assert(false);
    return b;
}

template<domain D, domain D0, domain D1, type T>
D bool [[1]] apply_bop(string s, D0 T [[1]] x, D1 T [[1]] y){
    D bool [[1]] b;
    if      (s == "==") b = (x == y);
    else if (s == "<=") b = (x <= y);
    else if (s == "<")  b = (x <  y);
    else if (s == ">=") b = (x >= y);
    else if (s == ">")  b = (x >  y);
    else assert(false);
    return b;
}

//TODO the following block is going to be deprecated
template<type T1, type T2, type S1, type S2>
pd_shared3p bool [[1]] bop(string s, relColumn<pd_shared3p, T1, S1> x, relColumn<public, T2, S2> y){
    return apply_bop(s, x.val, y.val);
}

template<type T1, type T2, type S1, type S2>
pd_shared3p bool [[1]] bop(string s, relColumn<public, T1, S1> x, relColumn<pd_shared3p, T2, S2> y){
    return apply_bop(s, x.val, y.val);
}

template<domain D, type T1, type T2, type S1, type S2>
D bool [[1]] bop(string s, relColumn<D, T1, S1> x, relColumn<D, T2, S2> y){
    return apply_bop(s, x.val, y.val);
}

//boolean operation on data
template<type T1, type T2, type S1, type S2>
pd_shared3p bool [[1]] bop(string s, relColumn<pd_shared3p, T1, S1> x, relColumn<public, T2, S2> y, uint [[2]] is){
    return _bop(s, x, y, is);
}

template<type T1, type T2, type S1, type S2>
pd_shared3p bool [[1]] bop(string s, relColumn<public, T1, S1> x, relColumn<pd_shared3p, T2, S2> y, uint [[2]] is){
    return _bop(s, x, y, is);
}

template<domain D, type T1, type T2, type S1, type S2>
D bool [[1]] bop(string s, relColumn<D, T1, S1> x, relColumn<D, T2, S2> y, uint [[2]] is){
    return _bop(s, x, y, is);
}

template<domain D, domain D1, domain D2, type T1, type T2, type S1, type S2>
D bool [[1]] _bop(string s, relColumn<D1, T1, S1> x, relColumn<D2, T2, S2> y, uint [[2]] is){

    uint m = shape(is)[1];

    uint [[1]] tgt = iota(m);
    uint [[1]] srcx (m);
    uint [[1]] srcy (m);

    D1 T1 [[1]] x0 (m); x0 = partialRearrange(x.val, x0, is[x.tableRef,:], tgt);
    D2 T2 [[1]] y0 (m); y0 = partialRearrange(y.val, y0, is[y.tableRef,:], tgt);
    D bool [[1]] z = apply_bop(s, x0, y0);

    return z;

}


//integer division that rounds the result down to the nearest integer
template<domain D, domain D0, domain D1>
D int32 [[1]] div(D0 int32 [[1]] x, D1 int32 [[1]] y){
    D0 bool [[1]] xpos = (x >= 0);
    D1 bool [[1]] ypos = (y >= 0);
    D0 uint32 [[1]] ux = (uint32)x * (2*(uint32)xpos - 1);
    D1 uint32 [[1]] uy = (uint32)y * (2*(uint32)ypos - 1);
    D bool [[1]] exactDiv = (ux % uy == 0);
    D bool [[1]] diffSign = (xpos ^ ypos);
    D uint32 [[1]] uz = ux / uy;
    D int32 [[1]] z = (int32)uz + 1 - (int32)(exactDiv | !diffSign);
    return (int32)z * (1 - 2*(int32)diffSign);
}

template<domain D>
D float32 [[1]] div(D float32 [[1]] x, float32 [[1]] y){
   D float32 [[1]] dy = y;
   return x / dy;
}

template<domain D, domain D0, domain D1>
D float32 [[1]] div(D0 float32 [[1]] x, D1 float32 [[1]] y){
   return x / y;
}

//power operation
template<type T, type T0>
pd_shared3p T [[1]] power(pd_shared3p T [[1]] x, pd_shared3p T0 [[1]] y){
   pd_shared3p float32 [[1]] pry = (float32)y;
   return (T)pow((float32)x, pry);
}

template<type T, type T0>
T [[1]] power(public T [[1]] x, public T0 [[1]] y){
   pd_shared3p float32 [[1]] prx = (float32)x;
   pd_shared3p float32 [[1]] pry = (float32)y;
   return (T)declassify(pow(prx, pry));
}

template<type T, type T0>
pd_shared3p T [[1]] power(pd_shared3p T [[1]] x, public T0 [[1]] y){
   pd_shared3p float32 [[1]] pry = (float32)y;
   return (T)pow((float32)x, pry);
}



//arithmetic operations
template<domain D, domain D0, domain D1, type T>
D T [[1]] apply_aop(string s, D0 T [[1]] x, D1 T [[1]] y){
    D T [[1]] z;
    if      (s == "+") z = x + y;
    else if (s == "-") z = x - y;
    else if (s == "*") z = x * y;
    else if (s == "/") z = div(x, y);
    else if (s == "pow") z = power(x,y);
    //TODO can we do it more efficiently if one argument is public?
    else if (s == "min"){
        D T [[1]] xpr = x;
        D T [[1]] ypr = y;
        z = min(xpr, ypr);
    }
    else if (s == "max"){
        D T [[1]] xpr = x;
        D T [[1]] ypr = y;
        z = max(xpr, ypr);
    }
    else assert(false);
    return z;
}

//--
template<domain D, type T, type S>
relColumn<D, float32, float32> cast_float32(relColumn<D, T, S> x){

    relColumn<D, float32, float32> y;
    y.tableRef = x.tableRef;
    y.val = (float32)x.val;
    y.str = (float32)x.str;
    return y;
}

//---
template<domain D, type T>
relColumn<D, float32, float32> apply_sqrt(relColumn<D, T, T> x){
    relColumn<D, float32, float32> z;
    z.tableRef = x.tableRef;
    z.val = sqrt((float32)x.val);
    z.str = reshape(0,shape(x.str)[0], shape(x.str)[1]);
    return z;
}

//join of aop domains
// TODO add indices to all aop-s
template<type T, type S>
relColumn<pd_shared3p, T, S> aop(string s, relColumn<pd_shared3p, T, S> x, relColumn<public, T, S> y, uint [[2]] is){

    uint m = shape(is)[1];

    uint [[1]] tgt = iota(m);
    uint [[1]] srcx (m);
    uint [[1]] srcy (m);

    pd_shared3p T [[1]] x0 (m); x0 = partialRearrange(x.val, x0, is[x.tableRef,:], tgt);
    public T [[1]] y0 (m);      y0 = partialRearrange(y.val, y0, is[y.tableRef,:], tgt);

    relColumn<pd_shared3p, T, S> z;

    z.tableRef = shape(is)[0] - 1;

    z.val = apply_aop(s, x0, y0);
    z.str = reshape((S)0, shape(z.val)[0], shape(x.str)[1]);
    return z;
}

template<type T, type S>
relColumn<pd_shared3p, T, S> aop(string s, relColumn<public, T, S> x, relColumn<pd_shared3p, T, S> y, uint [[2]] is){

    uint m = shape(is)[1];

    uint [[1]] tgt = iota(m);
    uint [[1]] srcx (m);
    uint [[1]] srcy (m);

    public T [[1]] x0 (m);      x0 = partialRearrange(x.val, x0, is[x.tableRef,:], tgt);
    pd_shared3p T [[1]] y0 (m); y0 = partialRearrange(y.val, y0, is[y.tableRef,:], tgt);

    relColumn<pd_shared3p, T, S> z;

    z.tableRef = shape(is)[0] - 1;

    z.val = apply_aop(s, x.val, y.val);
    z.str = reshape((S)0,shape(z.val)[0], shape(x.str)[1]);
    return z;
}

template<domain D, type T, type S>
relColumn<D, T, S> aop(string s, relColumn<D, T, S> x, relColumn<D, T, S> y, uint [[2]] is){

    uint m = shape(is)[1];

    uint [[1]] tgt = iota(m);
    uint [[1]] srcx (m);
    uint [[1]] srcy (m);

    D T [[1]] x0 (m); x0 = partialRearrange(x.val, x0, is[x.tableRef,:], tgt);
    D T [[1]] y0 (m); y0 = partialRearrange(y.val, y0, is[y.tableRef,:], tgt);

    relColumn<D, T, S> z;

    //TODO this should map to the last column
    z.tableRef = 0;

    z.val = apply_aop(s, x.val, y.val);
    z.str = reshape((S)0,shape(z.val)[0], shape(x.str)[1]);
    return z;
}


//aggregations
template<domain D0, domain D, type T, type S>
relColumn<D0, int32, int32> count_filter(relColumn<D, T, S> x, D0 bool [[1]] b, uint m){
    uint n = size(b) / m;
    assert(m*n == size(b));
    D0 int32 [[1]] bu = (int32)b;

    relColumn<D, int32, int32> y;

    y.tableRef = x.tableRef;
    y.val = sum(bu,n);
    y.str = reshape(0,n,0);

    return y;
}

template<domain D0, domain D, type T, type S>
relColumn<D0, T, S> sum_filter(relColumn<D, T, S> x, D0 bool [[1]] b, uint m){
    uint n = size(b) / m;
    assert(m*n == size(b));
    D0 int32 [[1]] bu = (int32)b;

    relColumn<D, T, S> y;
    y.tableRef = x.tableRef;
    y.val = sum(x.val * bu, n);
    y.str = reshape((S)0,n,0);

    return y;
}

template<domain D0, domain D, type T, type S>
relColumn<D0, float32, float32> avg_filter(relColumn<D, T, S> x, D0 bool [[1]] b, uint m){
    uint n = size(b) / m;
    assert(m*n == size(b));
    D0 int32 [[1]] bu = (int32)b;

    relColumn<D, float32, float32> y;
    y.tableRef = x.tableRef;
    y.val = (float32)sum(x.val * bu, n) / (float32)sum(bu,n);
    y.str = reshape(0,n,0);

    return y;
}

int32 maxValue(int32 x){
  return INT32_MAX;
}
int32 minValue(int32 x){
  return INT32_MIN;
}

//TODO find better bounds for floats
float32 maxValue(float32 x){
  return (float32)(INT32_MAX);
}
float32 minValue(float32 x){
  return (float32)(INT32_MIN);
}

template<domain D0, domain D, type T, type S>
relColumn<D0, T, S> min_filter(relColumn<D, T, S> x, D0 bool [[1]] b, uint m){
    uint n = size(b) / m;
    assert(m*n == size(b));
    D0 T [[1]] bu = (T)b;

    T dummy;
    T ff = maxValue(dummy);
    relColumn<D, T, S> y;
    y.tableRef = x.tableRef;
    y.val = min(bu * (x.val - maxValue(dummy)) + maxValue(dummy), n);
    y.str = reshape((S)0,n,0);

    return y;
}

template<domain D0, domain D, type T, type S>
relColumn<D0, T, S> max_filter(relColumn<D, T, S> x, D0 bool [[1]] b, uint m){
    uint n = size(b) / m;
    assert(m*n == size(b));
    D0 int32 [[1]] bu = (int32)b;

    T dummy;
    relColumn<D, T, S> y;
    y.tableRef = x.tableRef;
    y.val = max(bu * (x.val - minValue(dummy)) + minValue(dummy), n);
    y.str = reshape((S)0,n,0);

    return y;
}

template<domain D0, domain D, type T, type S>
relColumn<D0, T, S> times_filter(relColumn<D, T, S> x, D0 bool [[1]] b, uint m){
    uint n = size(b) / m;
    assert(m*n == size(b));
    D0 int32 [[1]] bu = (int32)b;

    relColumn<D, T, S> y;
    y.tableRef = x.tableRef;
    y.val = product(bu * (x.val - 1) + 1, n);
    y.str = reshape((S)0,n,0);

    return y;
}

