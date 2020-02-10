module lp_essentials;

import stdlib;
import shared3p;
import shared3p_random;
import shared3p_string;

import shared3p_table_database;
import table_database;

//the core rearrangement function
template <domain D, type T, type S, dim M, dim N>
D T [[N]] partialRearrange(D T [[M]] a, D T [[N]] b, S [[1]] source, S [[1]] target){
    assert(size(source) == size(target));
    D T [[1]] temp (size(source));
    __syscall("shared3p::gather_$T\_vec",  __domainid(D), a, temp, __cref (uint)source);
    __syscall("shared3p::scatter_$T\_vec", __domainid(D), temp, b, __cref (uint)target);
    return b;
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

//replicates each variable in a block n times
//ms - differences between block starting points (steps)
//ns - times to replicate each variable in the corresponding block
template <type T>
T [[1]] myReplicate(T [[1]] a, uint [[1]] ms, uint [[1]] ns){
    assert(size(ms) == size(ns));

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
template <domain D, type T, type U>
D T [[1]] applyPermutation (D T [[1]] a, D U [[1]] pi){
    assert(size(a) == size(pi));

    D uint8 [[1]] key(32);
    key = randomize(key);

    uint [[1]] tau = (uint)declassify(shuffle(pi, key));
    a = applyPermutation(a, tau);
    return inverseShuffle(a, key);
}

template <domain D, type T, type U>
D T [[2]] applyPermutation(D T [[2]] A, D U [[1]] pi){
    assert(shape(A)[0] == size(pi));

    D uint8 [[1]] key(32);
    key = randomize(key);

    uint [[1]] tau = (uint)declassify(shuffle(pi, key));
    A = applyPermutation(A, tau);
    return inverseShuffleRows(A, key);
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

//equivalent of stdlib 'cat'
template <domain D: shared3p, type T>
D T[[1]] myCat(D T[[1]] X, D T[[1]] Y) {
    if (size(X) > 0 && size(Y) > 0){
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
    }else if (size(X) > 0){
        return X;
    }else{
        return Y;
    }
}

template <domain D: shared3p, type T>
D T[[2]] myCat(D T[[2]] X, D T[[2]] Y, int d) {
    if (size(X) > 0 && size(Y) > 0){
        D T [[2]] Z;
        uint offset;
        if (d == 0){
            offset = shape(X)[1];
            D T [[2]] Z_aux (shape(X)[0] + shape(Y)[0], shape(X)[1]);
            Z = Z_aux;
        }else{
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
    }else if (size(X) > 0){
        return X;
    }else{
        return Y;
    }
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


//TODO we hope to merge these two structs and functions into one
template<domain D>
struct strColumnPublic{
    uint32 [[1]] col;
    uint8 [[2]] col_str;
}

template<domain D>
struct strColumnPrivate{
    D xor_uint32 [[1]] col;
    D xor_uint8 [[2]] col_str;
}

template<domain D, type T1, type T2, dim N>
T2 [[N]] declassifyIfNeed(D T1 [[N]] x){
    return declassify(x);
}

template<domain D, type T, dim N>
D T [[N]] declassifyIfNeed(D T [[N]] x){
    return x;
}

template<domain D>
D int32 [[1]] getIntColumn(string ds, string tableName, uint colIndex, uint m, uint mi, uint ni){
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    D int32 [[1]] col = tdbReadColumn(ds, tableName, colIndex);
    col = copyBlock(myReplicate(col, ms, ns), {mi * ni}, {m / (mi * ni)});
    return col;
}

template<domain D>
strColumnPublic<D> getStrColumn(string ds, string tableName, uint colIndex, uint m, uint mi, uint ni){
    uint rv;
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    uint32 [[1]] col     = reshape(0,mi);
    uint8  [[2]] col_str = reshape(0,mi,0);
    rv = tdbReadColumn(ds, tableName, colIndex);
    for (uint i = 0; i < mi; i++){
        D xor_uint8 [[1]] temp = tdbVmapGetVlenValue(rv, "values", i);
        col[i] = declassifyIfNeed(CRC32(temp));
        uint n = size(temp);

        uint8 [[2]] temp2 = reshape(0, shape(col_str)[0], max(shape(col_str)[1], n));
        temp2 = mySetSlice(col_str, temp2, 0, shape(col_str)[0], 0, shape(col_str)[1]);
        uint8 [[1]] temp3 = declassifyIfNeed(temp);
        temp2 = mySetSlice(myReshape(temp3, 1, n), temp2, i, i+1, 0, n);
        col_str = temp2;
    }
    col     = copyBlock(myReplicate(col, ms, ns), {mi * ni}, {m / (mi * ni)});
    col_str = copyBlock(myReplicate(col_str, ms, ns), {mi * ni}, {m / (mi * ni)});

    public strColumnPublic<D> result;
    result.col     = col;
    result.col_str = col_str;
    return result;
}

template<domain D>
strColumnPrivate<D> getStrColumn(string ds, string tableName, uint colIndex, uint m, uint mi, uint ni){
    uint rv;
    uint [[1]] ms (mi); ms = 1;
    uint [[1]] ns (mi); ns = ni;

    D xor_uint32 [[1]] col     = reshape(0,mi);
    D xor_uint8  [[2]] col_str = reshape(0,mi,0);
    rv = tdbReadColumn(ds, tableName, colIndex);
    for (uint i = 0; i < mi; i++){
        D xor_uint8 [[1]] temp = tdbVmapGetVlenValue(rv, "values", i);
        col[i] = declassifyIfNeed(CRC32(temp));
        uint n = size(temp);

        D xor_uint8 [[2]] temp2 = reshape(0, shape(col_str)[0], max(shape(col_str)[1], n));
        temp2 = mySetSlice(col_str, temp2, 0, shape(col_str)[0], 0, shape(col_str)[1]);
        D xor_uint8 [[1]] temp3 = declassifyIfNeed(temp);
        temp2 = mySetSlice(myReshape(temp3, 1, n), temp2, i, i+1, 0, n);
        col_str = temp2;
    }
    col     = copyBlock(myReplicate(col, ms, ns), {mi * ni}, {m / (mi * ni)});
    col_str = copyBlock(myReplicate(col_str, ms, ns), {mi * ni}, {m / (mi * ni)});

    public strColumnPrivate<D> result;
    result.col     = col;
    result.col_str = col_str;
    return result;
}

