module lp_essentials;

import stdlib;
import shared3p;

//the core rearrangement function
template <domain D, type T, type S, dim M, dim N>
D T [[N]] partialRearrange(D T [[M]] a, D T [[N]] b, S [[1]] source, S [[1]] target){
    assert(size(source) == size(target));
    D T [[1]] temp (size(source));
    __syscall("shared3p::gather_$T\_vec",  __domainid(D), a, temp, __cref (uint)source);
    __syscall("shared3p::scatter_$T\_vec", __domainid(D), temp, b, __cref (uint)target);
    return b;
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
