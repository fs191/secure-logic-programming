%Casting uint64 <-> xor_uint64
:-type(db(@x : private xor_uint64, @y : public uint64)).

p(X) :-
  RX = reshare(X),
  RY = reshare(Y),
  db(X, Y),
  db(RY, RX).

?-p(X).
