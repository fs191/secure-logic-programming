%Casting uint64 <-> xor_uint64
:-type(db(@x : primary private xor_uint64, @y : public uint64)).

p(X, Y) :-
  RX = reshare(X),
  db(RX, Y).

:-outputs([Y]).
?-p(3 : public uint64, Y).
