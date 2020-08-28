zeroSum(AliceBefore,AliceAfter,BobBefore,BobAfter) :-
    AliceBefore + AliceAfter = BobBefore + BobAfter.
% This example demonstrates how NOT to use Prolog/Datalog (as it may behave not as expected).
% What SWI-Prolog thinks about this rule:
% A + B = C + 3 and similar cases inevitably end up in "unknown type", so we need to assume that the goal contains more inputs to get reasonable output
% A + 3 = 5 + D unifies to A = 5, D = 3 (intuitively)
% A + B = 5 + 3 unifies to A = 5, B = 3 (not so intuitively, as there actually exist more arithmetic solutions)
% 4 + 4 = 5 + 3 fails (quite non-intuitively, but '=' is an unification)
% 4 + 4 =:= 5 + 3 succeeds (intuitively), since =:= evaluates both sides and compares them
% A + 2 =:= 5 + 3 gives an error since LHS contains free variables
% A + 'b' = 'c' + D unifies to A = 'c', D = 'b' (although evaluating '+' on strings would cause type error, this is fine as far as no '=:=' is applied)
% 'c' + 'b' =:= 'c' + 'b' fails (trying to evaluate '+' on strings)

:-inputs([@x1,@y1]).
:-outputs([X2,Y2]).
?-zeroSum(@x1, X2, @y1, Y2).
