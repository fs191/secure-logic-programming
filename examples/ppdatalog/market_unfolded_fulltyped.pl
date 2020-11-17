:-type(sells(@seller:private string, @item_seller:private string, @price_seller:private int)).
:-type(buys(@buyer :private string, @item_buyer:private string, @price_buyer:private int)).

% combining domain(x) and domain(y) into domain(f(x,y)):
% public & private -> private
% public & unknown -> unknown
% private & unknown -> private

%the types of input variables (X0) are taken from the goal
%the types of output (X1) and free (X2) variables will be determined inside the rule body
%the truth type of the rule is private since at least one of its lines has private truth type
bargain(X0 : private string,X1 : private string,X2 : private string)  : private bool :-

  %reading from the database always succeeds and returns "public true"
  %there are also implicit unifications "X0 = buys.buyer", "X2 = buys.item", and "X3 = buys.price"
  %we only take into account types of those unifications for which "Xi has already been declared before",
  % i.e. Xi is one of the following:
  % - an input variable of the goal: X0, which is private
  % - a variable that has alrady been used before in the rule: none in this case
  % unification for X0 is private
  buys(X0 : private string, X2 : private string, X3 : private int)  : private bool,

  % we now need to take into account the following unifications:
  % - an input variable of the goal: X1, which is private
  % - a variable that has alrady been used before in the rule: X2, valuated in the previous line to private
  % unifications for X1 and X2 are private
  sells(X1 : private string, X2 : private string, X4 : private int) : private bool,
  (X3 : private int >= X4 : private int)                            : private bool.

% the truth type is private since it is private for bargain rule
% the type of 'x' is private as was assigned by the user
% the type of Y is private string, matched against 2nd arg of the bargain rule
% the type of Z is private string, matched against 3rd arg of the bargain rule
:-inputs([@x : private string]).
:-outputs([Y : private string]).
?-bargain(@x : private string, Y : private string, Z : private string) : private bool.
