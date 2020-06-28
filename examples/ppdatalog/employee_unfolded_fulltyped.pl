:-type(eds(employee:public string,department:public string, salary:private int)).
:-type(dm(department:public string, manager:public string)).

% combining domain(x) and domain(y) into domain(f(x,y)):
% public & private -> private
% public & unknown -> unknown
% private & unknown -> private


%the types of input variables (X4) are taken from the goal
%the types of output variables (X0,X2) will be determined inside the rule body
%the truth type of the rule is private since at least one its line has private truth type
viewESM(X0 : public string, X2 : private int, X4 : public string) : private bool :-

  %reading from the database always succeeds and returns "public true"
  %there are also implicit unifications "X0 = eds.employee", "X1 = eds.department", and "X2 = eds.salary"
  %we only take into account types of those unifications for which "Xi has already been declared before",
  % i.e. Xi is one of the following:
  % - an input variable of the goal
  % - a variable that has alrady been used before in the rule
  % there are no such variables in our case
  eds(X0 : public string, X1 : public string, X2 : private int) : public bool,

  (X2 : private int < 2500 : public int)                        : private bool,

  % we now need to take into account the following unifications:
  % - an input variable of the goal: X4, its type is public (defined in the goal)
  % - a variable that has alrady been used before in the rule: none
  % the unification X4 : public = dm.manager : public is public
  dm(X3 : public string, X4 : public string)                    : public bool,

  X1 : public string = X3 : public string                       : public bool.


%the types of input variables (X4) are taken from the goal
%the types of constants (0) are public
%the types of output variables (X0) will be determined inside the rule body
%the truth type of the rule is private since at least one its line has private truth type
%there are no implicit matchings of input against constant 0 since the second argument of the goal is not an input
%(constant arguments are treated differently in fib.pl example)
viewESM(X0 : public string, 0 : public int, X4 : public string) : private bool :-
  eds(X0 : public string, X1 : public string, X2 : private int) : public bool,
  2500 : public int =< X2 : private int                         : private bool,
  dm(X3 : public string, X4 : public string),                   : public bool,
  X1 : public string = X3 : public string                       : public bool.

% the truth type is private since it is private for at least one viewWSM rule
% the type of 'manager1' is public since it is a constant
% the type of Y1 is public string since it is so for all viewESM rules
% the type of Y2 is private string since it is so for at least one viewESM rule
:- input([]).
:- output([Y1, Y2]).
viewESM(Y1 : public string, Y2 : private int, 'manager1' : public string) : private bool?
