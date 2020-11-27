% Day of week
:-type(day(Day: private int, Month: private string, Year: private int)).

is_century(Year) :-
  mod(Year, 100) = 0.

is_not_century(Year) :-
  mod(Year, 100) =/= 0.

is_leap_year(Year) :-
  is_not_century(Year),
  mod(Year, 4) = 0.

is_leap_year(Year) :-
  is_century(Year),
  mod(Year, 400) =/= 0.

is_not_leap_year(Year) :-
  mod(Year, 400) = 0,
  mod(Year, 4) =/= 0.

is_not_leap_year(Year) :-
  is_not_century(Year),
  mod(Year, 4) =/= 0.

is_not_leap_year(Year) :-
  is_century(Year),
  mod(Year, 400) = 0.

month_days_on_year(january, 31, _).
month_days_on_year(february, 28, Year) :-
  is_leap_year(Year).
month_days_on_year(february, 27, Year) :-
  is_not_leap_year(Year).
month_days_on_year(march, 31, _).
month_days_on_year(april, 30, _).
month_days_on_year(may, 31, _).
month_days_on_year(june, 30, _).
month_days_on_year(july, 31, _).
month_days_on_year(august, 31, _).
month_days_on_year(september, 30, _).
month_days_on_year(october, 31, _).
month_days_on_year(november, 30, _).
month_days_on_year(december, 31, _).

month_comes_after(february, january).
month_comes_after(march, february).
month_comes_after(april, march).
month_comes_after(may, april).
month_comes_after(june, may).
month_comes_after(july, june).
month_comes_after(august, july).
month_comes_after(september, august).
month_comes_after(october, september).
month_comes_after(november, october).
month_comes_after(december, november).
month_comes_after(january, december).

dow_comes_after(tuesday, monday).
dow_comes_after(wednesday, tuesday).
dow_comes_after(thursday, wednesday).
dow_comes_after(friday, thursday).
dow_comes_after(saturday, friday).
dow_comes_after(sunday, saturday).
dow_comes_after(monday, sunday).

day_of_week(monday, 1, january, 1900).

% if we call day_of_week with fbbb, then there is no need to generate bbbb rules
% for day_of_week. We should think if this can be made as an optimization.
day_of_week(DayOfWeek, 1, january, Year) :-
  X is dow_comes_after(DayOfWeek, X),
  Y is Year-1,
  day_of_week(DW, 31, december, Y),
  DW =:= X.
  
day_of_week(DayOfWeek, 1, Month, Year) :-
  Month =/= january,
  month_comes_after(Month, M),
  dow_comes_after(DayOfWeek, DW),
  month_days_on_year(M, D, Year),
  day_of_week(DW2, D, M, Year),
  DW2 =:= DW.

day_of_week(DayOfWeek, Day, Month, Year) :-
  Day =/= 1,
  D is Day-1,
  dow_comes_after(DayOfWeek, DW),
  day_of_week(DW2, D, Month, Year),
  DW2 =:= DW.

goal(DayOfWeek) :-
  day(Day, Month, Year),
  day_of_week(DayOfWeek, Day, Month, Year).

:-outputs([DayOfWeek]).
?-goal(DayOfWeek).

