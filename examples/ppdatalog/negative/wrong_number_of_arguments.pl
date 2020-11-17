% Day of week
:-type(day(@day: private int, @month: private string, @year: private int)).

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

comes_after(february, january).
comes_after(march, february).
comes_after(april, march).
comes_after(may, april).
comes_after(june, may).
comes_after(july, june).
comes_after(august, july).
comes_after(september, august).
comes_after(october, september).
comes_after(november, october).
comes_after(december, november).
comes_after(january, december).

comes_after(tuesday, monday).
comes_after(wednesday, tuesday).
comes_after(thursday, wednesday).
comes_after(friday, thursday).
comes_after(saturday, friday).
comes_after(sunday, saturday).
comes_after(monday, sunday).

day_of_week(monday, 1, january, 1900).

day_of_week(DayOfWeek, 1, january, Year) :-
  X = comes_after(DayOfWeek, X),
  Y = Year-1,
  day_of_week(X, 31, december, Y).
  
day_of_week(DayOfWeek, 1, Month, Year) :-
  Month =/= january,
  comes_after(Month, X),
  comes_after(DayOfWeek, Y),
  month_days_on_year(X, N, Year),
  day_of_week(Y, N, Year).

day_of_week(DayOfWeek, Day, Month, Year) :-
  Day =/= 1,
  comes_after(Day, X),
  comes_after(DayOfWeek, Y),
  day_of_week(Y, X, Month, Year).

goal(DayOfWeek) :-
  day(Day, Month, Year),
  day_of_week(DayOfWeek, Day, Month, Year).

:-outputs([DayOfWeek]).
?-goal(DayOfWeek).
