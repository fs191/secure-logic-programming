---
title: lpsec
description: a compiler for the privacy-preserving logic programming language PrivaLog
---

# Syntax

PrivaLog has a very similar syntax to Datalog. For this example let's write a program
that checks whether two people are from the same generation. First we state some facts:

```
parent(mati, anna).
parent(mati, ott).
parent(anna, ella).
parent(anna, liis).
parent(ott, mari).
```

Here we state that Mati is Anna's parent, Anna is Ella's parent and so on. Note
that the names are not capitalized, since we want to treat them as constant strings
rather than variables.

Next we introduce the `same_generation` rule. First we state that every person is
from the same generation with themselves.

```
same_generation(X, X).
```

Here the `X` token is treated as a variable, since it is capitalized.

We can also say that two people are from the same generation if their parents are
from the same generation:

```
same_generation(X, Y) :-
  parent(ParentX, X),
  parent(ParentY, Y),
  same_generation(X, Y).
```

Finally, to run the program we need a goal clause. Goal clause is marked with a
question mark and a dash symbol. Let's say we want to know whether Mari and Ella
are from the same generation. Then we would write

```
?-same_generation(mari, ella).
```

There must be exactly one goal clause in each PrivaLog program.

Now the program is complete and we can run `lpsec input.pl -o output.sc`, where
`input.pl` is the file containing the PrivaLog program. After the compilation 
succeeds, we will have a SecreC program that can be further compiled to a 
Sharemind binary.

# External database facts (EDB facts)

Now let's look at a case where the parental relationships are stored in a private 
database. To access data from the database, we use *type clauses*:

```
:-type(parent(X : private string, Y : private string)).
```

