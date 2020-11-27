---
title: lpsec
description: a compiler for the privacy-preserving logic programming language PrivaLog
---

# Running lpsec

To compile a PrivaLog program, run `lpsec INPUT -o OUTPUT`, where `INPUT` is a
path to the PrivaLog program and `OUTPUT` is the name of the SecreC program that
gets output by the compiler.

Information about available options can be displayed with `lpsec --help`

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
from the same generation in relation to themselves.

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

Note that there must be exactly one goal clause in each PrivaLog program.

Now that the program is complete we can run `lpsec input.pl -o output.sc`, where
`input.pl` is the file containing the PrivaLog program. After the compilation 
succeeds, we will have a SecreC program that can be further compiled to a 
Sharemind binary.

# Working with private data

Now let's look at a case where the parental relationships are stored in a private 
database. To access data from the database, we use *type clauses*:

```
:-type(parent(@x : private string, @y : private string)).
```

When using type clauses, we need to explicitly write whether the column is
public or private, as well as the type of the column. After this
we can use the `parent` predicate to query the database. In most cases the compiler
will be able to derive the rest of the types automatically. If type inference
fails to derive some types, the user can specify them manually by appending a colon
followed by type information just like in the type clause.

# Inputs and outputs

We might instead want to find all the people that are from the same generation
as Mari. For that we need to specify the output variables. This can be done with
the *outputs clause*:

```
:-outputs([X]).
```

Now when we use the variable `X` in our goal clause, the compiler will know,
that we want to find all the possible values for it.

```
:-goal(same_generation(mari, X)).
```

With this goal, the program will output the list with all the people that are
from the same generation as Mari.

# Types

PrivaLog supports the following types:
+ `bool`
+ `int8`, `int16`, `int32` (alias `int`), `int64`
+ `uint8`, `uint16`, `uint32`, `uint64`
+ `xor_uint8`, `xor_uint16`, `xor_uint32`, `xor_uint64`
+ `string`
+ `float32` (alias `float`), `float64`

# Built-ins
PrivaLog has the following operators, grouped by precedence (low to high):
+ `;`
+ `,`
+ `>=`, `=<`, `>`, `<`, `=:=`, `=/=`, `=`, `is`
+ `+`, `-`
+ `*`, `/`
+ `^`
+ `\\+`

PrivaLog has the following built-in functions:
+ `sqrt`
+ `mod`

The operators work similarly to Prolog.

# Aggregations

# Nondeterminism

