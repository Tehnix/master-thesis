# Pure Functional Programming {#cha:purely_functional}
In this chapter, we will explore what it means to be a _purely functional programming language_, and what consequences that has for program design. If the reader is closely familiar with pure functional programming e.g. via Haskell, Idris, PureScript, Elm or similar, these concepts should already be familiar to the user, and they can safely skip this chapter.


## Functional programming {#sec:purely_functional_programming}
Functional programming is a paradigm, under the declarative programming paradigm, which stands heavily in contrast to imperative programming, positioning itself in the opposite end of the spectrum. Whereas an imperative program can be seen as a series of state changing steps, functional programming treats computations as evaluations of mathematical expressions. Boiling it down to the essentials, a functional language is basically an extension of lambda-calculus, with some added constructs to make the language richer. Common for functional languages are that they support concepts such as first-class functions, i.e. support for passing functions around as arguments, and higher-order functions, which are functions that can take other functions as input, or even return a function. The astute reader might recognize these concepts if they have played with something like Python, JavaScript or Ruby (and many other languages), and they would be right, seeing as functional programming concepts have spread out into mainstream languages more and more. An example of first-class functions in first Python and then Haskell are shown in [@lst:fist_class_functions_python] and [@lst:fist_class_functions_haskell] (both run in their respective REPLs),

```{#lst:fist_class_functions_python .python}
>>> def add3(a): return a + 3
>>> map(add3, [1,2,4])
[4, 5, 7]
```

: First-class functions in Python

```{#lst:fist_class_functions_haskell .haskell}
Prelude> let add3 a = a + 3
Prelude> map add3 [1,2,3,4]
[4,5,6,7]
```

: First-class functions in Haskell

In [@lst:fist_class_functions_python] and [@lst:fist_class_functions_haskell] we actually also demonstrate higher-order functions, in the form of `map`, which in Haskell has the type `map :: (a -> b) -> [a] -> [b]`, read as `map` is a function which first argument is a function taking an `a` and returning a `b`. It then takes a list of `a`s as the second argument and finally returns a list of `b`s.

A functional programming language can be said to be a language that primarily interacts by calling, manipulating and passing around functions, which is why we also see it in multi-paradigm languages, such as Python etc.

## Purity {#sec:purely_functional_purity}
Now that we have an idea of what a functional language is, we can talk about _purity_ and _impurity_. When we say a language is pure or impure, we are in essence describing how we treat computational effects, such as I/O. In [@Sabry1998] the author tries to come up with a formal definition of what it means to be pure,

> _In essence, the new definition asserts that a language is purely functional if it can be implemented using either call-by-value, call-by-need, or call-by-name, with no observable difference between the different strategies -- other than termination properties._ [@Sabry1998, p.1]

More informally, and what is usually also the more popular and simple explanation, is that a purity means that we get referential transparency, as we know it from mathematics. This was championed by John Launchbury and Simon Peyton Jones in [@Launchbury1995].

\ \

To reword the above in perhaps a more digestable way, for a function to be referentially transparent: if a function receives the same input, it must produce the same output. Take for example the code shown in [@lst:referential_transparency_python] and [@lst:referential_transparency_haskell].

```{#lst:referential_transparency_python .python}
def appendList(a, b):
  return a + b
```

: Example of a referentially transparent function in Python

```{#lst:referential_transparency_haskell .haskell}
appendList :: [a] -> [a] -> [a]
appendList a b = a ++ b
```

: Example of a referentially transparent function in Haskell

No matter how many times we we give the arguments `[1,2,3]` and `[4,5,6]` the result will be `[1,2,3,4,5,6]`. This is something that we can guarentee in the types of the Haskell code, but only something we can assume in the Python code, since it is untyped. Similarly, the statically type Java language would also only hold assumptions, and no guarentees, since in Java we have no way of talking about effects. This is where Haskell and similar purely functional languages, which has seemed quite similar so far, diverges!

```{#lst:impure_haskell .haskell}
appendList :: [a] -> [a] -> IO [a]
appendList a b = do
  let newList = a ++ b
  print newList
  newList
```

: An impure function in Haskell

In [@lst:impure_haskell] we note the return type of the function has become `IO [a]`. Why is that? If we inspect the function itself, we see that we suddenly added a `print` statement inside the function definition. The `print` function has the type `print :: Show a => a -> IO ()` which means it takes a _showable_ value, performs an I/O operation and finally returns `()` (called unit, which is an empty value). By using `print` inside of `appendList`, the type system now requires `appendList` to be marked as working inside I/O as well --- it is by this mechanism that we can guarentee that a function that does not have any I/O in its type signature will not perform any side-effects.


## Lazy vs Strict Evaluation {#sec:purely_functional_lazy}
Before finally moving onto Haskell, there is a final point that is worth touching on, namely the evaluation model of the language. Programming languages are commonly split between two major evaluation models, namely lazy or strict (eager) evaluation. Let's take a brief look at what this means for a language. Let us first define a function, in [@lst:evaluation_example] so that we can compare how it evaluates under the two models.

```{#lst:evaluation_example .haskell}
add a b = a + b

callAdd = add (2+3) (4*3)
```

: Function for demonstrating evaluation models

Most languages implement strict evalution, usually call-by-value, which is the simpler model of the two evaluation models. In a strict language, calling `callAdd` will go through the sequence shown in [@lst:evaluation_example_strict] during evaluation.

```{#lst:evaluation_example_strict .haskell}
add (2+3) (4*3)
add 5 (4*3)
add 5 12
5 + 12
17 <-- final evaluation
```

Now if we constrast this with lazy evaluation, we end up with a sequence as shown in [@lst:evaluation_example_lazy].

```{#lst:evaluation_example_lazy .haskell}
add (2+3) (4*3)
(2+3) + (4*3) <-- final evaluation (for now)
```

The lazy evaluation leaves us with a pointer, called a thunk, which points to the computation. Only when we actually need the value inside the computation, say if we later decide to `print` it, do we perform the actual evaluation. Typically this is implemented as call-by-need, which means that it only evaluates the computation when it needs it, and additionally, memoizes the value so it does not need to reevaluate it on subsequent calls (in constrast to call-by-name, which evaluates the computation every time).


## Primer on Haskell {#sec:purely_functional_primer}
With our theoretical background on some of the core concepts of purely functional programming languages, we can move on to a specific one in the category, namely Haskell. Haskell is a lazy-evaluated purely functional programming, that was a unification of several research projects and attempts at lazily-evaluated languages, made by the Haskell working group in around 1990.

Haskell is strongly statically typed, and features a type system with type inference. This means that type annotations are not required, except to resolve ambiquity for the type system when multiple typing judgements are valid.

\ \

TODO: More code!
