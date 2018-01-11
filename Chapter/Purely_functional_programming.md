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


## Pure vs Impure {#sec:purely_functional_purity}
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

In [@lst:impure_haskell] we note the return type of the function has become `IO [a]`. Why is that? If we inspect the function itself, we see that we suddenly added a `print` statement inside the function definition. The `print` function has the type `print :: Show a => a -> IO ()` which means it takes a _showable_ value, performs an I/O operation and finally returns `()` (called unit, which is an empty value). By using `print` inside of `appendList`, the type system now requires `appendList` to be marked as working inside I/O as well --- it is by this mechanism that we can guarentee that a function that does not have any I/O in its type signature will not perform any side-effects --- or as a popular saying in the Haskell community "It will not launch nukes", in the context of running a pure function.


## Lazy vs Strict Evaluation {#sec:purely_functional_lazy}
Before finally moving onto Haskell, there is a final point that is worth touching on, namely the evaluation model of the language. Programming languages are commonly split between two major evaluation models, namely lazy or strict (eager) evaluation. Let's take a brief look at what this means for a language. Let us first define a function, in [@lst:evaluation_example] so that we can compare how it evaluates under the two models.

```{#lst:evaluation_example .haskell}
add a b = a + b

callAdd = add (2+3) (4*3)
```

: Function for demonstrating evaluation models

Most languages implement strict evalution, usually call-by-value, which is the simpler model of the two evaluation models. In a strict language, calling `callAdd` will go through the sequence shown in [@lst:evaluation_sequence_strict] during evaluation.

```{#lst:evaluation_sequence_strict .haskell}
add (2+3) (4*3)
add 5 (4*3)
add 5 12
5 + 12
17 <-- final evaluation
```

: Strict evaluation sequence

We see that arguments are evaluated before being passed into the function. Some arguments will be passed as references, commonly arrays and dictrionaries, in some language that mix in call-by-reference. That said, it is not a requirement from strict evaluation at all. Now if we constrast this with lazy evaluation, we end up with a sequence as shown in [@lst:evaluation_sequence_lazy].

```{#lst:evaluation_sequence_lazy .haskell}
add (2+3) (4*3)
(2+3) + (4*3) <-- final evaluation (for now)
```

: Lazy evaluation sequence

The lazy evaluation leaves us with a pointer, called a thunk, which points to the computation. Only when we actually need the value inside the computation, say if we later decide to `print` it, do we perform the actual evaluation. Typically this is implemented as call-by-need, which means that it only evaluates the computation when it needs it, and additionally, memoizes the value so it does not need to reevaluate it on subsequent calls (in constrast to call-by-name, which evaluates the computation every time).

One thing to note about the use thunks/laziness is that it inherently makes reasoning about performance harder, particularly reasoning about space, since thunks can build up and consume a ton of additional memory. There are common strategies to fix this problem, such as marking a data type as strict, or forcing the evaluation to normal form.

\ \

Finally, some interesting features we get with laziness/non-strict evaluation are infinite lists, recursive definitions and much more. For example, in [@lst:evaluation_infinite_list] we see an example of an infinite list being used. This does not cause an endless loop, because the `take` function only needs to evaluate the list to the first five elements.

```{#lst:evaluation_infinite_list .haskell}
Prelude> let infList = [1,2..]
Prelude> take 5 infList
[1,2,3,4,5]
```

: Taking five elements from an infinite list


## Primer on Haskell {#sec:purely_functional_primer}
With our theoretical background on some of the core concepts of purely functional programming languages, we can move on to a specific one in the category, namely Haskell. Haskell is a lazy-evaluated purely functional programming, that was a unification of several research projects and attempts at lazily-evaluated languages, made by the Haskell working group in around 1990.

Haskell is strongly statically typed, and features a type system with type inference. This means that type annotations are not required, except to resolve ambiquity for the type system when multiple typing judgements are valid. That said, it is still common practice to annotate functions with type signatures, as they very much help documenting the code.

\ \

First, let us take a look at a simple Haskell function, as shown in[@lst:primer_haskell_function].

```{#lst:primer_haskell_function .haskell}
add :: Int -> Int -> Int
add a b = a + b
```

: Dissection of a Haskell function

The first line, `add :: Int -> Int -> Int`, is the type signature. Since Haskell is based on the lambda-calculus, it uses a concept called currying to allow multiple arguments into a function, since a lambda-expression only really takes one argument in. While in lambda-calculus we might write $\lambda a. \lambda b. a + b$, we see this concept mostly in the type signature with $\lambda a$ being the first `Int`, $\lambda b$ being the second, and finally the returned value `a+b` is represented by the third and final `Int`.

On the second line, `add a b = a + b`, we have the function definition, which declares a function, `add`, which takes two arguments, `a` and `b`, and then in the function body it returns `a + b`. In Haskell the last expression in the function body is the one that is returned by the function.

\ \

Moving on, an essential part of Haskell are the types, which brings us to the three different ways of specifying types, `type` which simply aliases something, `data` which creates a new data type along with its constructors, and `newtype` which is a special form of a data type that only exist at compile time and is erased at runtime. `newtype` is often used to wrap existing types, so that one can describe different behaviour for them. For example, if you wanted to express an _email_ and a _username_, but both are `String`s, you can use a newtype such in [@lst:primer_newtypes].

```{#lst:primer_newtypes .haskell}
newtype Email = Email String
newtype Username = Username String

readEmail :: Email -> IO ()
readEmail (Email email) = print email

...
```

: Using newtypes to distinquish between an email and a username

You are now able to force any consumer of your program \gls{api} to be aware if they are supplying an email or username, instead of simply accepting any form of `String`ly value.

Data types come in handy when we want to express multiple states, options or similar, such as a traffic light, as shown in [@lst:primer_data_types].

```{#lst:primer_newtypes .haskell}
data TrafficLight = Red | Yellow | Green

trafficLight :: TrafficLight -> IO ()
trafficLight Red = print "Stop!"
trafficLight Yellow = print "Are you ready?"
trafficLight Green = print "Go!"
```

: Using data types to operate a traffic light

\ \

Quickly moving on, Haskell introduces a syntatic sugar for sequencing actions via _monads_, called _do-notation_, demonstrated in [@lst:primer_haskell_do].

```{#lst:primer_haskell_function .haskell}
tellTheWorld :: IO ()
tellTheWorld = do
  print "Hey! What's your name?"
  name <- getLine
  print $ "Hello " ++ name
```

: Introducing do-notation

So a couple of concepts were introduced here. First we have the type signature, `tellTheWorld :: IO ()`, telling us we are performing an action in the `IO` monad. Then we use the `<-` operator, inside the do block, to pull out a monadic value from `getLine`, which has the type signature `IO String`, giving us a `String` in `name`. Then we finally use that value by concatenating the string `"Hello "` with the value of the same type, `name`. The `$` can be read as "parentheses to the end of the line", meaning we could have written `print ("Hello " ++ name)` instead.

We wont delve too much into `Monad`s, `Applicative`s or `Functor`s, other than to note that they are a very common abstraction in Haskell. Their typeclasses are shown in [@lst:primer_monads] for reference.

```{#lst:primer_monads .haskell}
class Functor f where
    fmap    :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure    :: a -> f a
    (<*>)   :: f (a -> b) -> f a -> f b
    (*>)    :: f a -> f b -> f b
    u *> v  = …
    (<*)    :: f a -> f b -> f a
    u <* v  = …

class Applicative m => Monad m where
    (>>=)   :: m a -> (a -> m b) -> m b
    return  :: a -> m a
    return  =  pure
    (>>)    :: m a -> m b -> m b
    m >> k  =  m >>= \_ -> k
```

: The `Functor`, `Applicative` and `Monad` type class definitions

\ \

With this we can introduce typeclasses, which are a way of both overloading operators and function names, and providing abstractions. A typeclass, much like an interface in Java, specifies the functions which any instance of it has to implement. This gives us ad-hoc polymorphism. For example, in [@lst:primer_typeclass] we define the typeclass `Printable`, and an instance thereof.

```{#lst:primer_typeclass .haskell}
class Printable a where
  printable :: a -> String

instance Printable Int where
  printable i = show i

printing :: Printable s => s -> IO ()
printing s = print $ printable s
```

: The `Printable` typeclass and an instance of it

The example is perhaps a bit contrived; our instance just defines a way for an `Int` to be converted to a `String`, but we can now talk about functions that are constrained to types that have an instance of `Printable`, as shown in the last part of [@lst:primer_typeclass]. In the type signature, `printing :: Printable s => s -> IO ()`, we note the `Printable s =>` part, which means that all type variables, `s`, need to be instances of the `Printable` typeclass.

\ \

Finally, we are going to touch on the subject of \glspl{gadt} which are a generalization of the data types we saw earlier, allowing us to annotate the constructors with types. First, let's look at a motivation for why we even need them, by constructing a `Program`, as shown in [@lst:primer_gadts_motivation].

```{#lst:primer_gadts_motivation .haskell}
data Program
  = Value Int
  | Boolean Bool
  | Addition Program Program
  | Equality Program Program
```

: A program as a data type

This leaves us with a problem though: the `Equality` constructor clearly is expecting a `Boolean`, while the `Addition` constructor is expecting a `Value`. When we later have to implement our evaluator for this program, we will run into trouble with this. But what if we could specify the return and input types of the program? In comes \glspl{gadt} to the rescue, as shown in [@lst:primer_gadts].

```{#lst:primer_gadts .haskell}
data Program a
  = Value    :: Int -> Program Int
  | Boolean  :: Bool -> Program Bool
  | Addition :: Program Int -> Program Int -> Program Int
  | Equality :: Program Boolean -> Program Boolean -> Program Boolean
```

: A \gls{gadt} to describe a program

\ \

While there are many more interesting extensions and concepts in Haskell, this should be sufficient to understand most of this thesis.
