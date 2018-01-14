# Approaches to Offloading {#cha:approaches}
With the theoretical background in place, we can begin to investigate what our possibilities are for a possible implementation of a system for offloading of computations. In this chapter we will take a look at five different proposals for handling offloading, along with their pros and cons, and finally summing them up in [@tbl:approaches_overview].

For reference in the rest of the chapter, an overview of the GHC system is presented in [@fig:approaches_ghc_overview]. GHC and Haskell is used interchangeably throughout, seeing as GHC it is considered the de facto compiler for Haskell.

![GHC overview](Graphic/Haskell Overview.png "GHC overview"){#fig:approaches_ghc_overview width=70% }

We see that besides the compiled Haskell code itself, there is a supporting runtime, along with linking to libraries and a C code \gls{ffi}. This all of course sits on top of the \gls{os} and underlying hardware.

## Extending the Runtime {#sec:approaches_runtime}
The first immediate thought is to go the way of many previous attempts, and aim to automate as much as possible so the programmer would not have to worry about if they even want to offload a function or not, but simply have the runtime decide that based on the current conditions. Indeed, as a user of the system, this would provide the smoothest way to get started, initially, but it also is the most complex and invasive approach we could choose.

\ \

The \gls{ghc} runtime is a massive codebase written in C and clocking in at around 50.000 lines of code[@Brown2012], and is responsible for a number of things, such as:

* Memory management (i.e. garbage collection)
* Thread management and scheduling
* Primitive operations provided by \gls{ghc}\
    - Function calls\
    - Exceptions\
    - Allocating arrays\
    - Handling `MVar`s\
    - Etc
* Profiling (heap-profiling, time-profiling)
* Support for \gls{stm}
* Bytecode interpreter and dynamic linker for \gls{ghci} (the REPL)

An overview of the runtime can roughly be given as shown in [@fig:approaches_runtime_overview].

![GHCs Runtime System](Graphic/Runtime System.png "GHCs Runtime System"){#fig:approaches_runtime_overview width=100% }

One can quickly see that extending the runtime is a daunting task. For example, taking over function calls would involve making sure that we do not mess with the \gls{gc}, adding support in the profiler and scheduler, making sure we do not affect \gls{stm}, support byte-code generation of the changed functions.

\ \

Since the idea is to put the granularity of offloading at the function level---and only pure functions at that, since we cannot know if any functions in `IO` are offloadable---it makes sense to check out how the GHC runtime handles function calls, which we can get by checking out the commentary on the GHC compiler, specifically for its runtime[@CommentaryFunction]. We see that the \gls{ghc} \gls{rts} divides function calls into four different categories (see [@lst:approaches_four_categories] for the definitions of the functions mentioned in the list):

- Unknown functions, i.e. the argument `f`, in `map`, is unknown at compile time and can come from many places
- Known functions with too few arguments, such as `tooFewMap`, which is missing one argument
- Known functions that are saturated, such as `saturatedMap`
- Known functions with too many arguments, such as `compose`, which takes in two functions, as if it was a saturated call, composes them and returns a new function which will behave as an unknown function


```{#lst:approaches_four_categories .haskell}
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

tooFewMap :: Num a => [a] -> [a]
tooFewMap = map (\x -> x + 2)

saturatedMap :: Num a => [a]
saturatedMap = map (\x -> x + 2) [1,2,3,4]

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = f . g
```

: Examples of the four different categories of function calls


From the original paper [@Marlow2006] discussing the choice of using an _eval/apply_ evaluation model rather than _push/enter_, we get some estimates with regards to how many calls are actually to unknown functions, stating that on average the figure is around 20%. Conversely this means that 80% of calls should be to known functions, and we can therefore argue that an implementation should only look into supporting offloading for known function calls, which would cut down a little on the surface area of the implementation.

While this may still seem like too coarse a granularity, one could eventually add support for pragmas that would mark a function as not offloadable (and offloadable for `IO` functions), as done in MAUI. This would entail propogating this information around the runtime.

\ \

As such, we can conclude that extending the runtime is first and foremost a very complex and huge undertaking. Further more it would require quite the buy-in from the users, since they would have to use a custom version of the GHC compiler, seeing as it would probably be hard (and would take years due to the feature cycle of GHC) to get it into the main GHC implementation.

A final note is that it would also involve implementing the whole system in C, which sort of defeats the purpose of exploring what we can use purely functional programming languages for.


## Using the `unsafePerformIO` Escape Hatch {#sec:approaches_unsafe}
Haskell is a very high-level language, and as such abstracts a lot of the nitty-gritty details of the underlying hardware away. Sometimes though, one is forced to deal with lower-level details to implement fast primitives that can later be exposed in a higher-level manner. As such, there are several escape hatches in Haskell. To name a few,

- `unsafePerformIO` is the main "back door" into the `IO` monad, and can circumvent the type system, meaning you could perform it inside a pure function
- `unsafeDupablePerformIO` is a more efficient version of `unsafePerformIO` that lacks thread safety
- `unsafeInterleaveIO` makes it possible to defer `IO` computations lazily, and as such is used to implement lazy file reading, for example

The idea is to use `unsafePerformIO` inside function definitions, to decide whether to offload them or not. By iteself this would require manually sprinkling these calls around, but in combination with one of the other approaches, such as Rewrite Rules [@sec:approaches_rewrite] or Compiler/Language extensions [@sec:approaches_extension], this could be made feasible in an ergonomic way for the developer to use.

Now, one might ask "If you need to manually insert calls, why not use a regular function instead of `unsafePerformIO`?" which is indeed a valid question. The reasoning behind this is that we would like as much as possible to force functions into the `IO` monad, which did not need to be there in the first place, as it breaks down our ability to reason about our code (w.r.t. referential transparency), and means we would have to use monadic do-notation everywhere, making writing code quite tedious very quickly.

\ \

`unsafePerformIO` comes with some assumptions for it to work "safely", such as not relying on any specific order of which the side-effects, in the function using `unsafePerformIO`, will be performed. That is, it is indeterministic, because of the call-by-need semantics of Haskell, which neither guarentees a function to ever be fully evaluated to normal form, nor that it will be evaluated at the place it is encountered. Furthermore, for the side-effects to not be optimized away, there are a few precautions, such as making sure the function is not inlined (via `{-# NOINLINE foo #-}` pragma), the expression is not eliminated (via the compiler flag `-fno-cse`) and making sure the call to `unsafePerformIO` is not floated outside a lambda.

To address these assumptions, we mainly need to consider the first one, because the second one is mainly an implementation detail. Luckily we do not care about neither the order of evaluation, nor if it ever gets evaluated (in fact, that is only a good thing), because of the inherent goal of offloading is to save the device from executing code. This means that Haskell's call-by-need semantics is very well suited for the goal of offloading.

\ \

A simplified example is shown in [@lst:approaches_unsafe], as a rough sketch of the idea, with `shouldOffload` and `offloadFunction` just being stubs to satisfy the type system for now.

```{#lst:approaches_unsafe .haskell}
import System.IO.Unsafe

shouldOffload :: IO Bool
shouldOffload = pure True

offload :: (a -> b) -> a -> IO b
offload f a = pure $ f a

offloadFunction :: (a -> b) -> a -> b
offloadFunction f a =
  if unsafePerformIO shouldOffload
    then unsafePerformIO $ offload f a
    else f a
```

: Rough sketch for an offload function using `unsafePerformIO`

The code does the following:

- The `offloadFunction` function simply take in a function and the last argument that should be applied,
- calls `shouldOffload` containing the network state which returns `True` if it makes sense to offload and `False` if not,
- and then either return\
    - a call to `unsafePerformIO`, with `offload` taking care of performing the actual network calls to offload, which needs to be synchronous/blocking\
    - the `f a` function itself, if it should not have offloaded

Admittedly a lot of implementation details are left out, but the underlying concept should still be present in the code. This implementation would allow us to do something like `offloadFunction heavyComputation input`, which will then take care of either offloading the funtion, getting back its input and returning that, or simply returning the function application as would happen in normal code.


## GHC Compiler/Language Extension {#sec:approaches_extension}
À la how `ApplicativeDo` reorders computations.


## Rewrite Rules {#sec:approaches_rewrite}


## Mondic Framework {#sec:approaches_monadic}
MTL-style/Free/Freer, maybe look into `Haxl`.


## Manipulate the Source {#sec:approaches_source}
E.q. haskell-src-exts or ghc-exactprint


## Template Haskell {#sec:approaches_template}


## Evaluation {#sec:approaches_evaluation}
The rows for [@tbl:approaches_overview] are:

- **C**omplexity of the implementation
- **A**doptability by the wider community
- **B**uy-in, for a developer to use the system
- **G**ranularity of the offloading mechanism

TODO: Mention the complexity of routing the function calls to the correct place server side

----------------------------------------------------------------------------------------------------------------------------
**Approach**                     **C**\tiny{omplexity} **A**\tiny{doptability} **B**\tiny{uy-in} **G**\tiny{ranularity}
-------------------------------- --------------------- ----------------------- ----------------- ---------------------------
Extending the runtime            Very High[^er]        Low                     High              Coarse[^allpure]

**unsafePerformIO**              Low                   Medium                  Low               Fine[^manually]

Compiler/Language                ...                   ...                     ...               ...
Extension

**Rewrite Rules**                ...                   ...                     ...               ...

Monadic Framework                ...                   ...                     ...               ...

**Manipulate the**               ...                   ...                     ...               ...
**source**

Template Haskell                 ...                   ...                     ...               ...
----------------------------------------------------------------------------------------------------------------------------

Table: Overview of the pros and cons of the different proposals {#tbl:approaches_overview}

[^er]: While technically feasible, it would be a massive undertaking

[^allpure]: All pure _known_ functions with saturated arguments

[^manually]: Manually controlled by adding function calls before the code that should be offloaded
