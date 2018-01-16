# Approaches to Offloading {#cha:approaches}
With the theoretical background in place, we can begin to investigate what our possibilities are for a possible implementation of a system for offloading of computations. In this chapter we will take a look at five different proposals for handling offloading, along with their pros and cons, and finally summing them up in [@tbl:approaches_overview]. Each approach will be evaluated based on the following parameters,

- **C**omplexity of the implementation
- **A**doptability by the wider community
- **B**uy-in, for a developer to use the system
- **G**ranularity of the offloading mechanism
- **S**erver-side story

\ \

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

As such, we can conclude that extending the runtime is first and foremost a very complex and huge undertaking. Further more it would require quite the buy-in from the users, since they would have to use a custom version of the \gls{ghc} compiler, seeing as it would probably be hard (and would take years due to the feature cycle of \gls{ghc}) to get it into the main GHC implementation. This will also greatly hurt adoptability, and the custom \gls{ghc} compiler would have to play catch up to the main implementation. And of course, we do not have a server-side story, since this would only enable the offloading mechanism, but nothing in regards to how the server should be implemented.

A final note is that it would also involve implementing the whole system in C, which sort of defeats the purpose of exploring what we can use purely functional programming languages for.


## Using the `unsafePerformIO` Escape Hatch {#sec:approaches_unsafe}
Haskell is a very high-level language, and as such abstracts a lot of the nitty-gritty details of the underlying hardware away. Sometimes though, one is forced to deal with lower-level details to implement fast primitives that can later be exposed in a higher-level manner. As such, there are several escape hatches in Haskell. To name a few,

- `unsafePerformIO` is the main "back door" into the `IO` monad, and can circumvent the type system, meaning you could perform it inside a pure function
- `unsafeDupablePerformIO` is a more efficient version of `unsafePerformIO` that lacks thread safety
- `unsafeInterleaveIO` makes it possible to defer `IO` computations lazily, and as such is used to implement lazy file reading, for example

The idea is to use `unsafePerformIO` inside function definitions, to decide whether to offload them or not. By iteself this would require manually sprinkling these calls around, but in combination with one of the other approaches, such as Rewrite Rules [@sec:approaches_rewrite] or Compiler/Language extensions [@sec:approaches_extension], this could be made feasible in an ergonomic way for the developer to use.

Now, one might ask "If you need to manually insert calls, why not use a regular function instead of `unsafePerformIO`?" which is indeed a valid question. The reasoning behind this is that we would like as much as possible to force functions into the `IO` monad, which did not need to be there in the first place, as it breaks down our ability to reason about our code (w.r.t. referential transparency), and means we would have to use monadic do-notation everywhere, making writing code quite tedious very quickly.

\ \

`unsafePerformIO` comes with some assumptions for it to work _safely_, such as not relying on any specific order of which the side-effects, in the function using `unsafePerformIO`, will be performed. That is, it is indeterministic, because of the call-by-need semantics of Haskell, which neither guarentees a function to ever be fully evaluated to normal form, nor that it will be evaluated at the place it is encountered. Furthermore, for the side-effects to not be optimized away, there are a few precautions, such as making sure the function is not inlined (via `{-# NOINLINE foo #-}` pragma), the expression is not eliminated (via the compiler flag `-fno-cse`) and making sure the call to `unsafePerformIO` is not floated outside a lambda.

To address these assumptions, we mainly need to consider the first one, because the second one is mainly an implementation detail. Luckily we do not care about neither the order of evaluation, nor if it ever gets evaluated (in fact, that is only a good thing), because of the inherent goal of offloading is to save the device from executing code. This means that Haskell's call-by-need semantics is very well suited for the goal of offloading.

\ \

A simplified example is shown in [@lst:approaches_unsafe], as a rough sketch of the idea, with `shouldOffload` and `offloadFunction` just being stubs to satisfy the type system for now.

```{#lst:approaches_unsafe .haskell}
import System.IO.Unsafe

shouldOffload :: IO Bool
shouldOffload = pure True

offload :: (a -> b) -> a -> IO b
offload f a = pure $ f a

{-# NOINLINE offloadFunction #-}
offloadFunction :: (a -> b) -> a -> b
offloadFunction f a =
  if unsafePerformIO shouldOffload
    then unsafePerformIO $ offload f a
    else f a
```

: Rough sketch for an offload function using `unsafePerformIO`

The code does the following:

- The `offloadFunction` function simply take in a function and an argument that should be applied,
- calls `shouldOffload` containing the network state which returns `True` if it makes sense to offload and `False` if not,
- and then either return\
    - a call to `unsafePerformIO`, with `offload` taking care of performing the actual network calls to offload, which needs to be synchronous/blocking\
    - the `f a` function itself, if it should not have offloaded

Admittedly a lot of implementation details are left out, but the underlying concept should still be present in the code. This implementation would allow us to do something like `offloadFunction heavyComputation input`, which will then take care of either offloading the funtion, getting back its input and returning that, or simply returning the function application as would happen in normal code.


<!--
TODO: Make the code polyvariadic, see:
  - https://mail.haskell.org/pipermail/haskell-cafe/2006-May/015905.html,
  - http://okmij.org/ftp/Haskell/vararg-fn.lhs, https://wiki.haskell.org/Varargs,
  - http://okmij.org/ftp/Haskell/polyvariadic.html#polyvar-fn

One thing that might seem a bit annoying is the fact that `offloadFunction` only works on functions with one argument. While this is certainly not a hindrance to its usage, we can actually make the function polyvariadic---i.e. a function of variable number of arguments of variable type---by extending it a bit, as shown in [@lst:approaches_unsafe_polyvariadic].

```{#lst:approaches_unsafe_polyvariadic .haskell}

```

: Making `offloadFunction` polyvariadic
-->

\ \

The complexity would not be that high, and the buy-in would be low, but the adoptability will probably not be the greatest seeing as `unsafePerformIO` introduces quite a lot of uncertainty in the program. The implementation would need to be battle-tested for people to be willing to trust it at least. As for granularity, it is very fine-grained---perhaps too much---function calls need to be manually placed at places that might benefit from offloading. Finally, there is yet again no server-side story.


## GHC Compiler/Language Extension {#sec:approaches_extension}
À la how `ApplicativeDo` reorders computations.


## Rewrite Rules {#sec:approaches_rewrite}
With \glspl{ghc} support for rewriting functions using rewrite rules, one could imagine coming up with a set of rules to rewrite common functions into ones that are identical, except for additional logic to offload the function itself. For example, combining the approach from [@sec:approaches_unsafe], using `unsafePerformIO`, one could keep the signature of the functions, while discretely adding the additional logic.

One problem with this approach though is that we can only target specific functions with our rewrite rules. For example, as shown in [@lst:approaches_rewrite_simpleFunction], we can target specific functions, in this case the function `simpleFunction`, which will then get rewritten from `simpleFunction x` into `offloadFunction simpleFunction x`.

```{#lst:approaches_rewrite_simpleFunction .haskell}
main :: IO ()
main = do
  print "Start:"
  print $ simpleFunction 3
  print "End!"

{-# RULES
"simpleFunction/offload simpleFunction" forall x.
    simpleFunction x = offloadFunction simpleFunction x
  #-}

simpleFunction :: Int -> [Int]
simpleFunction a = map (+a) $ map (+2) [1,2,3]
```

: Rewrite rule for `simpleFunction` to wrap it in `offloadFunction`

We can check that the rule has fired by running `stack exec -- ghc Main.hs -O2 -ddump-rule-firings` (or omit the `stack exec --` part if you are running plain \gls{ghc}), which yields,

```
...
Rule fired: map (GHC.Base)
Rule fired: unpack (GHC.Base)
Rule fired: Class op >>= (BUILTIN)
Rule fired: Class op pure (BUILTIN)
```
```
Rule fired: simpleFunction/offload simpleFunction (Main) <--- Our rule
```
```
Rule fired: Class op show (BUILTIN)
Rule fired: unpack (GHC.Base)
Rule fired: unpack-list (GHC.Base)
...
```

and running the program, combined with the code from [@lst:approaches_unsafe], gives us the following output,

```
"Start:"
"Offloading function..."
[6,7,8]
"End!"
```

_Side-note:_ the reason we defined `simpleFunction` as `map (+a) $ map (+2) [1,2,3]` was to show that multiple rewrites will take place, since `map f (map g xs) == map (f . g) xs`.

\ \

Adding calls to offload functions are indeed doable using rewrite rules, but it is a very brittle approach, since inlining will cause the rule not to fire, and it is also quite tedious, since every function one wants to offload needs a rewrite rule. As for complexity, it adds little to no real complexity on top of the `unsafePerformIO` approach from [@sec:approaches_unsafe]. Because of the brittleness, it would be most likely not see widespread adoption, even though the buy-in is very small.


## Mondic Framework {#sec:approaches_monadic}
An approach less radical than the others so far, is to utilize the structure of the program itself to enable separating the interpretation/implementation of the program---and thereby the effects---from the semantics of the program itself. This can be done thanks to the way monadic computations are done, by forming a sequence of actions to be performed. This effectively allows us to, as shown in [@fig:approaches_monadic_multiple_interpreters], having multiple ways of interpreting the same program, depending on what we want from it. We could have a pure interpretation for testing, which allows us to, say, store the program output as a list of strings and inspect it later on. Or, a debug interpreter that allows us to step through the program sequences, or just an effectful interpreter running the actual program.

![Multiple interpreters from a single program](Graphic/Multiple Interpreters.png "Multiple interpreters from a single program"){#fig:approaches_monadic_multiple_interpreters width=60% }

There are two popular styles of writing larger applications in Haskell, the first one, and by far most popular, is the \gls{mtl} style, and the second is using a concept known as `Free` monads---let us dive into the former first.


### MTL-style
TODO: Showcase how MTL can be used to separate the implementation and semantics, touch on the boilerplate problem (O(n*m) instances) and the lack of fine-grained control? Monad transformers arise because _Monads do not compose in general_.


### Free style
The `Free`-style of writing a program allows for a very clean separation of the semantics of the program and the interpretation of it, by first structuring a form of \gls{ast} of the program, and then constructing different interpreters depending on how you want to run the program.

`Free` monads are a concept from category theory, which when used in Haskell gives us a monad for free for our data types, as long as we implement the functors for them ourselves. If you squint a bit, the data type for `Free`, shown in [@lst:approaches_free_definition], looks a lot like the data type for list, and in fact it is a fair intuition of how the structure of a program written in `Free`-style is built up. One thing that differs though, is that we would not be able to do much, if we could not rely on previous input. Luckily `Free` is not entirely like a list, and in fact works by passing continuations along the program, also known as \gls{cps}.

```{#lst:approaches_free_definition .haskell}
data Free f r
  = Pure r
  | Free (f (Free f r))

data List a
  = Nil
  | Cons a (List a)
```

: Definition of `Free` compare with the definition of `List`

To construct a program using `Free` monads, one first starts with the data type that states which instructions our program can perform. Let us make a small program that can read files, get user input, write output and perform a computation. We construct a data type, called `Effects`, for this in [@lst:approaches_free_data_type].

```{#lst:approaches_free_data_type .haskell}
data Effects next
  = ReadFile String (String -> next)
  | WriteOutput String next
  | WriteOutputInt Int next
  | GetInput (String -> next)
  | Computation Int Int (Int -> next)
```

: Our `Effects` data type that models our program

Our data type parameter, `next`, is the continuation that will allow us to go through the program. The first parameter to `ReadFile` is the filename, meaning an argument to `ReadFile`, while the second argument is the output into the continuation, `(String -> next)`, meaning it outputs a `String` to the next step. Now we can define our `Functor` instance for our data type, as defined in [@lst:approaches_free_functors].

```{#lst:approaches_free_functors .haskell}
instance Functor Effects where
  fmap f (ReadFile s g) = ReadFile s (f . g)
  fmap f (WriteOutput s g) = WriteOutput s (f g)
  fmap f (WriteOutputInt i g) = WriteOutputInt i (f g)
  fmap f (GetInput g) = GetInput (f . g)
  fmap f (Computation i1 i2 g) = Computation i1 i2 (f . g)
```

: The `Functor` instance for our `Effects` data type

Which is fairly straight forward. The `f` of the `fmap` either gets composed with the continuation of the data type (e.g. `f . g`) if there are arguments to the continuation, or applies `f g` if the continuation is a plain `next`.

The next step is to setup our functions that lift our functors into the `Free` monad, along with creating a type alias for our `Free Effects` program, as shown in [@lst:approaches_free_functions].

```{#lst:approaches_free_functions .haskell}
type Program = Free Effects

readFile :: String -> Program String
readFile s = liftF $ ReadFile s id

writeOutput :: String -> Program ()
writeOutput s = liftF $ WriteOutput s ()

writeOutputInt :: Int -> Program ()
writeOutputInt i = liftF $ WriteOutputInt i ()

getInput :: Program String
getInput = liftF $ GetInput id

computation :: Int -> Int -> Program Int
computation i1 i2 = liftF $ Computation i1 i2 id
```

: Functions lifting the functors into the `Program` monad

We are now ready to write an interpreter for our program. Let us start with a test interpreter, in [@lst:approaches_free_interpreter], that runs the whole program and simulates the input and prints the output.

```{#lst:approaches_free_interpreter .haskell}
testInterpreter :: Program next -> IO next
testInterpreter (Pure a) = return a
testInterpreter (Free effect) =
  case effect of
    ReadFile filename next -> do
      let fakeFileContent = "Test file content for: " ++ filename
      testInterpreter $ next fakeFileContent
    WriteOutput s next -> do
      putStrLn s
      testInterpreter next
    WriteOutputInt i next -> do
      print i
      testInterpreter next
    GetInput next -> do
      let fakeInput = "Fake input"
      testInterpreter $ next fakeInput
    Computation i1 i2 next -> testInterpreter $ next (i1 + i2)
```

: Interpreter for the `Free Effects` program

The final step is to construct a program, using the functions defined in [@lst:approaches_free_functions], and then apply the interpreter, from [@lst:approaches_free_interpreter], on the program. We will do both at the same time in [@lst:approaches_free_program].

```{#lst:approaches_free_program .haskell}
program :: Program ()
program = do
 filename <- MF.getInput
 contents <- MF.readFile filename
 MF.writeOutput contents
 result <- MF.computation 12 22
 MF.writeOutputInt result

main :: IO ()
main = testInterpreter program
```

: Running the `testInterpreter` on the program

Which yields the output,

```
Test file content for: Fake input
34
```

Some quick notes, since creating functors and making the functions to lift our functors into monads are such frequently done, there is some machinery to make it a **lot** more ergonomic. We could replace the whole of [@lst:approaches_free_functors] with a `deriving (Functor)` on our data type and a `{-# LANGUAGE DeriveFunctor #-}` language pragma. Furthermore, by using Template Haskell, we could do away with [@lst:approaches_free_functions] (keeping the `Program` type alias) by using `makeFree ''Effects` along with a `{-# LANGUAGE TemplateHaskell #-}` language pragma at the top. This cuts down a lot on the tedious an erroneous parts of setting up a program using the `Free`-style.

\ \

There is one problem with `Free` though: it has terrible performance. Constantly doing `(f >>= g) >>= h` behaves as bad as `(a ++ b) ++ c` (i.e. list concatenation) performance-wise. This brings us to our next section on an improvement over `Free`, namely `Freer`.


### Freer
TODO: Explain how this builds upon `Free` and the performance benefits along with getting functors for free. GADTs also allows us to put constraints into the signature, as to make `computation` and `writeOutput` more polymorphic.


## Manipulate the Source {#sec:approaches_source}
TODO: Explain how to use e.q. `haskell-src-exts` or `ghc-exactprint` to extract the AST, add the offloading function, and output the program. Preproccessing to be exact.


## Template Haskell {#sec:approaches_template}
TODO: Demonstate a similar usage as the [debug package](http://neilmitchell.blogspot.dk/2017/12/announcing-debug-package.html), i.e. wrapping up functions in additional code.


## Common Challenges {#sec:approahces_common}
Common for most of these approaches---barring the monadic frameworks presented in [@sec:approaches_monadic]---is they all are missing a story for how to handle things on the server side---there needs to be a way to take in an arbitrary function, and somehow route it to the correct call along with its arguments.

One way to handle this is to have a main entry point that imports all the functions that need to be accessible to offloading, but this presents a new problem entirely: cyclic imports are almost guarenteed to happen, and great care will have to be taken to avoid

TODO: Demonstrate the cyclic import (if it actually happens).


## Evaluation {#sec:approaches_evaluation}
The rows for [@tbl:approaches_overview] are reiterated here again, for convenience,

- **C**omplexity of the implementation (very low--very high)
- **A**doptability by the wider community (very low--very high)
- **B**uy-in, for a developer to use the system (very low--very high)
- **G**ranularity of the offloading mechanism (very fine--very coarse)
- **S**erver-side story (none--yes)

--------------------------------------------------------------------------------------------------------------
**Approach**                     **C**            **A**            **B**            **G**               **S**
-------------------------------- ---------------- ---------------- ---------------- ------------------- ---------
Extending the runtime            Very High[^er]   Low              High             Very Coarse[^pure]  None

**unsafePerformIO**              **Low**          **Medium**       **Low**          **Very Fine[^man]** **None**

Compiler/Language                High             Medium           Medium           Coarse
Extension

**Rewrite Rules**                **Low**          **Low**          **Low**          **Very fine[^re]**  **None**

Monadic Framework                Medium           High             Medium           Flexible[^flex]     Yes

**Manipulate the**               High             Medium           Medium           Coarse              Yes
**source**

Template Haskell                 Medium           Medium           Low              Very Fine           Yes
--------------------------------------------------------------------------------------------------------------

Table: Overview of the pros and cons of the different proposals {#tbl:approaches_overview}



[^er]: While technically feasible, it would be a massive undertaking
[^pure]: All pure _known_ functions with saturated arguments
[^man]: Manually controlled by adding function calls before the code that should be offloaded
[^re]: Needs a rewrite rule for every function that should support offloading
[^flex]: Very flexible granularity, since one can simply add more fine-grained effects if the offloading should be more fine-grainde
