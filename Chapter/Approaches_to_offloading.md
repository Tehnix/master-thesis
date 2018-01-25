# Approaches to Offloading {#cha:approaches}
With the theoretical background in place, we can begin to investigate what our possibilities are for a possible implementation of a system for offloading of computations. In this chapter we will take a look at five different proposals for handling offloading, along with their pros and cons, and finally summing them up in [@tbl:approaches_overview]. Each approach will be evaluated based on the following parameters,

- **C**omplexity of the implementation
- **A**doptability by the wider community
- **B**uy-in, for a developer to use the system
- **G**ranularity of the offloading mechanism
- **S**erver-side story
- **P**ortability to other pure functional programming languages

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

As such, we can conclude that extending the runtime is first and foremost a very complex and huge undertaking. Further more it would require quite the buy-in from the users, since they would have to use a custom version of the \gls{ghc} compiler, seeing as it would probably be hard (and would take years due to the feature cycle of \gls{ghc}) to get it into the main GHC implementation. This will also greatly hurt adoptability---not to mention portability---and the custom \gls{ghc} compiler would have to play catch up to the main implementation. And of course, we do not have a server-side story, since this would only enable the offloading mechanism, but nothing in regards to how the server should be implemented.

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

-- | Stub for `shouldOffload`.
shouldOffload :: IO Bool
shouldOffload = pure True

-- | Stub for `offload`.
offload :: String -> (a -> b) -> a -> IO b
offload name f a = do
  -- Offload the function to the server,
  print $ "Offloading function " ++ name ++ "..."
  -- or support falling back to running it locally.
  pure $ f a

offloadFunction :: String -> (a -> b) -> a -> b
{-# NOINLINE offloadFunction #-}
offloadFunction name f a =
  if unsafePerformIO shouldOffload
    then unsafePerformIO $ offload name f a
    else f a
```

: Rough sketch for an offload function using `unsafePerformIO`

The code does the following:

- The `offloadFunction` function simply take in a function name, a function and an argument that should be applied,
- calls `shouldOffload` containing the network state which returns `True` if it makes sense to offload and `False` if not,
- and then either return\
    - a call to `unsafePerformIO`, with `offload` taking care of performing the actual network calls to offload, which needs to be synchronous/blocking\
    - the `f a` function itself, if it should not have offloaded

Admittedly a lot of implementation details are left out, but the underlying concept should still be present in the code. This implementation would allow us to do something like `offloadFunction "heavyComputation" heavyComputation input`, which will then take care of either offloading the funtion, getting back its input and returning that, or simply returning the function application as would happen in normal code.

One thing to note, as Haskell is currently, there is no way to serialize or get the name of a function at runtime, which we would need to be able to tell the server exactly what code it needs to run (i.e. what function to call). This gives us a bit of an akward interface to the function, as shown in [@lst:approaches_unsafe_run].

```{#lst:approaches_unsafe_run .haskell}
main :: IO ()
main = do
  print "Start:"
  print $ offloadFunction "simpleFunction" simpleFunction 3
  print "End!"

simpleFunction :: Int -> [Int]
simpleFunction a = map (+a) $ map (+2) [1,2,3]
```

: Running code using `offloadFunction`

An example of running the code, shown in [@lst:approaches_unsafe_run], will yield the output:

```
"Start:"
"Offloading function..."
[6,7,8]
"End!"
```

<!--
FIXME: Make the code polyvariadic, see:
  - https://mail.haskell.org/pipermail/haskell-cafe/2006-May/015905.html,
  - http://okmij.org/ftp/Haskell/vararg-fn.lhs, https://wiki.haskell.org/Varargs,
  - http://okmij.org/ftp/Haskell/polyvariadic.html#polyvar-fn

One thing that might seem a bit annoying is the fact that `offloadFunction` only works on functions with one argument. While this is certainly not a hindrance to its usage, we can actually make the function polyvariadic---i.e. a function of variable number of arguments of variable type---by extending it a bit, as shown in [@lst:approaches_unsafe_polyvariadic].

```{#lst:approaches_unsafe_polyvariadic .haskell}

```

: Making `offloadFunction` polyvariadic
-->

\ \

The complexity would not be that high, and the buy-in would be low, but the adoptability will probably not be the greatest seeing as `unsafePerformIO` introduces quite a lot of uncertainty in the program, and is generally is avoided if possible. The implementation would need to be battle-tested for people to be willing to trust it at least. As for granularity, it is very fine-grained---perhaps too much---function calls need to be manually placed at places that might benefit from offloading. Finally, there is yet again no server-side story. Finally, portability is a bit up in the air, since `unsafePerformIO` is Haskell specific, but it could very well be implemented using similar constructs if a language supports any form of escape-hatch into IO---for example `unsafePerformEff`[^psunsafe] in PureScript or `unsafePerformIO` in Idris, although both languages are strict, so the evaluation will differ a bit.

[^psunsafe]: https://github.com/purescript/purescript-eff/blob/master/src/Control/Monad/Eff/Unsafe.purs


<!-- ## GHC Compiler/Language Extension {#sec:approaches_extension}
À la how `ApplicativeDo` reorders computations. -->


## Rewrite Rules {#sec:approaches_rewrite}
With \glspl{ghc} support for rewriting functions using rewrite rules, one could imagine coming up with a set of rules to rewrite common functions into ones that are identical, except for additional logic to offload the function itself. For example, combining the approach from [@sec:approaches_unsafe], using `unsafePerformIO`, one could keep the signature of the functions, while discretely adding the additional logic.

One problem with this approach though is that we can only target specific functions with our rewrite rules. For example, as shown in [@lst:approaches_rewrite_simpleFunction], we can target specific functions, in this case the function `simpleFunction`, which will then get rewritten from `simpleFunction x` into `offloadFunction simpleFunction x`.

```{#lst:approaches_rewrite_simpleFunction .haskell}
main :: IO ()
main = print $ simpleFunction 3

{-# RULES
"simpleFunction/offload simpleFunction" forall x.
    simpleFunction x = offloadFunction "simpleFunction"
                                       simpleFunction x
  #-}

simpleFunction :: Int -> [Int]
simpleFunction a = map (+a) $ map (+2) [1,2,3]
```

: Rewrite rule for `simpleFunction` to wrap it in `offloadFunction`

\ \

We can check that the rule has fired by running `stack exec -- ghc Main.hs -O2 -ddump-rule-firings` (or omit the `stack exec --` part if you are running plain \gls{ghc}), which yields the output in [@lst:approaches_rewrite_simpleFunction_output].

```{#lst:approaches_rewrite_simpleFunction_output .haskell}
...
Rule fired: map (GHC.Base)
Rule fired: unpack (GHC.Base)
Rule fired: Class op >>= (BUILTIN)
Rule fired: Class op pure (BUILTIN)
Rule fired: simpleFunction/offload simpleFunction (Main) <--- Our rule
Rule fired: Class op show (BUILTIN)
Rule fired: unpack (GHC.Base)
Rule fired: unpack-list (GHC.Base)
...
```

: Dump of the fired rewrite rules

Running the program, combined with the code from [@lst:approaches_unsafe], gives us the following output,

```
"Offloading function..."
[6,7,8]
```

_Side-note:_ the reason we defined `simpleFunction` as `map (+a) $ map (+2) [1,2,3]` also serves to show that multiple rewrites will take place, since `map f (map g xs) == map (f . g) xs`.

\ \

Adding calls to offload functions are indeed doable using rewrite rules, and also alleviates some of the quirky interface to `offloadFunction`, but it is a very brittle approach, since inlining will cause the rule not to fire, and it is also quite tedious, since every function one wants to offload needs a rewrite rule. As for complexity, it adds little to no real complexity on top of the `unsafePerformIO` approach from [@sec:approaches_unsafe]. Because of the brittleness, it would be most likely not see widespread adoption, even though the buy-in is very small. It is also a not portable, e.g. PureScript still does not have a concrete proposal for rewrites[^psrewrite].

[^psrewrite]: https://github.com/purescript/purescript/issues/2749


## Mondic Framework {#sec:approaches_monadic}
An approach less radical than the others so far, is to utilize the structure of the program itself to enable separating the interpretation/implementation of the program---and thereby the effects---from the semantics of the program itself. This can be done thanks to the way monadic computations are done, by forming a sequence of actions to be performed. This effectively allows us to, as shown in [@fig:approaches_monadic_multiple_interpreters], having multiple ways of interpreting the same program, depending on what we want from it. We could have a pure interpretation for testing, which allows us to, say, store the program output as a list of strings and inspect it later on. Or, a debug interpreter that allows us to step through the program sequences, or just an effectful interpreter running the actual program.

![Multiple interpreters from a single program](Graphic/Multiple Interpreters.png "Multiple interpreters from a single program"){#fig:approaches_monadic_multiple_interpreters width=60% }

There are two popular styles of writing larger applications in Haskell, the first one, and by far most popular, is the \gls{mtl}-style, and the second is using a concept known as `Free` monads. Let us take a look at the \gls{mtl}-style of structuring programs first.


### MTL-style
Monad transformers came about because of a natural limitations of monads, namely that they do not compose. One way to mitigate this, is then to have a set of transformers that know how to go from one specific monad instance, to another.

For example, from the \gls{mtl} we have the `Reader` monad as `MonadReader` with the interface for the monadic operations it support, and then instances for each monad it supports, including its own base case, `ReaderT`, as shown in [@lst:approaches_mtl_reader].

```{#lst:approaches_mtl_reader .haskell}
class MonadReader r m | m -> r where
  ask :: m r

instance Monad m => MonadReader r (ReaderT r m) where
  ask = Control.Monad.Trans.ReaderT.ask
instance (MonadReader r m) => MonadReader r (StateT s) where
  ask = lift ask
```

: The `MonadReader` class and its instances for `ReaderT` and `StateT`

We see that in the first instance---the base case---it simply uses the `ask` operation from `ReaderT` directly, but in the second instance, where `MonadReader` is wrapping `StateT`, it needs to life the ask operation once, because the ask operation will be called from inside the `StateT` monad, and therefore needs to bubble one level up. We now have a general way of composing these transformers, at the expense of writing boilerplate code for the monads that we want to compose with. In fact, for every monad instance you add, you would need $n^2$ instances (at least if you want full composition). For example, to support `MonadReader` and `MonadState`, we need a base case for each and then an instance for `MonadState` supporting `ReaderT` and one for `MonadReader` supporting `StateT`. This can quickly grow, so we are getting this flexibility at the expense of setting up some boilerplate.

Going back to the task at hand, what we really want is a way to create operations---without specifying the type of effects---that can be abstracted over, and later on a concrete type can be choosen, depending on the place we want to use the program, be it client-side, testing or server-side. The way we would do this in the \gls{mtl}-style, is to have our operations as typeclass methods, as shown in [@lst:approaches_mtl_effects], collected in the typeclass `MonadEffects`.

```{#lst:approaches_mtl_effects .haskell}
class (Monad m) => MonadEffects m where
  writeOutput :: Show a => a -> m ()
  getInput :: m String
  computation :: Int -> Int -> m Int
  -- Provide a default implementation for empty instances.
  default writeOutput :: (MonadTrans t, MonadEffects m', m ~ t m'
                      , Show a) => a -> m ()
  writeOutput = lift . writeOutput
  default getInput :: (MonadTrans t, MonadEffects m', m ~ t m')
                      => m String
  getInput = lift getInput
  default computation :: (MonadTrans t, MonadEffects m', m ~ t m')
                         => Int -> Int -> m Int
  computation i1 i2 = lift $ computation i1 i2
```

: Defining our operations with `MonadEffects`

```{#lst:approaches_mtl_effects_mtl .haskell}
instance MonadEffects m => MonadEffects (LoggingT m)
instance MonadEffects m => MonadEffects (ReaderT r m)
instance MonadEffects m => MonadEffects (StateT s m)
instance (MonadEffects m, Monoid w) => MonadEffects (WriterT w m)
```

: Default instances of `MonadEffects` to make it compatible with common MTL classes

`MonadEffects` becomes our general program interface, exposing some of the operations we can exercise precise control over, depending on the environment they are running in. We fill in a bit of boilerplate by making some default implementations, marked by `default`, of each operations, so that we can derive a bunch of \gls{mtl} typeclasses in few lines, as shown in [@lst:approaches_mtl_effects_mtl].

We can now provide two wrapper types around `IO`, also called carrier types, which will serve to make a distinction between our `MonadEffects` instance running on the client, and the one running on the server. We do this, as shown in [@lst:approaches_mtl_effects_wrappers], by making `newtype` wrappers around `IO`, providing a `runX` to unwrap it, and deriving some common classes.

```{#lst:approaches_mtl_effects_wrappers .haskell}
-- Our client IO instances.
newtype Client m a = Client { runClient :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- Our server IO instances.
newtype Server m a = Server { runServer :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)
```

: Our carrier types for the client and server respectively

Before we can run the program, we need to make some concrete implementations of `MonadEffects` for each of these two carriers. We will do these fairly straightforward, as shown in [@lst:approaches_mtl_effects_instances].

```{#lst:approaches_mtl_effects_instances .haskell}
instance MonadEffects (Client IO) where
  getInput = Client $ pure "Fake Input"
  writeOutput = Client . print
  computation i1 i2 = do
    liftIO $ print "Offloading"
    Client . pure $ i1 + i2

instance MonadEffects (Server IO) where
  getInput = Server $ pure "Fake Input"
  writeOutput = Server . print
  computation i1 i2 = Server . pure $ i1 + i2
```

: The concrete instances for our `Client` and `Server`

Now we can finally write our program, which we can interpret both on the client and server side, as shown in [@lst:approaches_mtl_effects_program], sprinkled with a `Reader` to show that our boilerplate for the \gls{mtl} classes.

```{#lst:approaches_mtl_effects_program .haskell}
program :: (MonadEffects m, MonadReader Env m) => m ()
program = do
  env <- ask
  inp <- getInput
  writeOutput $ "Input: " ++ inp ++ ", Hostname: " ++ envHost env
  res <- computation 12 22
  writeOutput res

data Env = Env { envHost :: String }

main = do
  runClient . runReaderT program $ Env { envHost = "localhost" }
  runServer . runReaderT program $ Env { envHost = "remote" }
```

: Our program running both the client and server instances

With this, we should have demonstrated how to structure a program, and separating the execution of these. Still, the separation does not feel as clean as we could wish for, and there is a lot of boilerplate involved (even though we cut down on this extensively). This actually brings us on to our next apporach; the `Free` monad.


### Free Monads
The `Free`-style of writing a program allows for a very clean separation of the semantics of the program and the interpretation of it, by first structuring a form of \gls{ast} of the program, and then constructing different interpreters depending on how you want to run the program.

`Free` monads are a concept from category theory, which when used in Haskell gives us a monad for free for our data types, as long as we implement the functors for them ourselves. If you squint a bit, the data type for `Free`, shown in [@lst:approaches_free_definition], looks a lot like the data type for list, and in fact it is a fair intuition of how the structure of a program written in `Free`-style is built up.

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

Our data type parameter, `next`, is the continuation that will allow us to go through the program. The first parameter to `ReadFile` is the filename, meaning an argument to `ReadFile`, while the second argument is the output into the continuation, `(String -> next)`, meaning it outputs a `String` to the next step. Now we can define our `Functor` instance for our data type, as defined in [@lst:approaches_free_functors]---we need this, because `Free` only allows us to recover the monad by assuming the functor operations.

```{#lst:approaches_free_functors .haskell}
instance Functor Effects where
  fmap f (ReadFile s g) = ReadFile s (f . g)
  fmap f (WriteOutput s g) = WriteOutput s (f g)
  fmap f (WriteOutputInt i g) = WriteOutputInt i (f g)
  fmap f (GetInput g) = GetInput (f . g)
  fmap f (Computation i1 i2 g) = Computation i1 i2 (f . g)
```

: The `Functor` instance for our `Effects` data type

This is fairly straight forward; the `f` of the `fmap` either gets composed with the continuation of the data type (e.g. `f . g`) if there are arguments to the continuation, or applies `f g` if the continuation is a plain `next`.

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
    ReadFile filename next ->
      let fakeFileContent = "Test file content for: " ++ filename
      in testInterpreter $ next fakeFileContent
    WriteOutput s next -> putStrLn s >> testInterpreter next
    WriteOutputInt i next -> print i >> testInterpreter next
    GetInput next -> let fakeInput = "Fake input"
      in testInterpreter $ next fakeInput
    Computation i1 i2 next -> testInterpreter $ next (i1 + i2)
```

: Interpreter for the `Free Effects` program

The final step is to construct a program, using the functions defined in [@lst:approaches_free_functions], and then apply the interpreter, from [@lst:approaches_free_interpreter], on the program. We will do both at the same time in [@lst:approaches_free_program].

```{#lst:approaches_free_program .haskell}
program :: Program ()
program = do
 filename <- getInput
 contents <- readFile filename
 writeOutput contents
 result <- computation 12 22
 writeOutputInt result

main :: IO ()
main = testInterpreter program
```

: Running the `testInterpreter` on the program

Which yields the output,

```
Test file content for: Fake input
34
```

Some quick notes, since creating functors and making the functions to lift our functors into monads are done so frequently, there is some machinery to make it a _lot_ more ergonomic. We could replace the whole of [@lst:approaches_free_functors] with a `deriving (Functor)` on our data type and a `{-# LANGUAGE DeriveFunctor #-}` language pragma. Furthermore, by using Template Haskell, we could do away with [@lst:approaches_free_functions] (keeping the `Program` type alias) by using `makeFree ''Effects` along with a `{-# LANGUAGE TemplateHaskell #-}` language pragma at the top. This cuts down a lot on the tedious an erroneous parts of setting up a program using the `Free`-style.

\ \

There is one problem with `Free` though: it has terrible performance. Constantly doing `(f >>= g) >>= h` behaves performance-wise as bad as `(a ++ b) ++ c`---that is, list concatenation. This brings us to our next section on an improvement over both the performance of `Free` and its interface, namely `Freer`.


### Freer Monads
`Freer`, introduced in [@Kiselyov2015], changes up the interface, but the overall idea is the same as `Free`---to create a clean separation between the program semantics and the implementation details.

The first difference comes from our data type, which is now a \gls{gadt}, as shown in [@lst:approaches_freer_data]. This also gives us the power to use constraints on our types, and we can do away with the two `writeOutput` functions, and simply require the argument to be `Show`able.

```{#lst:approaches_freer_data .haskell}
data Effect n where
  ReadFile :: String -> Effect String
  WriteOutput :: Show a => a -> Effect ()
  GetInput :: Effect String
  Computation :: Int -> Int -> Effect Int
```

: Our `Effect` GADT that models our program

As we can see, we also get a more function-like syntax, and do away with our explicit continuations. Another thing to note is that `Freer` goes beyond `Free` and gives us not just `bind` and `join` for free, but also `Functor`, meaning we do not have to write or derive these instances anymore.

The next step is to model our functions, which are then ones we will use when writing our `Freer` programs. It follows much the same way as for `Free`, in that we define a function for each constructor in our \gls{gadt}, but other than that it is a quite different type signature and we also use a different function to lift it into `Freer`, as shown in [@lst:approaches_freer_functions].

```{#lst:approaches_freer_functions .haskell}
readFilename :: Member Effect effs => String -> Eff effs String
readFilename = send . ReadFilename

writeOutput :: (Member Effect effs, Show a) => a -> Eff effs ()
writeOutput = send . WriteOutput

getInput :: Member Effect effs => Eff effs String
getInput = send GetInput

computation :: Member Effect effs => Int -> Int -> Eff effs Int
computation i1 i2 = send $ Computation i1 i2
```

: Functions lifting the GADT constructors into our effects

We are now ready to write our interpreter, as shown in [@lst:approaches_freer_interpreter]. Here we, much like with `Free`, simply pattern match on each of the type constructors from our `Effect` \gls{gadt}, and perform the actions which the specific interpreter should take. A thing to note is the new syntax in the type signature, `'[Effect, IO]` (note the tick, `'`, in front of the list). This is a type-level list, and tells us that our effects consists of `Effect` and `IO`. We could add more here if we wanted to, making them composable.

```{#lst:approaches_freer_interpreter .haskell}
runEffect :: Eff '[Effect, IO] a -> IO a
runEffect = runM . interpretM (\e -> case e of
  ReadFilename filename -> pure $ "Test file content for: "
                                  ++ filename
  WriteOutput s -> print s
  GetInput -> pure "Fake input"
  Computation i1 i2 -> pure $ i1 + i2)
```

: Interpreter for our `Freer` program

We can also see, from [@lst:approaches_freer_interpreter], that we do not have to manually end with the continuation to the next call of the interpreter anymore, removing some boilerplate code.

And finally we can run the interpreter on a program, as we have done in [@lst:approaches_freer_interpreter].

```{#lst:approaches_freer_interpreter .haskell}
program :: Eff '[Effect, IO] ()
program = do
 filename <- getInput
 contents <- readFilename filename
 writeOutput contents
 result <- computation 12 22
 writeOutput result

main = runEffect program
```

: Running the `Freer` interpreter on our program

Which yields the output,

```
Test file content for: Fake input
34
```

exactly like `Free`.

\ \

We have now seen a way we can structure our program so that we can seperate our semantics from our implementation, giving us great freedom in moving bits of the program execution around as we see fit. We can choose certain effects to be offloadable, and then have more or less the same interpreter on the server-side, just without the choice to offload. Another benefit is that data types are serializable, whereas functions are not. We have seen this problem arise for example in [@sec:approaches_unsafe], where we had to pass a `String` with the function name/id on, so that we could identify it on the server-side.

We gain a lot for very little real complexity, while also maintaining an approach that is both easily adoptable---and in fact already in use, albeit for different goals---and also very portable to other languages, with sufficient enough type systems (like Idris or PureScript). We have a server-side story, and a flexible coarsing for choosing what to offload.

<!-- ## Manipulate the Source {#sec:approaches_source} -->
<!-- TODO: Explain how to use e.q. `haskell-src-exts` or `ghc-exactprint` to extract the AST, add the offloading function, and output the program. Preproccessing to be exact.
- http://mpickering.github.io/posts/2015-07-23-ghc-exactprint.html
- https://www.reddit.com/r/haskell/comments/3edts8/announcing_ghcexactprint_a_new_foundation_for/
- https://stackoverflow.com/questions/15784076/parsing-unicodesyntax-with-haskell-src-exts
- https://hackage.haskell.org/package/ghc-exactprint
- https://hackage.haskell.org/package/haskell-src-exts
 -->


## Template Haskell {#sec:approaches_template}
\acrfull{th}---first introduced in [@Sheard2002] but greatly improved since--- is a feature of \gls{ghc} that adds meta-programming functionality to Haskell. \gls{th} is commonly used to automatically derive typeclass instances for you data types. For example, the \gls{json} parsing and encoding library, `aeson`[^aeson], can create encoders and decoders, to and from \gls{json}, simply based on your data type, by using \gls{th}. Another common usage is for \glspl{dsl} to be embedded inside Haskell code, or loading resources and files during compile time.

In short, \gls{th} allows us to generate code at compile time, so we perform actions that might not yet be supported by the host language itself, or simply cut down on boilerplate code. This functionality is hosted in the `Q` monad, which exposes all the functionality we need for doing our meta-programming via \gls{th}.

[^aeson]: https://hackage.haskell.org/package/aeson

\ \

One way we could use this meta-programming feature, and make it work for us, is by utilizing it to generate the code we need for offloading our function, while also constructing the logic that we need for the server-side of the offloading equation to work. It can also make the interface a bit smoother, by letting \gls{th} handle getting the function name that the server-side needs, using reification.

An example of how this could be done is finely demonstrated in the `debug`[^debug] package, which wraps a function inside a quasiquoter, as shown in [@lst:approach_th_debug], and then generates code to debug the function and view it in a web interface.


```{#lst:approach_th_debug .haskell}
debug [d|
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []
   quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
       where (lt, gt) = partition (<= x) xs
   |]
```

: Example of the `debug` packages' quasiquoter (from its hackage documentation)

[^debug]: https://hackage.haskell.org/package/debug

The `[d| ... |]` is quatation syntax for producing a declaration, and has the type `Q [Dec]`. There is also `[t| ... |]` giving `Q Type` for types, `[p| ... |]` giving `Q Pat` for patterns and finally `[e| ... |]` (or simply `[| ... |]`) giving `Q Exp` for expressions. An example, shown in [@lst:approach_th_runq_e], would be running our offload function call through the expressions quasiquoter, to generate the Template Haskell \gls{ast} for us.

```{#lst:approach_th_runq_e .haskell}
*Main> runQ [e| offloadFunction "simpleFunction" simpleFunction 3 |]
AppE
  (AppE
    (AppE
      (VarE Main.offloadFunction)
      (LitE (StringL "simpleFunction"))
    )
    (VarE Simple.simpleFunction)
  )
  (LitE (IntegerL 3)
)
```

: Generating a Template Haskell expressions in the REPL

As we see, generating code using \gls{th} we are essentially manually constructing an \gls{ast} and manipulating that towards our goal. This does give us great power, but at the same time allows us to shoot ourselves in the foot very easily.

\ \

So, let us set up some design goals for our \gls{th} system; we want it to,

- remove duplicate (and thereby error-prone) arguments when calling the offloading function, and
- help us with the server-side of things.

To do this, we could imagine making a \gls{th} function, that will simply take in the function, and its arguments, add the `offloadFunction` and neccessary additional arguments in front, and create a mapping stating what name it passed on to `offloadFunction` and which function this should call on the server-side.

We tackle the first goal, by creating a function that will automatically lookup the name of the function, and construct the actual expression for `offloadFunction`. The \gls{th} code to derive this is shown in [@lst:approach_th_deriveoffload].

```{#lst:approach_th_deriveoffload .haskell}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Offload (offloadFunction)

deriveOffload :: Name -> Q Exp
deriveOffload name =
  [e|
    offloadFunction n $a
    |]
  where
    a = varE name
    n = showName name
```

: Deriving the full offloading expression via \gls{th}

To explain what is going on: we take in the function as an argument, in the form of `'functionName`---notice the single ' tick in the beginning, which is a \gls{th} special syntax to pass a function as a `Name`---which we can then convert into a string with the name, via `showName name`, and also keep the original expression, via `varE name`. We then splice in the function (i.e. the original expression) using  `$a`, inside our quasiquotation.

Alternatively, we can create our own quasiquote that uses `deriveOffload`, as shown in [@lst:approach_th_deriveoffload_quasi].

```{#lst:approach_th_deriveoffload_quasi .haskell}
import Data.List (dropWhileEnd, dropWhile)
import Data.Char (isSpace)
import Language.Haskell.TH.Quote  (QuasiQuoter(..))

off :: QuasiQuoter
off = QuasiQuoter
  { quoteExp = \n -> do
      let name = dropWhileEnd isSpace $ dropWhile isSpace n
      maybeName <- lookupValueName name
      case maybeName of
        Just name' -> deriveOffload name'
        Nothing -> fail $ "The function '" ++ name
                          ++ "' is either not in scope or"
                          ++ " does not exist"
  , quotePat = error "Doest not support using as pattern"
  , quoteType = error "Doest not support using as type"
  , quoteDec = error "Doest not support using as declaration"
  }
```

: Wrapping our `deriveOffload` inside a quasiquoter `[off|...|]`


Our new functions, `deriveOffload` and `[off|...|]`, are then called, as shown in, [@lst:approach_th_deriveoffload_usage].

```{#lst:approach_th_deriveoffload_usage .haskell}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
main :: IO ()
main = do
  -- Original.
  print $ offloadFunction "simpleFunction" simpleFunction 3
  -- Two new Template Haskell approaches.
  print $ $(deriveOffload 'simpleFunction) 3
  print $ [off|simpleFunction|] 3
```

: Using `deriveOffload` and `[off|...|]`

Currently we win very little, other than removing the chance of giving the wrong function name as a `String` argument to `offloadFunction` (which would mean calling the wrong function on the server-side.) But we are not done yet! For this to actually give us enough benefit that we would choose this over the original syntax, we can generate the server-side endpoint for the derived code. Let us take a closer look at how we could do that.

\ \

We would like our server-side routing function to act as an entry point for the server-side, and then have a mapping of `String`s to actual functions. If we could make sure a piece of \gls{th} code ran at the end, we could build up a list of function names to functions, but unfortunately we have no guarentee of order in the compilation. So, we need to divide up our compilation processes into a client compilation, which features the `deriveOffload` code, and a server compilation, which would construct the endpoint from the information made available from the finished client compilation. As such, one way to go about it is to generate a file consisting of all the mappings we need, and then have the server compilation read in this file and generate the Haskell code, via \gls{th}, that we need.

Let us first extend the `deriveOffload`, as shown in [@lst:approach_th_deriveoffload_extended], to also write out the mapping in a file, in the format of `functionString:function`, separating each mapping by a newline.

```{#lst:approach_th_deriveoffload_extended .haskell}
extendedDeriveOffload :: Name -> Q Exp
extendedDeriveOffload name = do
  let n = showName name
  runIO $ appendFile "FunctionMapping.txt" (n ++ ":" ++ n ++ "\n")
  [e| offloadFunction n $(varE name) |]
```

: Extending `deriveOffload` to write the function mappings to a file

We can now use this file in a later compilation process, to gather all the mappings and create endpoints that will call the specific functions. In [@lst:approach_th_deriveoffload_extended_generate_endpoints], we read in the file, remove duplicate mappings, and then generate a case for each mapping, point to its function.

```{#lst:approach_th_deriveoffload_extended_generate_endpoints .haskell}
deriveEndpoints :: String -> Q [Dec]
deriveEndpoints path = do
  let g (s:f:[]) = (LitP $ StringL s, VarE (mkName f))
  content <- runIO (readFile path)
  addDependentFile path -- Recompile on filechange.
  lcBody <- [e|error "Undefined mapping"|]
  let mappings = map (splitOn ":") (lines content)
      clauses = zipWith
        (\body pat -> Clause [pat] (NormalB body) [])
        (map (snd . g) mappings) (map (fst . g) mappings)
      lastClause = [Clause [VarP (mkName "s")] (NormalB lcBody) []]
  pure [FunD (mkName "endpoint") (clauses ++ lastClause)]
```

: Generating endpoints from the data in the "FunctionMapping.txt" file

And then in our main file, we call it with a simple `$(deriveEndpoints "FunctionMapping.txt")`.

\ \

There is one problem though, we are generating a function that redirects to other functions, and as such we need there to be a uniform type signature. This quickly breaks down the program when you go beyond trivial cases, although we could alleviate some of this with some typeclass trickery and existential types.

Another way to solve this is to evaluate the code through an interpreter, such as `hint`[^hint], which allows us to supply a string that will then get evaluated. We could then get rid of the tedious server-side generation of code, and replace it with the approach shown in [@lst:approach_th_hint], and running the code in [@lst:approach_th_hint_run], which outputs `"[6,7,8]"` (i.e. a `String` with the result).

```{#lst:approach_th_hint .haskell}
interpreterEndpoint :: (String, String) -> String
                    -> Interpreter String
interpreterEndpoint (_,f) arg = do
  let splitFn = splitOn "." f
      fnName = last splitFn
      moduleName = intercalate "." $ init splitFn
  if null moduleName
    then setImports ["Prelude"]
    else setImports ["Prelude", moduleName]
  eval $ fnName ++ arg
```

: Interpreting incoming code on the server-side

```{#lst:approach_th_hint_run .haskell}
endpoint' :: (String, String) -> String -> IO String
endpoint' (s,f) arg = do
  res <- runInterpreter $ interpreterEndpoint (s,f) arg
  case res of
    Left err -> do
      print err
      pure "Failed"
    Right e -> pure e

main = do
  res <- endpoint' ("", "Unsafe.simpleFunction") " 3"
  print res
```

: Running the interpreter on incoming code on the server-side


The endpoint takes in the code, passes it on to the interpreter, which splits it up and loads the module for the function, afterwhich it evaluates it with arguments. The endpoint then returns this result as a `String`, from which we can return it to the client, and the client can handle the type casting to the correct type.

\ \

Through all of this, we have seen that \gls{th} offers a lot of opportunities, but at the cost of quite some complexity. A thing to note is that \gls{th} is known not to be very portable across hardware architectures, and this might pose a bigger problem, then simply how we get the pieces to fit, if we wanted to use it. The buy-in is fairly low, since it would need to be manually added, which also means the granularity is very fine-coarsed.


[^hint]: https://hackage.haskell.org/package/hint


## Evaluation {#sec:approaches_evaluation}
To sum up the evaluation throughout this chapter, the rows for [@tbl:approaches_overview] are reiterated here again, for convenience,

- **C**omplexity of the implementation (very low--very high)
- **A**doptability by the wider community (very low--very high)
- **B**uy-in, for a developer to use the system (very low--very high)
- **G**ranularity of the offloading mechanism (very fine--very coarse)
- **S**erver-side story (no--yes)
- **P**ortability to other pure functional programming languages (no--yes)

-----------------------------------------------------------------------------------------------------------------------------
**Approach**                     **C**            **A**            **B**            **G**               **S**     **P**
-------------------------------- ---------------- ---------------- ---------------- ------------------- --------- -----------
Extending the runtime            Very High[^er]   Low              High             Very Coarse[^pure]  No        No

**unsafePerformIO**              **Low**          **Mid**          **Low**          **Very Fine[^man]** **No**    **Yes**

**Rewrite Rules**                **Low**          **Low**          **Low**          **Very Fine[^re]**  **No**    **No**

Monadic Framework                Mid              High             Mid              Flexible[^flex]     Yes       Yes

**Template Haskell**             **High**         **Mid**          **Low**          **Very Fine**       **Yes**   **No**
-----------------------------------------------------------------------------------------------------------------------------

Table: Overview of the pros and cons of the different proposals {#tbl:approaches_overview}

<!--
Manipulate the                   High             Medium           Medium           Coarse              Yes       None
source

Compiler/Language                High             Mid              Mid              Coarse              No        No
Extension

-->

[^er]: While technically feasible, it would be a massive undertaking
[^pure]: All pure _known_ functions with saturated arguments
[^man]: Manually controlled by adding function calls before the code that should be offloaded
[^re]: Needs a rewrite rule for every function that should support offloading
[^flex]: Very flexible granularity, since one can simply add more fine-grained effects if the offloading should be more fine-grainde

### Honorable Mentions
There were a few approaches we did not thoroughly inspect, but can somewhat quickly argue that their dismissal are justified.

- **Manipulating the source**: This would involve preprocessing the Haskell source code, using something like `ghc-exactprint`[^ghcexactprint], and then add our offloading code (e.g. via `unsafePerformIO` again) in the \gls{ast} of the source code, before restructuring the program. This quite obviously seems like a brittle approach with very little control, with the complexity being in the mid-tier.
- **Compiler/Language Extension**: Another approach left out was to create a language extension that could be turned on, and then would rewrite suitable functions to allow offloading. This was dismissed much for the same reasons as manipulating the source code.


[^ghcexactprint]: https://hackage.haskell.org/package/ghc-exactprint


## Summary
We have explored several ways to approach the design of our offloading system, with each their respective strengths and drawbacks, although strongly indicating that the monadic framework, presented in [@sec:approaches_monadic], seems to be the most flexible and powerful approach.

Common for most of these approaches---barring the monadic framework, and Template Haskell in [@sec:approaches_template]---is that they all are missing a story for how to handle things on the server side; there needs to be a way to take in an arbitrary function, and somehow route it to the correct call along with its arguments.

One general way to handle this, as we see in [@lst:approach_th_hint], is to place an interpreter as the endpoint on the server-side, and this would perhaps be the most flexible way to set things up, although not entirely desirable.

We land on the monadic framework, using `Freer` (or, extensible effects), as the approach we will proceed with in \cref{cha:monadic_framework}, and use for our implementation. It allows us to cleanly separate our program semantics from implementation, allowing us to do different things on the server and client, for the same program. It also is the only one that provides both a story for server-side, and also is portable across languages.
