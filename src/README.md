To run the various projects:


## Unsafe/
In unsafe we experiment with `unsafePerformIO`:

```haskell
$ stack ghci
>:l Unsafe
*Unsafe> main
"Start:"
"Offloading function simpleFunction..."
[6,7,8]
"End!"
```

```haskell
*Unsafe> offloadFunction "simpleFunction" simpleFunction 3
"Offloading function simpleFunction..."
[6,7,8]
```

## Template/
In template haskell we explore how we can get Haskell to generate code for us, taking care of the tedious parts. We have our Template Haskell functions defined in `TH.hs` and

```haskell
$ stack ghci
>:l Template
Template> main
"Offloading function simpleFunction..."
[6,7,8]

"Offloading function Unsafe.simpleFunction..."
[6,7,8]

"Offloading function Unsafe.simpleFunction..."
[6,7,8]

"Interpret the received instruction and map it to a function call:"
"[6,7,8]"
```

## Rewrite/
In rewrite we are mainly interested in exploring the rewrite rules that happen during compilation. We can see these by calling GHC and passing in two flags to ensure optimizations happen and that it dumps the applied rules:

```haskell
$ stack exec -- ghc Rewrite.hs -O2 -ddump-rule-firings
[1 of 1] Compiling Rewrite          ( Rewrite.hs, Rewrite.o )
Rule fired: SPEC $fShow[] (GHC.Show)
Rule fired: map (GHC.Base) <-- Normal Rule
...
Rule fired: simpleFunction/offload simpleFunction (Rewrite)  <--- Our Rule
Rule fired: unpack (GHC.Base)
...
Rule fired: +# (BUILTIN)
```

## Preprocessor/

```haskell
$ stack ghci
>:l
```

## MonadTransformers/
In monad transformers we explore how we can structure our programs in a way that allow the same program constructs to be performed, but run in various ways depending on the environment.

E.g. if a computation runs on the client side, it might decided to offload. If it's run on the server side, it will always run the computation.

```haskell
$ stack ghci
>:l MonadTransformers
*MonadTransformers> main
"Input: Fake Input, Hostname: localhost"
"Offloading"
34

"Input: Fake Input, Hostname: remote"
34
```

## FreeFun/
In free we explore how we can simplify taking advantage of effects to structure our program in a way that allows us to switch out implementations.

We provide both a manual implementation, to showcase all the details, and a derived/generated implementation, to showcase how much of the mechanical work and boilerplate we can get rid of.

```haskell
$ stack ghci
>:l FreeFun
FreeFun> main
"Manually setup free:"
Test file content for: Fake input
34

"Derived setup free:"
Test file content for: Fake input
34

"Derived setup free with pure interpreter:"
["Test file content for: Fake input","34"]
```

## FreerFun/
Finally, in freer we try to minimize the boilerplate work even further than free.

```haskell
$ stack ghci
>:l FreerFun
FreerFun> main
"Test file content for: Fake input"
"Imagine the offloading happening here"
34
```


## ExtensibleOffload/

```haskell
$ stack ghci
>:l
```
