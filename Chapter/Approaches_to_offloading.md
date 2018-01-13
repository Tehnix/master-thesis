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

One can quickly see that extending the runtime is a daunting task. For example, taking over function calls would involve making sure the \gls{gc} knows how to allocate/deallocate the potential extra resources, adding support in the profiler and scheduler, making sure we do not affect \gls{stm}, support byte-code generation of the changed functions.

As such, we can conclude that extending the runtime is first and foremost a very complex and huge undertaking. Further more it would require quite the buy-in from the users, since they would have to use a custom version of the GHC compiler, seeing as it would probably be hard (and would take years due to the feature cycle of GHC) to get it into the main GHC implementation. The granularity of offloading would be at the function level, and only pure functions at that---we cannot know if any functions in `IO` are offloadable. One could eventually add support for pragmas that would mark a function as not offloadable (and offloadable for `IO` functions), as done in MAUI.

A final note is that it would also involve implementing the whole system in C, which sort of defeats the purpose of exploring what we can use purely functional programming languages for.

TODO: Come back to this section after reading [@Marlow2006]


## GHC Compiler/Language Extension {#sec:approaches_extension}
À la how `ApplicativeDo` reorders computations.


## Using the `unsafePerformIO` Escape Hatch {#sec:approaches_unsafe}


## Rewrite Rules {#sec:approaches_rewrite}


## Mondic Framework {#sec:approaches_monadic}
MTL-style/Free/Freer, maybe look into `Haxl`.


## Manipulate the Source {#sec:approaches_source}
E.q. haskell-src-exts or ghc-exactprint


## Template Haskell {#sec:approaches_template}


## Evaluation {#sec:approaches_evaluation}
The rows for [@tbl:approaches_overview] are:

- **F**easability of implementing it
- **C**omplexity of the implementation
- **A**doptability by the wider community
- **G**ranularity of the offloading mechanism
- **B**uy-in, for a developer to use the system

-------------------------------------------------------------------
**Approach**             **F**    **C**   **A**   **G**      **B**
----------------------- -------- ------- ------- ---------- -------
Extending               Low[^1]   High    Low    All pure   High
Runtime                                          functions

Compiler/Language       ...      ...   ...   ...        ...
Extension

unsafePerformIO         ...      ...   ...   ...        ...

Rewrite Rules           ...      ...   ...   ...        ...

Monadic Framework       ...      ...   ...   ...        ...

Manipulate the          ...      ...   ...   ...        ...
source

Template Haskell        ...      ...   ...   ...        ...
-------------------------------------------------------------------

Table: Overview of the pros and cons of the different proposals {#tbl:approaches_overview}

[^1]: While technically feasible, it would be a massive undertaking
