# Exploring the use of purely functional programming languages for offloading of mobile computations

- [Exploring the use of purely functional programming languages for offloading of mobile computations](#exploring-the-use-of-purely-functional-programming-languages-for-offloading-of-mobile-computations)
  - [Source Code](#source-code)
    - [src/ code](#src-code)
    - [src-reflex/ code](#src-reflex-code)
    - [src-xcode/ code](#src-xcode-code)
    - [src-etlas/ code](#src-etlas-code)
  - [Compiling the Thesis](#compiling-the-thesis)

Note: A much more detailed discussion can be found in the _Justification_ section in the [thesis project description](https://github.com/Tehnix/master-thesis/blob/master/Appendix/Thesis%20Project%20Description.pdf).

This thesis topic is about exploring, as the title implies, the usage of purely functional programming languages for offloading of mobile computations, with the main goal of achieving energy savings. The work will focus on an investigation of tackling some of the commonly found problems in current mobile code offloading approaches, such as a) handling state and b) knowing which parts can are offloadable and which are not (e.q. UI code is not). This will be done by looking at what purely functional programming languages bring to the table, exploring several approaches to solve the domain, along with describing their pros and cons, and finally further developing and implementing one of the approaches to show the feasibility.

The work will be done in collaboration with my supervisors Henrik Lehrmann Christiansen (DTU) and Sung-Ju Lee (KAIST).

As such, the preliminary outline of the work will look like,

- Introduction
- The Case for Offloading
- Related work
- Purely Functional Programming
- Approaches to Offloading
- Offloading Using a Monadic Framework
- Conclusion and Evaluation

Chapter 1 serves to motivate why offloading of computations is a worthwhile pursuit, by examining why one would want to offload in the first place, then goes on to give an overview of common areas in which it can lower energy consumption. Finally, we investigate when and under what exact conditions offloading is beneficial---and conversely---when it is not. Chapter 2 then gives an overview of existing systems, on the application level, that have been developed to facilitate offloading of computations. In chapter 3 the reader is taken on a tour of what it means to be a purely functional programming language, what we mean when we say "pure" computation, differences in evaluation model, and finally a primer on Haskell, which serves to familiarize the reader with the syntax of the Haskell language, since this will be the focal point of this thesis. Continuing to chapter 4, a series of different approaches are investigated as to how offloading could be implemented in a purely functional language, with a focus on Haskell so that concrete suggestions can be given. This leads us on to chapter 5, which implements one of the approaches discussed in chapter 4. In chapter 6 we evaluate and discuss the outcome of the approach taken, and conclude on our work.


## Source Code
The source code is structured into several folders.

### src/ code
The `src/` folder contains working code showcasing the different approaches discussed in chapter 4.

- `Unsafe/` contains the code for section 4.2.
- `Rewrite/` contains the ode for section 4.3.
- Section 4.4 on Monadic Framework is divided into three:
  - `MonadTransformers/` showing MTL-style .
  - `FreeFun/` shows `Free`-style.
  - `FreerFun/` shows `Freer`/Extensible Effects-style.
- `Template/` contains the code for section 4.5.
- `Preprocessor/` contains the unfinished code for the dropped section on manipulating the source.

And finally `Playground/` and `ExtensibleOffload/` don't really matter, the former was just a template to copy, and the second was meant to be the implementation for chapter 5, but that now lives in `src-reflex/`.


### src-reflex/ code
The `src-reflex/` folder contains the code for chapter 5, showcasing a proof-of-concept of using `Freer`/Extensible Effects-style on a mobile device. The code uses Nix to build, setting it up via [`reflex-platform`](https://github.com/reflex-frp/reflex-platform). To get set up, go into `src-reflex/reflex-platform/` and run `./try-reflex`. It is recommended to consult the reflex-platform documentation during setup.

`src-reflex/` contains three projects to be combined:

- `common/` contains the code that both the frontend and backend will share, such as types and computations.
- `frontend/` contains the code that goes on the mobile device, by generating it vias GHCJS. You can run `./watch-js.sh` inside `src-reflex/` to get into the GHCJS nix-shell, and then run `./watch-js.sh` **again** while inside the nix-shell to setup compilation on file changes.
- `backend/` contains the code for the server, which is a very simply [servant](https://github.com/haskell-servant/servant) server. You can run `./watch.sh` inside `src-reflex/` to get into the GHC nix-shell, and then run `./watch.sh` **again** while inside the nix-shell to setup compilation on file changes. Finally, `start-server.sh` will start the server.


### src-xcode/ code
The `src-xcode/` folder contains the Xcode project, which constructs a mix of a WebView and some labels that the code can interact with. JavaScript can communicate to the Swift code via GHCJS's JavaScript FFI, and some minor setup in the `ViewController.swift` in Xcode. This is described in the last part of section 5.4.


### src-etlas/ code
The `src-xcode/` folder contains the beginning of an Android project that would have used [Eta-lang](https://eta-lang.org), which is basically Haskell on the JVM. This was explored in parallel with the reflex-dom approach, but was dropped because of time constraints, and limited resources on how to integrate the two (i.e. Eta and Android).


## Compiling the Thesis
The thesis is written using [Pandoc](https://pandoc.org/index.html) Markdown, compiling to LaTeX. The content is contained in `Chapter/`, with the accompanying self-explanatory folders `Appendix/`, `Bibliography/` and `Graphic/`. Finally `Template/` contains the LaTeX templates, for which two of them support pandoc variables, namely `Tempalte/default.tex` and `Tempalte/pandoc-preamble.tex`. It is also worth mentioning that the chapters are pulled from `main.md`, so if you add anything new, it needs to go in there (needed for ordering if they are naturally alphabetical). Furthermore, `metadata.yaml` contains metadata information and settings for the build.

The setup needs a local Tex installation. If you are on macOS you can get a minimal setup going by running `./setup.sh`, which sets up _Pandoc_ with two extensions [_citeproc_](https://github.com/jgm/pandoc-citeproc/blob/master/man/pandoc-citeproc.1.md) and [_crossref_](http://lierdakil.github.io/pandoc-crossref/), [_basictex_](http://www.tug.org/mactex/morepackages.html) containing the LaTeX compilers, and finally a bunch of Tex packages installed using `tlmgr`. If you are on any other OS, inspecting the `setup.sh` file should be enough to get you going.

To compile the thesis, we use a `Makefile`, which means you simply have to run `make all` in the root folder. For convenience just run `./watch.sh`, which compiles the appropriate targets on file changes. I recommend using a PDF viewer that can update to changes, such as [Skim](https://skim-app.sourceforge.io) on macOS.
