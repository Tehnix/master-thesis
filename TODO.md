# Todo

- [ ] Play around with Haxl
  - [ ] https://simonmar.github.io/posts/2015-10-20-Fun-With-Haxl-1.html
  - [ ] https://github.com/facebook/Haxl/tree/master/example/sql
  - [ ] http://gelisam.blogspot.kr/2015/01/haxl-anti-tutorial.html

- [ ] Read up about GHCs runtime system
  - [ ] https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts


- [ ] Play around with Free/Freer monads
  - [ ] http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
  - [ ] https://joashc.github.io/posts/2016-03-23-free-monads.html
  - [ ] https://joashc.github.io/posts/2015-09-13-free-monad-steps.html

- [ ] Study up on exisiting literrature
  - [ ] Brush up on H.1 - There is no Fork- an Abstraction for Efficient, Concurrent, and Concise Data Access (Haxl)
  - [ ] Read H.8 - Freer Monads, More Extensible Effects
  - [ ] Brush up on O.7 - MAUI - Making Smartphones Last Longer with Code Offload
  - [ ] Brush up on O.3 - Mobile App Acceleration via Fine-Grain Offloading to the Cloud
  - [ ] Brush up on O.2 - Cloud-based computation offloading for mobile devices - State of the art, challenges and opportunities
  - [ ] Brush up on O.6 - Rethinking the mobile code offloading paradigm - from concept to practice
  - [ ] Read O.4 - A Hitchhiker's Guide to Computation Offloading - Opinions from Practitioners


#### Thesis outline
- [ ] Abstract
- [x] Introduction
  - [x] What are we exploring in this thesis?
  - [x] Why is it new?
- [ ] The case for offloading <-- meta analysis
  - [ ] Why would we want to do it?
  - [ ] Can it actually save energy?
  - [ ] When to offload
    - [ ] What network conditions?
    - [ ] What about input/output size?
- [ ] Related Work
  - [ ] Read through whitepapers and write down notes to include in this chapter
  - [ ] MAUI
  - [ ] CloneCloud
  - [ ] Other systems?
  - [ ] Play around with Haxl
- [ ] Purely functional programming
  - [ ] Purity and functional programming (Haskell, PureScript, Idris, Elm, etc)
  - [ ] Lazy vs Strict evaluation
  - [ ] Primer on Haskell
- [ ] Approaches to offloading (with Haskell in mind)
  - [ ] Extending the runtime
  - [ ] Mondic framework (MTL-style/Free/Freer) (à la `Haxl` maybe, going beyond)
  - [ ] unsafePerformIO
  - [ ] GHC compiler or language extension (à la how `ApplicativeDo` reorders computations)
  - [ ] Pre-process the source (e.q. haskell-src-exts or ghc-exactprint)
  - [ ] Template Haskell
  - [ ] Evaluation
- [ ] Offloading using a monadic framework (Free/Freer)
  - [ ] Build up concept from small examples
  - [ ] Talk about larger implementation
- [ ] Evaluation & Discussion
- [ ] Future work
- [ ] Conclusion
