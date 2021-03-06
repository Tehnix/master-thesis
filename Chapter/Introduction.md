\chapter*{Introduction}
\markboth{Introduction}{}
\addcontentsline{toc}{chapter}{Introduction}

As we continue to push the limits of our mobile devices, form factor and size still remain a major consideration during the design process. This often manifests itself in the form of decreasing battery size or cutting down on external ports, while trying to fit everything into a smaller device. As such, many different approaches are explored and utilized to squeeze out every last drop of battery, while retaining as much performance as possible---one of these approaches is to offload heavy or time consuming computations off of the device, so the CPU can remain idle as much as possible and thereby save energy.

\ \

From a mobile application developers' perspective, offloading is a trade-off between complexity of the code base and the limitations of the devices being developed for. More and more things are being pushed onto the device. Take for example Apple's recent addition of an on-device \gls{dnn}, which is part of the A11 chip powering the iPhone 8 and iPhone X [@Simonite2017]. This move makes sense for the use cases they envision, like triggering their voice assistant Siri and adding \gls{ar} features to the camera, which require very low latency to appear believable. On the other hand, once we thread into the domain of computations that are far more time or resource intensive, while not having a strict low latency requirement, we find ourselves in the territory of where offloading computations are highly beneficial, assuming the right network conditions.

Why then, is it we do not see a widespread adoption of frameworks and technologies that allow us to easily offload computations from the device, but instead mainly see it in the small ad-hoc scale of using \glspl{api} to talk to servers, but usually only by necessity (e.g. accessing a database)? Are they too complex, like UpShift which needs to transfer the memory to the server and is limited to ARM-based servers? Do they require too much buy-in, like MAUI essentially being the runtime? Is it a lack of documentation on setup and little exposure to the majority of the developer community outside of academia?

\ \

One thing that is common for all of the existing approaches, is that they are bugged down by some major design limitations, state and side-effects, because of the language paradigm they are implemented in. MAUI and CloneCloud both transfer the state with them, MAUI on the method level and CloneCloud on the thread level. UpShift, as we mentioned, keeps the memory in sync uni-directionally from the client to the server, and MobileCOP requires the programmer to manually pass the state needed on to the platform. Of course, none of them can offload code with side-effects intended for local execution, such as UI changes, I/O or methods sharing native state---the offloading system either needs to be aware of these, which can be extremely difficult, or leave the task up to the programmer.

But what if we could actually differentiate between something that performs I/O, of which shared state and UI are a subclass of, and also guarantee that there are no side-effects in the function we are looking to offload? Enter here the concept of purity, specifically purely functional programming languages. We now have the ability to tackle two of our main design limitations, 1) no need for state: because of referential transparency, we are only concerned with the input of the function, and 2) we can immediately see from a functions' type signature, if it is able to perform side-effects or not. This means we are able to tell which functions we are _certain_ are offloadable, while the remaining part is dependent on the type of I/O it does. For example, fetching a webpage and transforming its output could still be offloaded, while changing the state of the UI is something that cannot.

\ \

This finally brings us to the topic at hand---this thesis explores, as the title implies, the use of purely functional programming languages specifically for offloading of (mobile) computations, while also serving as a meta analysis on the topic of offloading, by examining the currently published research and gathering the results, suggestions and advices into a unified overview. The main goal of the thesis is then to use this knowledge to explore which approaches are feasible, and finally implement one of these based on its merits. The final solution is then tested on an actual mobile device, to see how it performs in various conditions. More succinctly, our goals can be stated as:

- Gathering together the results, suggestions and advice from the literature on offloading.
- Compare existing systems and how they have progressed over time.
- Explain what makes purely functional programming an interesting angle to look at.
- Explore various approaches to making a system for offloading in a purely functional language.
- Implement the most suitable approach.
- Evaluate the approach, and conclude.

\ \

\Cref{ch:case_for_offloading} serves to motivate why offloading of computations is a worthwhile pursuit, by examining why one would want to offload in the first place, then goes on to give an overview of common areas in which it can lower energy consumption. Finally, we investigate when and under what exact conditions offloading is beneficial---and conversely---when it is not. \Cref{ch:related_work} then gives an overview of existing systems, on the application level, that have been developed to facilitate offloading of computations. In \cref{ch:purely_functional} the reader is taken on a tour of what it means to be a purely functional programming language, what we mean when we say "pure" computation, differences in evaluation model, and finally a primer on Haskell, which serves to familiarize the reader with the syntax of the Haskell language, since this will be the focal point of this thesis. Continuing to \cref{ch:approaches}, a series of different approaches are investigated as to how offloading could be implemented in a purely functional language, with a focus on Haskell so that concrete suggestions can be given. This leads us on to \cref{ch:monadic_framework}, which implements one of the approaches discussed in \cref{ch:approaches}. In \cref{ch:evaluation_and_discussion} we evaluate and discuss the outcome of the approach taken, and conclude on our work.
