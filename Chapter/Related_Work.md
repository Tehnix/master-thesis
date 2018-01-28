# Related Work {#ch:related_work}
There has already been several systems, frameworks, and approaches developed to perform remote execution---that is, offloading code---and these have indeed progressed over time. However most, if not all, suffer from similar constraints and limitations.

To better understand what has already been attempted, let us take a closer look at  the more popular of the approaches that have spawned over time. We will look at each of them, how they function, what their limitations are, and finally try to sum up both the commonly met challenges of remote execution systems, and the decisions that the systems seem to agree on. We will go through these chronologically, so it is more clear how the research has progressed over time. Note that we will not go into each paper's evaluation of when to offload, as that has been summed up in [@sec:case_when_network].

This chapter serves to motivate what design considerations we are trying to tackle in later chapters, namely \cref{ch:approaches} and \cref{ch:monadic_framework}.

## MAUI {#sec:related_maui}
The first system we will explore is MAUI, from [@Cuervo2010], which describes a new approach to offloading code at the time. Previous attempts have mainly been either entirely up to the programmer, along with the logic of under what conditions this should be performed, or a more all-in approach with full process or \gls{vm} migration into the cloud environment, automatically managing the state and code transfer.

MAUI then tries to combine the best of these two approaches. It runs in a \gls{vm}, specifically Microsoft's .NET \gls{clr}, although they mention they could have done the same with the Java \gls{vm}. They then use the fact that \glspl{vm} abstract away the underlying hardware, to run the same code both on the mobile device and remotely on the server, as can be seen in [@fig:related_maui], along with the other components of the MAUI system, to be explained later.

![MAUI Architecture](Graphic/MAUI.png "MAUI Architecture"){#fig:related_maui width=90% }

Using reflection, MAUI identifies which methods to offload, and then during the runtime they profile each method to estimate its cost, defined as the combined CPU and network cost, along with wireless connectivity cost such as bandwidth and latency. They keep updating this estimate during runtime, based on these factors. Mainly wireless connectivity and serialization size will change dynamically. They do mention that they are aware that profiling on _every_ method call adds overhead to the application, but do not go into detail whether they then throttle this or not.

MAUI still needs some intervention from the programmer, in the aspect of manually annotating methods for which MAUI should _consider_ offloading, and making sure _not_ to annotate those that should not be considered. The latter consist of 1) \gls{ui} code, 2) I/O code that only makes sense to run locally (e.g. GPS, sensors, etc), and 3) code that cannot handle being re-run, such as financial transactions. The rest is then taken care of by the MAUI runtime component. If a method finishes offloading successfully, this data is incorporated into the profiler, and if it fails, it falls back to running it locally on the phone, which naturally incurs a small energy penalty.

The client and server proxies handle the serialization and network communication of the offloaded methods, and the MAUI controller handles the authentication and resource allocation for the incoming requests. Finally, the solver is the decision engine of MAUI, which tries to determine, at runtime, if it makes sense to offload piece of code, weighing performance and energy considerations.

Since MAUI works in an imperative programming language, it needs to take into account state transfer. This is done by having the methods that are annotated, be wrapped in additional code, adding a new input argument, the current state, and an additional output argument, the new state. Using reflection, they traverse the in-memory data structures used by the program. Beyond the explicit parameters to the method, they take a conservative approach and add all the current object's members and nested complex objects, along with the state of any static classes and public static member variables---it is clear that this would add significant network transfer overhead, which most likely is not needed. They try to minimize the transfer size by only transferring deltas of the state.

\ \

Cutting this short, let us sum up the limitations and challenges met, along with the rest of the challenges mentioned throughout the paper:

- Manual annotation of methods to offload, where the programmer must provide their own guarantee that their adhere to the three restrictions mentioned.
- Must transfer the state of the object and additional static classes (even though they try to optimize this), because they do not know what is used and what is not.
- Can only offload methods from a single thread at any given point in time.
- Manual intervention must be taken to batch remote operations.

With this in mind, MAUI still achieves significant gains in lowering energy consumption, when it is run on WiFi, and sometimes even on 3G.


## CloneCloud {#sec:related_clonecloud}
The next system is CloneCloud, from [@Chun2011], which came out closely after MAUI, but was actually already envisioned in [@Chun2009], and referenced in the MAUI paper. As such, it is not surprising that this system closely resembles MAUI, in that it features a profiler, and a partitioning of the full code into a smartphone and remote server environment, running on a \gls{vm}.

![CloneCloud Architecture](Graphic/CloneCloud.png "CloneCloud Architecture"){#fig:related_clonecloud width=90% }

In essence, CloneCloud and MAUI share much of the same architectural ideas, as can broadly be seen from [@fig:related_clonecloud] (if you squint hard enough), but differ in implementation details. From [@Jiao2013], we can sum up some of the superficial differences as:

- CloneCloud works at the thread level, whereas MAUI works at the method level.
- Both transfer a substantial amount of state; virtual state, program counter, registers and stack/heap for CloneCloud.
- Neither can offload UI operations, but CloneCloud allows I/O to be offloaded, although it does pin machine specific features to the machine with a special annotation (such as GPS).
- CloneCloud does _not_ require manual annotations, as MAUI does.

We go back to the original paper to dig deeper into these details. The partitioning of CloneCloud (i.e. what to offload) is done offline, and can be run for multiple conditions, creating a database of partitions. This is in stark contrast to MAUI, which does all this work online (i.e. during runtime). The same goes for the solver. As such, CloneCloud lowers the runtime overhead, but does it at the loss of flexibility.

One interesting thing about the thread granularity, is that most mobile runtimes keep \gls{ui} interactions on the main thread, and often discourages doing \gls{ui} operations from elsewhere. This lends itself nicely to, for example, offloading separate worker threads, while keeping the \gls{ui} thread running, and only blocking if it tries to access the worker thread before it has returned. When CloneCloud reaches a migration point, its thread migrator suspends the thread, collects up its state and passes that state on to a node manager for data transfer.

CloneCloud of course manages to gain significant improvements in execution times and energy consumption, when the input size goes up to 1MB and over, on WiFi and in some instances also on 3G.

## UpShift {#sec:related_upshift}
Jumping forward a few years, we will take a look at UpShift, presented in [@Lin2014], which brings some new ideas to the table, mainly motivated by the problem of state, and how to alleviate the pain caused by this.

UpShifts key idea is to continuously replicate the mobile device memory to the cloud, in a so-called \gls{dsm} system, allowing for very fine-grained offloading at the method level. To make this memory replication efficient, they use a technique they called _compressive offloading_, which is based on _compressive sensing_. A simplification of this technique is that it transfers deltas that can be compressively sampled, with low encoding complexity (saves energy on the device), at the cost of higher decoding complexity (which is acceptable, since it happens on the server).

The implementation in the paper only supports unidirectional memory replication, going from mobile device to server, which means that offloaded methods cannot alter state, since this would not be transferred back. Since the memory is replicated, they can skip object serialization when offloading, and save time upon execution, along with enabling them to pass along pointers and address translations in the methods.

In the runtime on the mobile device, they introduce a shim layer, which handles the replication and redirecting method invocation to the server when suited, along with a daemon on the cloud server that decodes and applies the replicated memory, and handle the redirected method invocations. The shim layer takes into account network conditions, much like the profiler in MAUI, and battery levels, along with allowing the user to control offloading by disallowing it altogether or turning it to be always on.

The shim layer is done by creating an alternative memory allocation, `upshift_alloc`, which provides the additional features needed for the memory replication, compared to the default `alloc`, and then using this to allocate new Objective-C objects. These objects using `upshift_alloc` are then also cross-compiled for the server-side.

UpShift shows promising results for operations that take longer to perform than it takes to replicate the memory. At least, this is the case in their evaluation, although it remains to be seen what effect this replication has over the long-term, when it is not just for cases that are well suited for offloading, like the \gls{ocr} they did in their tests.


## MobiCOP {#sec:related_mobicop}
The final mobile code offloading platform, that we will take a look at, is MobiCOP, presented in [@Benedetto2017]. MobiCOP sets out to provide a more practical solution that can be used by developers, compared to the more drastic approaches like MAUI and CloneCloud, which require changes to the underlying \gls{vm} on the phone. It does so by taking a library approach with the goal of compatibility with major \gls{iaas} providers (e.g. \gls{aws} and Google Cloud Platform).

MobiCOP acknowledges that the need for state transfer is one of the biggest challenges in developing code offloading frameworks, and as such tries to avoid it by using a different approach. Instead of offloading arbitrarily at the method level, MobiCOP uses the existing practices of Android development, by extending the core Android `Service` class, and focuses more on long-running intensive background tasks. Because a `Service` class gets its state passed as input parameters upon initialization, MobiCOP already knows what it needs to transfer to the server, instead of having to traverse the heap or other tricks to determine the required state.

![MobiCOP Architecture](Graphic/MobiCOP.png "MobiCOP Architecture"){#fig:related_mobicop width=100% }

The MobiCOP system, shown in [@fig:related_mobicop], also features a decision engine that, like MAUI, learns continuously by making use of past executions to predict future execution time and output size. Because it relies on past executions, it will initially run in one of two modes; concurrent or optimistic. Concurrent executes the function both locally and remotely at the same time, while optimistic mode runs it based on the output of the decision engine.

The Firebase component is used as the main communication layer between the mobile device and the MobiCOP cloud environment. Using the Firebase channels allows for asynchronous and resumable data transfers, and reuses the same sockets which the Android platform uses for push notifications, helping lower energy consumption. If the application state is small enough, it will get packaged along with the Firebase messages, else it will be stored in a temporary file, and transferred to the file server via TUS[^tus].

Even though MobiCOP is "only" a library, it still manages to get energy savings up to a factor 11, compared to executing the code entirely on-device. This alludes that even with a less drastic approach, compared to MAUI, CloneCloud and UpShift, one can still get considerable benefits.

[^tus]: TUS is a protocol for resumable file uploads over HTTP---https://tus.io

<!--
## CloudHaskell {#sec:related_cloud_haskell}
As a slight detour from offloading frameworks, we are going to take a look at CloudHaskell, which is a library for distributed computing. Offloading of computations, or remote execution, is after all just a subset of this, so it makes sense to at least check out some of the relevant landscape for this.

TODO: Fill this out after watching the talk
-->


## Commonalities {#sec:related_limitations}
A crucial element that keeps coming up in all of the previous work is the need for a profiler that can take network conditions into account. Without this, one cannot be sure that the offloading will actually be beneficial to the user, since network conditions change very frequently. As such, a profiling component can be seen as a key component of any offloading system intended for serious use.

Another thing we keep seeing in most systems, except for MobiCOP, is the need for state transfer and handling state. This greatly complicates the offloading systems, and MobiCOP neatly side-steps this since the user needs to supply the `Service` class with the state, via input arguments. This is also one of the key motivators for this thesis, since, as we will see later in [@sec:purely_functional_purity], purity gives us a way to remove the concern about state.

A final thing that can be said about all of these is that they are platform specific. MAUI runs on the \gls{clr}, CloneCloud on the Java Dalvik \gls{vm}, UpShift in Objective-C and MobiCOP using the Android `Service` class. This ties their solutions to a single platform, and will be something we take into consideration in \cref{ch:approaches}, when we go through possible approaches to solving this. In other words, portability matters.


## Summary
As we have seen, there has been a progression from more heavy-handed approaches, that extend or change the \gls{vm} for the target platform, to less intrusive and more practically-oriented approaches, that give the developer control over the usage, and can be deployed to stock mobile devices. In fact, the last example, MobiCOP, shows that simply implementing the offloading system as a library is entirely feasible.

We note that a common trend through all is the need for a decision engine, which---except for CloneCloud---is usually a runtime component that makes decisions based on the current network conditions, so to give the best performance and lowest energy consumption. Many also incorporate continuous learning, which attempts to improve the performance of the offloading system.

Finally, it is clear that state has persistently been a concern when designing these systems/frameworks, since all of them are implemented in imperative languages, where state is unavoidable. Addressing this is one of the key motivations for this thesis.
