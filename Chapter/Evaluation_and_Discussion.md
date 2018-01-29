# Conclusion and Evaluation {#ch:evaluation_and_discussion}
Offloading still remains a relevant pursuit with many opportunities to save energy and enhance performance. There are many factors to take into account when designing a system for offloading, with some of the primary being:

- Network conditions.
  - Latency and \gls{rtt}.
  - Throughput.
  - Packet loss ratio and signal strength.
- Method conditions.
  - Size of input and output.
  - Execution time.

Based on our research of previous systems, we have explored several different approaches to offloading using a purely functional programming language, taking into account how they can help alleviate some of the core problems that trouble other systems:

- Global mutable state.
- Danger of re-execution.

We have then turned one of theses approaches into a small proof-of-concept. With minimal setup, our approach is able to handle small cases, and will be able to scale to more and more computations and events. With a little bit of polish, a lot of the code redundancy can be abstracted out into smaller pieces of code that streamline the \gls{api}.

With the above we are certain that purely functional programming is a good fit for the offloading paradigm, and can provide optimal conditions for designing the various components that are involved in the system.

### Future Work
The future direction of this work could involve going deeper into type safety. For example, dependent types offer an interesting avenue to pursue. With dependent types, one can encode extra information at the type level, such as requiring certain network conditions for code to be offloaded, and can dismiss functions at compile-time if they try to break this condition.

Further work and time could also be invested into creating a library that embody the ideas layed out in this thesis, providing ready made interpreters that can take care of offloading code, and handling all the underlying details for the user.
