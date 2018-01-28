# Offloading Using a Monadic Framework {#ch:monadic_framework}
<!-- TODO: Explain the implementation, using the `Freer` approach to set up effects for computations, local IO, remote IO, UI handling, etc. -->
If it was not clear from \cref{ch:approaches} (and the title), we landed on implementing our offloading system using the monadic framework approach, specifically using `Freer`, as it gave both better ergonomics, composability, extensibility and performance than `Free`, and allowed for a cleaner separation than \gls{mtl}.

The implementation will be divided into three main components:

- Common: The shared code between the client and the server, e.g. types and offloadable functions.
- Frontend: Our client code that will reside on the mobile device.
- Backend: Our server code, which will be run in an cloud environment.



## Summary
