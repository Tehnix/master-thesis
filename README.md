# Exploring the use of purely functional programming languages for offloading of mobile computations

A much more detailed discussion will be presented in the following section, justifying the project.

This thesis topic is about exploring, as the title implies, the usage of purely functional programming languages for offloading of mobile computations, with the main goal of achieving energy savings. The work will focus on an investigation of tackling some of the commonly found problems in current mobile code offloading approaches, such as a) handling state and b) knowing which parts can are offloadable and which are not (e.q. UI code is not). This will be done by looking at what purely functional programming languages bring to the table, exploring several approaches to solve the domain, along with describing their pros and cons, and finally further developing and implementing one of the approaches to show the feasibility.

The work will be done in collaboration with my supervisors Henrik Lehrmann Christiansen (DTU) and Sung-Ju Lee (KAIST) and finally an external contact from, Ivan Perez (Keera Studios<sup>[1](#footnote1)</sup>), which has expertise in Haskell on mobile platforms.

As such, the preliminary outline of the work will look like,

1. Related work
2. Exploring different approaches
3. Settle on the most optimal approach to continue with
4. Implementation of selected approach
5. Evaluation of the implementation both technical and practical
6. Discussion and further work based on the evaluation

A much more detailed discussion can be found in the _Justification_ section in the [thesis project description](https://github.com/Tehnix/master-thesis/blob/master/Appendix/Thesis%20Project%20Description.pdf).

<a name="footnote1">1</a> http://keera.co.uk/


# Miscelleanuous

## Generating yaml format bibliography

```shell
pandoc-citeproc --bib2yaml Bibliography.bib > Bibliography.yaml
```
