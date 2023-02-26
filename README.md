# Toy implementation of type inference for polymorphic record calculus

This repository contains a toy implementation of type inference for polymorphic record calculus[[1]](#1). The [`main`](https://github.com/coord-e/impl-polymorphic-records-type-inference/tree/main) branch uses the kinded unification described in [[1]](#1). The [`record-constraints`](https://github.com/coord-e/impl-polymorphic-records-type-inference/tree/record-constraints) branch, on the other hand, implements a solver for record-related constraints separately from unification.

## Bibliography

- <a id="1">[1]</a> Atsushi Ohori. 1995. A polymorphic record calculus and its compilation. ACM Trans. Program. Lang. Syst. 17, 6 (Nov. 1995), 844–895. https://doi.org/10.1145/218570.218572
