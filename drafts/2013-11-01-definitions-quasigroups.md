---
title: Definitions of quasigroups
---

Quasigroups are, like monoids, a generalization of groups. Unlike monoids, they are not necessarly associative. We will first take a look at equivalent definitions for this notion.

A **magma** is a set equipped with a binary operator. We suppose only that the binary operator is a well-defined function, i.e. the set is closed under the operation.

**Definition 1:** A **quasigroup** $(Q, \cdot)$ is a magma that obeys *unique solvability law*: for all $a, b \in Q$, there is a unique $x$ and unique $y$ in $Q$ such that 

 - $a \cdot x = b$
 - $y \cdot a = x$.

This fact also holds for groups by virtue of the existence of inverses, but note that we have not assumed that quasigroups have such inverses.

**Definition 2:** A **quasigroup** $(Q, \cdot)$ is a magma such that every right and left  multiplication map $R_x : a \mapsto a \cdot x$ and $L_x : a \mapsto x \cdot a$ is a bijection.

Definitions 1 and 2 are equivalent, and the proof of this is perhaps clearest by going through an intermediate definition:

**Definition 3:** A **quasigroup** $(Q, \cdot)$ is a magma that obeys the *solvability law* and the *cancellation law*: for all $a, b \in Q$:

 - there is a $x$ such that $a \cdot x = b$
 - there is a $y$ such that $y \cdot a = b$
 - $ax = bx$ implies $a = b$ for any $x$
 - $xa = xb$ implies $a = b$ for any $x$

Definition 3 is a direct translation of definition 2 into operators: solvability corresponds to surjectivity and cancellation corresponds to injectivity, so definitions 2 and 3 are equivalent.

To establish the equivalence of definitions 1 and 3, note that if $a x_1 = b = a x_2$, by cancellation $x_1 = x_2$. So Definition 3 implies Definition 1. Conversely, we need only prove that unique solvability implies cancellation. But if $a x_1 = a x_2$, then both $x_1$ and $x_2$ solve $a _ = b$, so they must be the same.
