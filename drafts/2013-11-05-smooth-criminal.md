---
title: Axioms for smooth infinitesimal analysis
tags: math, SIA
---

[For background, see here](http://xorshammer.com/2008/08/11/smooth-infinitesimal-analysis/) or Bell's book *A Primer of Infinitesimal Analysis*.

I hate the way Bell presents his axioms, so I'm going to mash up O'Connor's axioms with Bell's exercises.


 - $R$ is a commutative, unital ring

 - define **infinitesimals** to be the subset of $R$ of points that *cannot be distinguished from $0$*, meaning all $x$ such that it is not the case that $x \neq 0$. If this were classical logic, then zero would be the only infinitesimal.

 - $\forall x \neq 0, \exists y xy = 1$ (All non-infinitesimals are invertible)

 - There is a binary relation $<$ on $R$ which is irreflexive and transitive

 - $0 < 1$

 - $x < y$ implies $x + z < y + z$

 - $x < y$ and $z > 0$ implies $xz < yz$

 - $if x \neq y$, $(x > y or x < y)$ (distinguishable points are comparable)

 - $x > 0$ implies there's a unique $y > 0$ with $y^2 = x$ (square roots exist)

 - **Kock-Lawvere axiom:** Defining $D = \{d \in R : d^2 = 0\}$, for every $f: D \to R$ and for all $d \in D$, there is a unique $a \in R$ with $f(d) = f(0) + da.

In words, Kock-Lawvere says that every function defined on $D$ is uniquely determined by its value at zero and a certain number $a$, which we might call the *slope* of the function at zero.


*Exercise 1.1* - Prove:
  a) $0 < a$ implies $0 \neq a$.
  b) $0 < a$ iff $-a < 0$.
  c) $0 < 1 + 1$.
  d) $a < 0 or 0 < a$ implies $0 < a^2$

a) $<$ is irreflexive, so $0 = a$ implies $a < a$. Contradiction.
b) $0 < a \implies $-a = 0 - a < a - a = 0$. Add $a$ for the converse.
c) $0 < 1 \implies 1 < 1 + 1$, and transitivity establishes it.
d) clearly $0 < a \implies 0 < a^2$. $a < 0$ implies $-(-a) < 0$, which implies $0 < -a$ by (b). so $-a^2 = a(-a) < 0(-a) = 0$ by an axiom, which (b) implies $0 < a^2$.

*Exercise 1.2* - If $a < b$, then for all $x$ either $a < x$ or $x < b$. Hmmmmmmm.


**Microcancellation:** If $a, b \in R$, ($\forall d \in D ad = bd$) implies $a = b$.

*Proof:* The function $f: D \to R$ defined by $f(d) = ad$ can, by hypothesis, also be defined by $f(d) = bd$. The Kock-Lawvere axiom implies that $a = b$. $\Box$
