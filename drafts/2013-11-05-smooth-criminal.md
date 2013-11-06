---
title: Axioms for smooth infinitesimal analysis
tags: math, SIA
---

Maybe a brief section on intuitionist proof strategies should go here? At the very least mention that proof by contrapositive works and how to prove that (A or B) implies C via (A implies C) and (B implies C).

[For background, see here](http://xorshammer.com/2008/08/11/smooth-infinitesimal-analysis/) or Bell's book *A Primer of Infinitesimal Analysis*.

I hate the way Bell presents his axioms, so I'm going to mash up O'Connor's axioms with Bell's exercises.


 - $R$ is a commutative, unital ring

 - $\forall x \neq 0, \exists y xy = 1$ (All points distinguishable from zero are invertible)

 - There is a binary relation $<$ on $R$ which is irreflexive and transitive

 - $0 < 1$

 - $x < y$ implies $x + z < y + z$

 - $x < y$ and $z > 0$ implies $xz < yz$

 - $if x \neq y$, $(x > y or x < y)$ (distinguishable points are comparable)

 - $x > 0$ implies there's a unique $y > 0$ with $y^2 = x$ (square roots exist)

 - **Kock-Lawvere axiom:** Defining $\Delta = \{d \in R : d^2 = 0\}$, for every $f: \Delta \to R$ and for all $d \in \Delta$, there is a unique $a \in R$ with $f(d) = f(0) + da.

In words, Kock-Lawvere says that every function defined on $D$ is uniquely determined by its value at zero and a certain number $a$, which we might call the *slope* of the function at zero.


*Exercise 1.1* - Prove:
  a) $0 < a$ implies $0 \neq a$.
  b) $0 < a$ iff $-a < 0$.
  c) $0 < 1 + 1$.
  d) $a < 0 or 0 < a$ implies $0 < a^2$

a) $<$ is irreflexive, so $0 = a$ implies $a < a$. Contradiction.
b) $0 < a \implies $-a = 0 - a < a - a = 0$. Add $a$ for the converse.
c) $0 < 1 \implies 1 < 1 + 1$, and transitivity establishes it.
d) clearly $0 < a \implies 0 < a^2$. $a < 0$ implies $-(-a) < 0$, which implies $0 < -a$ by (b). so $-a^2 = a(-a) < 0(-a) = 0$ by an axiom, which (b) implies $0 < a^2$. $\Box$

Note that we've proven that for all $x \neq 0$, $0 < x^2$. Thus it is not the case that any $x \in \Delta$ is distinguishable from $0$. So we can't prove that $\Delta \neq 0$. But neither can we prove that $\Delta = \{0\}$! By the KL axiom, for every $d \in \Delta$ we have $f(d) = f(0) + ad$ being true for every $a \in R$, since $d = 0$ is the only possibility, reducing the equation to $f(0) = f(0) + 0a$. This is a clear contradiction of the KL axiom, so it is not the case that $\Delta = \{0\}$. It just also happens to be the case that we can prove that $\Delta \neq \{0\}$ (no $x \in \Delta$ are distinguishable from $0$). Smooth world is a weird one.

This also means that since infinitesimals cannot be distinguished from zero, we cannot divide by infinitesimals, given that our axioms only say we can divide by non-infinitesimals.

*Exercise 1.2* - If $a < b$, then for all $x$ either $a < x$ or $x < b$. 

Hmmmmmmm. Not sure about this one yet.

---

Define the **infinitesimals** of $R$ to be $\Delta = \{x \in R : x^2 = 0\}$. A part $A$ of $R$ is **microstable** if for all $a \in A$ and $\epsilon in D$, then $a + \epsilon \in A$.

**Microcancellation:** If $a, b \in R$, ($\forall d \in \Delta ad = bd$) implies $a = b$.

*Proof:* The function $f: \Delta \to R$ defined by $f(d) = ad$ can, by hypothesis, also be defined by $f(d) = bd$. The Kock-Lawvere axiom implies that $a = b$. $\Box$


The KL axiom intuitively says that any function defined on a very small neighborhood of $0$ (that is to say, $\Delta$) is actually a "linear" function (linear in the sense of *resembling a straight line*, not in the since of homomorphisms of vector spaces). We can obtain a stronger result from the KL axiom: for any $f: R \to R$ and any $x \in R$, define a function $g_x: \Delta \to R$ by $g_x(d) = f(x+d)$. Then by KL, for all $d \in \Delta$ there's a unique $m \in R$ with $g_x(d) = g_x(0) + md$. This implies that $m$ is the unique number such that $f(x+d) = f(x) + md$ for all $d \in \Delta$.

The intuitive meaning of this last fact is that every function is differentiable at each point. Bell calls this the *Principle of Microstraightness*.

We will actually *define* the derivative of a function $f$ at point $x$ to be the unique $m$ that we proved above. It will be denoted via the usual notation, $f'(x)$.

