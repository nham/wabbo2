---
title: Linear independence without subscripts
tags: math, linear algebra
---

Let $(V, \mathbb{F})$ be a vector space and $S$ be a finite subset of $V$. A **scaling** of $S$ is a map $\phi : S \to \mathbb{F}$ together with the multiset $\{ \phi(x) \cdot x : x \in S\}$ of vectors formed by taking the scalar multiple of each vector in $S$ with the field element associated to it by $\phi$. A vector $v$ is a **linear combination** of $S$ if $v$ can be obtained by summing the multiset of some scaling of $S$. 

(We assume here that there is an operation available for summing a finite multiset of vectors. This is easy to believe since we can obviously sum a set of vectors. The only difference is that we may have to some one vector more than once, depending on its multiplicity within the multiset.)

If $\phi$ is the scaling of some set $S$ of vectors, then $\text{img} \phi$ is said to be the *coefficients* of the scaling. A scaling is called **trivial** if the coefficients of the scaling are all zero.

This probably sounds weird if you're familiar with linear algebra (and even weirder if you're not), but here I'm trying (and possibly not succeeding) to keep separate the *result of adding and scaling vectors* (the linear combination) from the *way that result was obtained* (the scaling). This is because two scalings might result in the same vector. In the usual presentations these two are conflated.

The **span** of a set $X$ is the set of all linear combinations of finite subsets of $X$. This reduces, in the case that $X$ is finite, to the set of all linear combinations of $X$, but also accommodates the case of $X$ being infinite as well (since you cannot sum an infinite amount of vectors. (This is not real analysis, we do not have infinite series)).

A set $X$ is **linearly independent** in $V$ if no $x \in S$ is in the span of $S - x$. Here's an alternative characterization:

**Proposition:** $X$ is linearly independent iff for every finite subset $S$ of $X$, the only way to obtain $0$ as a linear combination of $S$ is the trivial scaling.

*Proof:* If $S \subset X$ is finite and a non-trivial scaling of $S$ yields $0$, then one coefficient $\phi(x)$ is non-zero, so the scaling of $S - x$ formed by dividing all the coefficients by $\phi(x)$ yields $-x$. Hence $x$ is in the span of $S - x$, so $S$ (and hence $X$) is not linearly independent.

Conversely, if $X$ is not linearly independent, one $S \subset X$ has an $x \in S$ with $x \in \text{span}(S - x)$, so there is clearly a nontrivial scaling of $S$ that obtains zero (the coefficient of $x$ is $-1$). $\Box$

**Proposition:** If $X \subseteq V$ is linearly independent, then for any finite subset $S$, for any $v \in V$, $v$ is the linear combination of at most one scaling of $S$. 

*Proof:* If $\phi$ and $\psi$ are two scalings of $S$ that result in $v$, then the scaling of $S$ defined by $v \mapsto \phi(v) - \psi(v)$ results in $0$. Since $X$ is linearly independent, $\phi(v) = \psi(v)$ for all $v$, so they are the same. $\Box$

If $Y \subseteq X$, then $Y$ is maximal with respect to some property if adding any element not in $Y$ to $Y$ makes the property not hold. It is minimal with respect to a property if removing any element makes it not hold. Maximal linearly independent sets and minimal spanning sets have interesting properties.

**Proposition:** A maximal linearly independent set is a spanning set. A minimal spanning set is linearly independent.

*Proof:* If $S$ is a maximal linearly independent subset of $V$, then any $x \in V - S$, when added to $S$, yields a set that isn't linearly independent. So one element is in the span of the others, which gives us a scaling of $S + x$ that results in $0$. If the coefficient of $x$ is zero, this contradicts $S$ being linearly independent since one vector in $S$ would be in the span of the other vectors of $S$. so $x \in \text{span} S$.

If $T$ spans $V$ and no proper subset does, then were some $x \in T$ to be in the span of $T - x$,  we would have that $T - x$ and $T$ have the same span, since every scaling of $T$ can be turned into a scaling of $T - x$ by absorbing $x$ into the coefficients of the other vectors. $\Box$

A **basis** is a linearly independent spanning set. The last proposition says that maximal linearly independent sets and minimal spaning sets are bases.

