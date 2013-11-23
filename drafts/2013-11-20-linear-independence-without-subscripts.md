---
title: Finite dimension in vector spaces
tags: math, linear algebra
---

This is an attempt at a minimalist presentation of subspaces, linear independence, spanning sets, bases, and finite dimension.

## Linear combinations and subspaces

Let $(V, \mathbb{F})$ be a vector space and $S$ be a finite subset of $V$. A **scaling** of $S$ is a map $\phi : S \to \mathbb{F}$ together with the multiset $\{ \phi(x) \cdot x : x \in S\}$ of vectors formed by taking the scalar multiple of each vector in $S$ with the field element associated to it by $\phi$. A vector $v$ is a **linear combination** of $S$ if $v$ can be obtained by summing the multiset of some scaling of $S$. 

(We assume here that there is an operation available for summing a finite multiset of vectors. This is easy to believe since we can obviously sum a set of vectors. The only difference is that we may have to some one vector more than once, depending on its multiplicity within the multiset.)

If $\phi$ is the scaling of some set $S$ of vectors, then $\text{img} \phi$ is said to be the *coefficients* of the scaling. A scaling is called **trivial** if the coefficients of the scaling are all zero.

This probably sounds weird if you're familiar with linear algebra (and even weirder if you're not), but here I'm trying (and possibly not succeeding) to keep separate the *result of adding and scaling vectors* (the linear combination) from the *way that result was obtained* (the scaling). This is because two scalings might result in the same vector. In the usual presentations these two are conflated.

We need to redefine the notion of "linear combination" so that it works for infinite sets. A **linear combination of $A$** for some set $A$ is a linear combination of a finite subset of $A$. This clearly does not change the definition for finite sets.

A **subspace** is a subset $W$ of a vector space $V$ such that when you restrict the operations of $V$ to $W$, they are all closed on $W$ and turn $W$ into a vector space. It can be proved that $W$ is a subspace iff every linear combination of $W$ is contained in $W$.

The **span** of a set $X$ of vectors is the smallest subspace that contains $X$. Formally, it is the intersection of all subspaces that contain $X$, which is well-defined because the intersection of any collection of subspaces is a subspace. (Prove it!)

**Proposition:** If $X \neq \emptyset$, then  $\text{span} S$ is the set of all linear combinations in $X$.

*Proof:* From the proposition stated above we know that any subspace containing $S$ must contain all linear combinations of $S$, so the collection of linear combinations is in the span of $S$ as well. But the collection of linear combinations of $S$ is also a subspace, so the other direction of containment holds as well. So they are one and the same. $\Box$

## Independence

A set $X$ is **linearly independent** in $V$ if no $x \in S$ is in the span of $S - x$. Here's an alternative characterization:

**Proposition:** $X$ is linearly independent iff for every finite subset $S$ of $X$, the only way to obtain $0$ as a linear combination of $S$ is the trivial scaling.

*Proof:* If $S \subset X$ is finite and a non-trivial scaling of $S$ yields $0$, then one coefficient $\phi(x)$ is non-zero, so the scaling of $S - x$ formed by dividing all the coefficients by $\phi(x)$ yields $-x$. Hence $x$ is in the span of $S - x$, so $S$ (and hence $X$) is not linearly independent.

Conversely, if $X$ is not linearly independent, one $S \subset X$ has an $x \in S$ with $x \in \text{span}(S - x)$, so there is clearly a nontrivial scaling of $S$ that obtains zero (the coefficient of $x$ is $-1$). $\Box$

**Proposition:** If $X \subseteq V$ is linearly independent, then for any finite subset $S$, for any $v \in V$, $v$ is the linear combination of at most one scaling of $S$. 

*Proof:* If $\phi$ and $\psi$ are two scalings of $S$ that result in $v$, then the scaling of $S$ defined by $v \mapsto \phi(v) - \psi(v)$ results in $0$. Since $X$ is linearly independent, $\phi(v) = \psi(v)$ for all $v$, so they are the same. $\Box$

If $Y \subseteq X$, then $Y$ is maximal with respect to some property if adding any element not in $Y$ to $Y$ makes the property not hold. It is minimal with respect to a property if removing any element makes it not hold. Maximal linearly independent sets and minimal spanning sets have interesting properties.

**Proposition:** If $S$ is linearly independent subset of $V$ and $x \notin \text{span} S$, then $S+x$ is linearly independent.

*Proof:* If not, then one $u \in S$ has $u \in \text{span}(S + x - u)$, meaning some scaling of $S + x - u$ obtains $u$. If the coefficient of $x$ in this scaling is non-zero, then we have a scaling of $S$ that obtains $x$, contradicting $x \notin \text{span} S$. But this implies $u \in \text{span}(S - u)$, contradicting $S$ being linearly independent. $\Box$

**Proposition:** A maximal linearly independent set is a spanning set. A minimal spanning set is linearly independent.

*Proof:* If $S$ is a maximal linearly independent subset of $V$, then any $x \in V - S$, when added to $S$, yields a set that isn't linearly independent. So one element is in the span of the others, which gives us a scaling of $S + x$ that results in $0$. If the coefficient of $x$ is zero, this contradicts $S$ being linearly independent since one vector in $S$ would be in the span of the other vectors of $S$. so $x \in \text{span} S$.

If $T$ spans $V$ and no proper subset does, then were some $x \in T$ to be in the span of $T - x$,  we would have that $T - x$ and $T$ have the same span, since every scaling of $T$ can be turned into a scaling of $T - x$ by absorbing $x$ into the coefficients of the other vectors. $\Box$

A **basis** is a linearly independent spanning set. The last proposition says that maximal linearly independent sets and minimal spaning sets are bases.

**Proposition:** Every finite spanning set has a subset that is a basis.

*Proof:* If $T$ is finite and spans $V$, then if it's not linearly independent, one element is (by definition) in the span of the others. So form a new set by removing that element and repeat the process. This must stop after finitely many iterations, and the result is a basis. $\Box$

The next result is called the *Steinitz exchange lemma*. It's the lever we're going to use to prove that there is a well-defined notion of "dimension".

**Lemma:** If $S$ is linearly independent and $x \in \text{span} S$, then there is a $u \in S$ with $\text{span}(S - u + x) = \text{span} S$ and $S - u + x$ is linearly independent.

*Proof:* Find a $T \subseteq S$ such that $x \in \span{T}$ but $x$ is not in the span of any proper subset of $T$. Pick any $u \in T$. Since $x \notin \text{span}(T - u)$, $T - u + x$ is linearly independent. 

Now to prove that $S - u + x$ is LI. If a non-trivial combination of $S - u + x$ yields $0$, then the sum of a combination of $S - u$ and a scalar multiple of $x$ is zero. But $x$ is a linear combination of $T$, and if the coefficient of $u$ is non-zero in that combination, then $u \in \text{span}(S - u$)$, a contradiction. So $x$ is actually a linear combination of $T - u$, which contradicts how we chose $T$ and $u$. So $S - u + x$ is linearly independent.

It is immediate that $\text{span}(S - u + x) \subseteq \text{span} S$. Conversely, since $x \in \text{span} T$ and for every linear combination of $T$ yielding $x$ we must have the coefficient of $u$ non-zero, then $u \in \text{span}(T - u + x)$. So any linear combination of $S$ with a non-zero coefficient attached to $u$ can be replaced by a linear combination from $S - u + x$. (Obviously any linear combination with a zero coefficient on $u$ is also in $\text{span}(S - u + x)$). $\Box$


