---
title: Finite fields and other miscellany
tags: math, algebra, number theory
---

Some brief notes on the ring $\mathbb{Z}_p$ for $p \in \mathbb{Z}$, $p > 1$. First of all, it is straightforward and boring to prove that $(\mathbb{Z}_p, +, \cdot)$ with the usual addition/multiplication defined on the residue classes $[n]$ of congruence modulo $p$ forms a commutative ring with identity. The question is when it forms a field. Note that we are only missing the property that every non-zero has a multiplicative inverse, so we are interested in necessary and sufficient conditions for this situation.

One direction is not too hard via the concept of zero divisors. A ring $(R, +, \cdot)$  has **zero divisors** $a$ and $b$ if $ab = 0$ with $a \neq 0$ and $b \neq 0$. We are interested in ruling this possibility out because it is very unlike how normal numbers behave. Indeed, zero divisors are impossible in any field: if $ab = 0$ and $a \neq 0$, then $0 a^{-1}ab = b$.

This leads to a proof that if $\mathbb{Z}_p$ is a field then $p$ must be prime: if $p$ isn't prime, there exist $a, b$ both greater than $1$ such that $p = ab$. Hence $[a] \cdot [b] = [0]$, so $\mathbb{Z}_p$ has zero divisors and hence could not be a field.

The other direction requires some number theory. Specifically we need Bezout's Lemma (no surprise here, number theory seems to begin with Bezout's Lemma). But first, a pre-lemma! Note that we will write $a \perp b$ for "$a$ is coprime to $b$", i.e. $gcd(a, b) = 1$.

**Lemma:** If $a, b \in \mathbb{Z}$ not both zero and there are $m, n \in \mathbb{Z}$ with $ma + nb = 1$, then $a \perp b$. 

*Proof:* Let $d = gcd(a, b)$, then $d$ divides both $a$ and $b$, so it divides any integer combination of $a$ and b$. Hence it divides $ma + nb = 1$. So $d = 1$. $\Box$

**Bezout's Lemma:** If $a, b \in \mathbb{Z}$ not both zero, then there exist $m, n \in \mathbb{Z}$ such that $ma + nb = gcd(a, b)$.

*Proof:* We first construct the set $C(a, b) = \{ ja + kb : j, k \in \mathbb{Z}\}$ of all integer combinations of $a$ and $b$. We also need $P(a, b) = \{ x \in C(a, b) : x > 0\}$. Since $P(a, b) \subseteq \mathbb{N}$, it has a smallest element $s$.

I claim $gcd(a, b) = s$. First, for any $x \in P(a, b)$, $s \mid x$ since otherwise an integer combination smaller than $s$ exists in $P(a, b)$ by the division algorithm. Since $a, b \in P(a, b)$, $s$ is a common divisor, so that $s \mid gcd(a, b)$ Finally, $s$ is itself an integer combination of $a$ and $b$, so $gcd(a, b) \mid s$. The claim is established. $\Box$

**Corollary:** $a \perp b$ iff $1 \in C(a, b)$. $\Box$

Back to algebra. For any ring with identity, a **unit** is an element $r$ with an $s$ such that $rs = sr = 1$, i.e. ring elements with multiplicative inverses. Clearly the subset of invertible elements of any monoid is a group, so we can talk about the **group of units** for any ring with identity. (This group always exists since $1$ itself is invertible.)

The key to our solution is to prove that for any $\mathbb{Z}_p$, the group of units is exactly equal to the set of residue classes whose elements are coprime to $p$.

First a calculation to make sure that this makes sense: for any $k \in \mathbb{Z}$, if $m \mid (a + kp)$ and $m \mid p$, then $m \mid a$ as well. So in fact any common divisor of $a + kp$ and $p$ is a common divisor of $a$ and $p$ (and clearly vice versa). I only care about this because it establishes that if $a \perp p$, then for all $x \in [a]$, $x \perp p$.

**Group of units/coprime residues:** The group of units of $\mathbb{Z}_p$ is the set $\{[x] : x \perp p\}$.

If $a \perp p$, then by Bezout's lemma there are $m, n$ with $ma + np = 1$, so $[m] [a] = [ma] = [1]$. Conversely, if $[a]$ is a unit in $\mathbb{Z}_p$, then $[a][b]$ = [ab] = [1]$ for some $b$, implying $ab + kp = 1$ for some $k$. By the corollary to Bezout's lemma, $a \perp p$. $\Box$

**Corollary:** If $p$ is prime, then $\mathbb{Z}_p$ is a field.

*Proof:* All the nonzero residues are coprime to $p$, so the non-zero elements are all invertible. $\Box$

Note that this theorem we've proved is strong enough to get us the other direction as well. If $\mathbb{Z}_p$ is a field, its group of units is all non-zero elements, so all non-zero elements are coprime to $p$, so $p$ must be prime. Fancy that.
