---
title: Elementary smooth infinitesimal analysis, part 1
tags: math, SIA
---

Maybe a brief section on intuitionist proof strategies should go here? At the very least mention how to prove that (A or B) implies C via (A implies C) and (B implies C).

This is also used repeatedly: not(A or B) iff (not A and not B)

Also, (A or B) and (not A) implies B, if I haven't made an error.

[For background, see here](http://xorshammer.com/2008/08/11/smooth-infinitesimal-analysis/) or Bell's book *A Primer of Infinitesimal Analysis*.

I hate the way Bell presents his axioms, so I'm going to mash up O'Connor's axioms with Bell's exercises.


**Algebra axioms**
 - $R$ is a commutative, unital ring

 - $\forall x \neq 0, \exists y xy = 1$ (All points distinguishable from zero are invertible)

**Order axioms**
 - There is a binary relation $<$ on $R$ which is irreflexive and transitive

 - $x < y$ implies $x + z < y + z$

 - $x < y$ and $z > 0$ implies $xz < yz$

 - $if x \neq y$, $(x > y or x < y)$ (distinguishable points are comparable)

 - For all $x$, $0 < x$ or $x < 1$

**Square root axiom**
 - $x > 0$ implies there's a unique $y > 0$ with $y^2 = x$ (square roots exist)

**Kock-Lawvere axiom:**
 - Defining $\Delta = \{d \in R : d^2 = 0\}$, for every $f: \Delta \to R$ and for all $d \in \Delta$, there is a unique $a \in R$ with $f(d) = f(0) + da. The elements of $\Delta$ are called **infinitesimals.**

In words, Kock-Lawvere says that every function defined on $\Delta$ is uniquely determined by its value at zero and a certain number $a$, which we might call the *slope* of the function at zero.

**Proposition 1:** 
    a) $0 < 1$
    b) $0 < a$ implies $0 \neq a$.
    c) $0 < a$ iff $-a < 0$.
    d) $a < 0 or 0 < a$ implies $0 < a^2$
    e) $b > 0$ implies the multiplicative inverse $b^{-1}$ exists and is s.t. $ b^{-1} > 0$

*Proof:*
    a) $0 < 0$ isn't true by irreflexivity, so $0 < 1$.
    b) $<$ is irreflexive, so $0 = a$ implies $a < a$. Contradiction.
    c) $0 < a \implies $-a = 0 - a < a - a = 0$. Add $a$ for the converse.
    d) clearly $0 < a \implies 0 < a^2$. $a < 0$ implies $-(-a) < 0$, which implies $0 < -a$ by (b). so $-a^2 = a(-a) < 0(-a) = 0$ by an axiom, which (b) implies $0 < a^2$. $\Box$
    e) Note that I'm saying "the" because multiplicative inverses are unique in unital rings (in monoids, really). Since $b > 0$, by (b) $b \neq 0$, so a multiplicative inverse $b^{-1}$ exists. If $b^{-1} = 0$, then $0 = 1$, implying $b = b 1 = b 0 = 0$, contradiction. so $b^{-1} \neq 0$, which implies $b^{-1} b^{-1} > 0$ (by (e)). We can multiply both sides by $b$ to obtain $b^{-1} > 0$. 
$\Box$


**Proposition 2:** If $a < b$, then for all $x$ either $a < x$ or $x < b$. 

*Proof:* First assume $b > 0$. Then $b^{-1}$ exists, so for all $x$, $0 < xb^{-1}$ or $xb^{-1} < 1, which implies $0 < x$ or $x < b$ after multiplying both sides by $b$.

Now, if $a < b$, $0 < x$ or $x < b - a$ (by the last paragraph) for all $x$. So in particular, for any $x$ we have $0 < x - a or $x - a < b - a$. After addition to both sides we obtain the desired statement. $\Box$

Note that we've proven that for all $x \neq 0$, $0 < x^2$. Thus it is not the case that any $x \in \Delta$ is distinguishable from $0$. (Note that this also means that we canot divide by infinitesimals).

But neither can we prove that $\Delta = \{0\}$! Assuming so, by the KL axiom, for every function $f: \Delta \to R$ and for every $d \in \Delta$ we have $f(d) = f(0) + ad$ for a unique $a \in R$. But since $d = 0$ is the only possibility, the equation reduces to $f(0) = f(0) + 0a$. So in fact $a$ is not unique, any $x \in R$ will do, contradicting the KL axiom.

We introduce some new definitions now.

Define an interval $]a,b[ := \{ x \in R : a < x and x < b\}$. This is the SIA-analog of the open interval. To define the closed interval, we define a new relation $\leq$ on $R$ by $a \leq b$ iff $\not (a < b)$. Then the closed interval $[a, b] := \{x \in R : a \leq x and x \leq b\}$.


**Proposition 3:** $]a,b[ = \emptyset$ iff $\not a < b$ iff $b \leq a$.

*Proof:* Note that $\not a < b$ iff $b \leq a$ by definition.

If $]a,b[$ is empty, $a < b$ implies that $a < \frac{a + b}{1+1} < b$ since $1+1 > 1$. Hence $\not (a < b)$. Conversely, if $\not (a < b)$ and $x \in R$, we have that $(a < x)$ and $(x < b)$ implies $a < b$, a contradiction. So for all $x$, $\not (a < x \wedge x < b)$. In other words, $]a, b[ = \emptyset$. $\Box$

**Proposition 4:** 
 a) $\leq$ is reflexive and transitive
 b) $0 \leq 1$
 c) $x \leq y$ implies $x + z \leq y + z$
 d) ($x \leq y$ and $0 \leq t$) implies $xt \leq yt$

*Proof:*
 a) $<$ is irreflexive, so $\leq$ is reflexive. Also, if $x \leq y$ and $ y \leq z$, this by definition means $\not (y < x)$ and $\not (z < y)$. If $z < x$, by proposition 2 we must have $z < a$ or $a < x$ for every $a$. In particular, $z < y$ or $y < x$. But we have assumed that neither of these are provable, so $\not (z < x$. In other words, $x \leq z$.

 b) $1 < 0$ implies $1 < 1$ by transitivity of $<$, so $0 \leq 1$

 c) $y +z < x + z$ implies $y < x$, which is not true since we assumed $x \leq y$.

 d) TODO. I haven't managed to prove this yet :)
