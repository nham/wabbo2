---
title: Axioms for smooth infinitesimal analysis
tags: math, SIA
---

Maybe a brief section on intuitionist proof strategies should go here? At the very least mention how to prove that (A or B) implies C via (A implies C) and (B implies C).

This is also used repeatedly: not(A or B) iff (not A and not B)

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

 - $0 < 1$

**Square root axiom**
 - $x > 0$ implies there's a unique $y > 0$ with $y^2 = x$ (square roots exist)

**Kock-Lawvere axiom:**
 - Defining $\Delta = \{d \in R : d^2 = 0\}$, for every $f: \Delta \to R$ and for all $d \in \Delta$, there is a unique $a \in R$ with $f(d) = f(0) + da. The elements of $\Delta$ are called **infinitesimals.**

In words, Kock-Lawvere says that every function defined on $\Delta$ is uniquely determined by its value at zero and a certain number $a$, which we might call the *slope* of the function at zero.

**Proposition:** 
    a) $0 < a$ implies $0 \neq a$.
    b) $0 < a$ iff $-a < 0$.
    c) $a < 0 or 0 < a$ implies $0 < a^2$

*Proof:*
    a) $<$ is irreflexive, so $0 = a$ implies $a < a$. Contradiction.
    b) $0 < a \implies $-a = 0 - a < a - a = 0$. Add $a$ for the converse.
    c) clearly $0 < a \implies 0 < a^2$. $a < 0$ implies $-(-a) < 0$, which implies $0 < -a$ by (b). so $-a^2 = a(-a) < 0(-a) = 0$ by an axiom, which (b) implies $0 < a^2$. $\Box$

Note that we've proven that for all $x \neq 0$, $0 < x^2$. Thus it is not the case that any $x \in \Delta$ is distinguishable from $0$. 

But neither can we prove that $\Delta = \{0\}$! By the KL axiom, for every $d \in \Delta$ we have $f(d) = f(0) + ad$ being true for every $a \in R$, since $d = 0$ is the only possibility, reducing the equation to $f(0) = f(0) + 0a$. This is a clear contradiction of the KL axiom, so it is not the case that $\Delta = \{0\}$. 

This also means that since infinitesimals cannot be distinguished from zero, we cannot divide by infinitesimals, given that our axioms only say we can divide by non-infinitesimals.

Here's a quick exploration of whether we need the $0 < 1$ axiom. If $b > 0$, by (a) $b \neq 0$, so a multiplicative inverse $b^{-1}$ exists. If $b^{-1} = 0$, then $0 = 1$, implying $b = b 1 = b 0 = 0$, contradiction. so $b^{-1} \neq 0$, which implies $b^{-1} b^{-1} > 0$. This implies that $b^{-1} > 0$. We can then use the multiplication axiom here to *prove* $0 < 1$. However, we assumed that $b > 0$ to begin with. Without $0 < 1$, how do we know that there's any such element to begin with? We do not. So we could replace our axiom, it seems, with "there exists some $b > 0$", but this hardly seems any better than assuming $0 < 1$.

There is a mismatch between O'Connor's and Bell's axioms. Bell replaces $0 < 1$ with $\forall x, 0 < x or x < 1$. This implies $0 < 1$, however I have not been able to prove Bell's axiom from all of O'Connor's.

If we take Bell's axiom instead, Exercise 1.2 of Bell's book asks us to prove the following:

*Exercise 1.2* - If $a < b$, then for all $x$ either $a < x$ or $x < b$. 

*Proof:* We prove this in baby steps. If $b > 0$, we also have $b^{-1} > 0$, so for all $x$ $0 < xb^{-1}$ or $xb^{-1} < 1, which implies $0 < x$ or $x < b$.

Now, if $a < b$, $0 < x$ or $x < b - a$ for all $x$. So in particular, for any $x$ we have $0 < x - a or $x - a < b - a$. After addition to both sides we obtain the desired statement.

Ex. 1.2 is used to directly prove Ex 1.3, but to state it we need some definitions:

$$]a,b[ := \{ x \in R : a < x and x < b\}$$

*Exercise 1.3* - $]a,b[ = \emptyset$ iff$ not $a < b$.

*Proof:* If $]a,b[$ is empty, $a < b$ implies that $a < \frac{a + b}{1+1} < b$ since $1+1 > 1$. Hence $\not (a < b)$. Conversely, if $
not (a < b)$, assuming $x \in R$ we have that $(a < x)$ and $(x < b)$ implies $a < b$, a contradiction. So for all $x$, $\not (a < x \wedge x < b)$.

---

Define the **infinitesimals** of $R$ to be $\Delta = \{x \in R : x^2 = 0\}$. A part $A$ of $R$ is **microstable** if for all $a \in A$ and $\epsilon in D$, then $a + \epsilon \in A$.

**Microcancellation:** If $a, b \in R$, ($\forall d \in \Delta ad = bd$) implies $a = b$.

*Proof:* The function $f: \Delta \to R$ defined by $f(d) = ad$ can, by hypothesis, also be defined by $f(d) = bd$. The Kock-Lawvere axiom implies that $a = b$. $\Box$


The KL axiom intuitively says that any function defined on a very small neighborhood of $0$ (that is to say, $\Delta$) is actually a "linear" function (linear in the sense of *resembling a straight line*, not in the since of homomorphisms of vector spaces). We can obtain a stronger result from the KL axiom: for any $f: R \to R$ and any $x \in R$, define a function $g_x: \Delta \to R$ by $g_x(d) = f(x+d)$. Then by KL, for all $d \in \Delta$ there's a unique $m \in R$ with $g_x(d) = g_x(0) + md$. This implies that $m$ is the unique number such that $f(x+d) = f(x) + md$ for all $d \in \Delta$.

The intuitive meaning of this last fact is that every function is differentiable at each point. Bell calls this the *Principle of Microstraightness*.

We will actually *define* the derivative of a function $f$ at point $x$ to be the unique $m$ that we proved above. It will be denoted via the usual notation, $f'(x)$.

