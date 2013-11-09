---
title: Elementary smooth infinitesimal analysis, part 2
tags: math, SIA
---

See [part 1](2013-11-09-smooth-criminal.html) for the background to this post.

A subset $A$ of $R$ is **microstable** if for all $a \in A$ and $\epsilon in \Delta$, then $a + \epsilon \in A$.

**Proposition 6:** For any $\epsilon \in \Delta$:
    a) $\not (\epsilon < 0 \vee 0 < \epsilon)$
    b) $0 \leq \epsilon$ or $\epsilon \leq 0$
    c) $\forall a \in R$, $a \epsilon \in \Delta$
    d) $a > 0$ implies $a + \epsilon > 0$

*Proof:*
    a) Either $\epsilon < 0$ or $0 < \epsilon$ implies $\epsilon^2 > 0$ by Proposition 1d, which contradicts $\epsilon \in \Delta$
    b) Immediately from (a)
    c) $(a \epsilon) (a \epsilon) = a^2 \epsilon^2 = a^2 0 = 0$ by commutativity.
    d) it is an axiom that $0 < 1 + \epsilon$ or $1 + \epsilon < 1$. The latter implies $\epsilon < 0$, which (a) shows not to be true. so $0 < 1 + \epsilon$. Now, if $a > 0$, then $a^{-1}$ exists, and by (c) $a^{-1} \epsilon \in \Delta$, implying $1 + a^{-1} \epsilon > 0$. Multiply both sides by $a$ to achieve our dream.
$\Box$

Note that Proposition 6d says that the set $\{x \in R : x > 0$ is microstable. We aim now to prove that closed intervals are microstable.

**Proposition 7:** $\forall a, b \in R$ and $\epsilon, \delta \in \Delta$, $[a, b] = [a + \epsilon, b + \delta]$.

*Proof:* We have to prove for $x \in R$ and $\epsilon \in \Delta$, that $a \leq x \leq b$ iff $a + \epsilon \leq x \leq b + \delta$. By definition of $\leq$ this is

$$\not (x < a) \wedge \not (b < x) \iff \not (x < a + \epsilon) \wedge \not (b + \delta < x$$

But $(P iff Q)$ implies $(\not P \iff \not Q)$ in intuitionist logic, so we can establish the above by proving

$$(x < a) \vee (b < x) \iff (x < a + \epsilon) \vee (b + \delta < x$$

(we have used that $\not (A \vee B) \iff \not A \wedge \not B$)

If $x < a + \epsilon$ implies $0 < (a - x) + \epsilon$, so by proposition 6d $ (a - x) + \epsilon - \epsilon = a - x > 0$, or $x < a$. The same strategy works for proving $b + \delta < x$ implies $b < x$.

Conversely, $x < a$ means $0 < a - x$, and again by 6d $x < a + \epsilon$. We can similarly prove $b < x \implies b + \delta < x$. This proves the proposition. $\Box$

**Corollary:** Any closed interval is microstable.

*Proof:* If $x \in [a,b]$ and $\epsilon \in \Delta$, $x \in [a - \epsilon, b - \epsilon]$ as well, so $a \leq x + \epsilon \leq b$. $\Box$

**Microcancellation:** If $a, b \in R$, ($\forall d \in \Delta ad = bd$) implies $a = b$.

*Proof:* The function $f: \Delta \to R$ defined by $f(d) = ad$ can, by hypothesis, also be defined by $f(d) = bd$. The Kock-Lawvere axiom implies that $a = b$. $\Box$

Let's revisit the Kock-Lawvere axiom. If we denote the set of all functions $\Delta \to R$ by $R^\Delta$ and define $\phi_{ab}(\epsilon) = a + b \epsilon$, then the Kock-Lawvere axiom says that $(a, b) \mapsto \phi_{ab}$ is a bijection $R^2 \to R^\Delta$.

The Kock-Lawvere axiom intuitively says that any function defined on a very small neighborhood of $0$ (that is to say, $\Delta$) is actually a "linear" function (linear in the sense of *resembling a straight line*, not in the since of homomorphisms of vector spaces). We can obtain a stronger result from the KL axiom: for any $f: R \to R$ and any $x \in R$, define a function $g_x: \Delta \to R$ by $g_x(d) = f(x+d)$. Then by KL, for all $d \in \Delta$ there's a unique $m \in R$ with $g_x(d) = g_x(0) + md$. This implies that $m$ is the unique number such that $f(x+d) = f(x) + md$ for all $d \in \Delta$.

The intuitive meaning of this last fact is that every function is differentiable at each point. Bell calls this the *Principle of Microstraightness*.

We will actually *define* the derivative of a function $f$ at point $x$ to be the unique $m$ that we proved above. It will be denoted via the usual notation, $f'(x)$.

