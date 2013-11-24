---
title: Elementary smooth infinitesimal analysis, part 2
tags: math, SIA
---

See [part 1](2013-11-09-smooth-criminal.html) for the background to this post.

A subset $A$ of $R$ is **microstable** if for all $a \in A$ and $\epsilon in \Delta$, then $a + \epsilon \in A$.

**Proposition 6:** For any $\epsilon \in \Delta$:

    a) $\neg (\epsilon < 0 \vee 0 < \epsilon)$
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

$$\neg (x < a) \wedge \neg (b < x) \iff \neg (x < a + \epsilon) \wedge \neg (b + \delta < x$$

But $(P iff Q)$ implies $(\neg P \iff \neg Q)$ in intuitionist logic, so we can establish the above by proving

$$(x < a) \vee (b < x) \iff (x < a + \epsilon) \vee (b + \delta < x$$

(we have used that $\neg (A \vee B) \iff \neg A \wedge \neg B$)

If $x < a + \epsilon$ implies $0 < (a - x) + \epsilon$, so by proposition 6d $ (a - x) + \epsilon - \epsilon = a - x > 0$, or $x < a$. The same strategy works for proving $b + \delta < x$ implies $b < x$.

Conversely, $x < a$ means $0 < a - x$, and again by 6d $x < a + \epsilon$. We can similarly prove $b < x \implies b + \delta < x$. This proves the proposition. $\Box$

**Corollary:** Any closed interval is microstable.

*Proof:* If $x \in [a,b]$ and $\epsilon \in \Delta$, $x \in [a - \epsilon, b - \epsilon]$ as well, so $a \leq x + \epsilon \leq b$. $\Box$

**Microcancellation:** If $a, b \in R$, ($\forall d \in \Delta ad = bd$) implies $a = b$.

*Proof:* The function $f: \Delta \to R$ defined by $f(d) = ad$ can, by hypothesis, also be defined by $f(d) = bd$. The Kock-Lawvere axiom implies that $a = b$. $\Box$


**Proposition 8:** None of the following are true:

  a) $\forall \epsilon, \eta \in \Delta$, $\epsilon \eta = 0$
  b) $\Delta$ is microstable
  c) $\forall x, y \in R$, $(x^2 + y^2 = 0)$ implies $x^2 = 0$

*Proof:*

  a) Assume yes and consider the function $f_\eta: \Delta \to R$ defined by $f_\eta(\epsilon) = \eta \epsilon$ for any $\eta \in \Delta$. Each $f_\eta$ is a constant function to zero, which contradicts the Kock-Lawvere axiom.
  b) If $\Delta$ is microstable, then for any $\epsilon, \eta \in \Delta$, $(\epsilon + \eta) \in \Delta$, so $\epsilon^2 + 2 \epsilon \eta + \eta^2 = (\epsilon + \eta)^2 = 0$. In other words, $2 \epsilon \eta = 0$, so $\epsilon \eta = 0$ in contradiction to (a)
  c) For any $\epsilon, \eta \in \Delta$, $(\epsilon + \eta)^2 + (\epsilon - \eta)^2 = 2 \epsilon \eta - 2 \epsilon \eta = 0$. So if the proposition is true, then $(\epsilon + \eta)^2 = 0$ for every pair of infinitesimals, contradicting (b)

$\Box$


**Proposition 9:**  Two points $a, b \in R$ are called **neighbors** if $(a - b) \in \Delta$. The neighbor relation is reflexive and symmetric, but not transitive.

*Proof:* $0 \in \Delta$, establishing reflexivity. If $(a - b) \in \Delta$, then $(b - a) = -(a - b)$ is too. Were transitivity to hold, then from knowing $(a - b) \in \Delta$ and $(b - c) \in \Delta$, we would know that $(a - c) \in \Delta$. But if we assume this to be true, then take any $\epsilon, \eta \in \Delta$. Let $a = \epsilon$, $b = 0$, $c = - \eta$. Transitivity implies that $\epsilon + \eta \in \Delta$, so we have just proved that $\Delta$ is microstable, contradicting 8d. $\Box$

