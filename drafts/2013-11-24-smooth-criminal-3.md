---
title: Elementary smooth infinitesimal analysis, part 3
tags: math, SIA
---

**Proposition 10:** A function $f: R \to R$ is **continuous** if, whenever $a, b$ are neighboring, then $f(a)$ and $f(b)$ are neighboring. Every function is continuous.

*Proof:*


------
this needs to be relocated to a different post.

Let's revisit the Kock-Lawvere axiom. If we denote the set of all functions $\Delta \to R$ by $R^\Delta$ and define $\phi_{ab}(\epsilon) = a + b \epsilon$, then the Kock-Lawvere axiom says that $(a, b) \mapsto \phi_{ab}$ is a bijection $R^2 \to R^\Delta$.

The Kock-Lawvere axiom intuitively says that any function defined on a very small neighborhood of $0$ (that is to say, $\Delta$) is actually a "linear" function (linear in the sense of *resembling a straight line*, not in the sense of homomorphisms of vector spaces). We can obtain a stronger result from the KL axiom: for any $f: R \to R$ and any $x \in R$, define a function $g_x: \Delta \to R$ by $g_x(d) = f(x+d)$. Then by KL, for all $d \in \Delta$ there's a unique $m \in R$ with $g_x(d) = g_x(0) + md$. This implies that $m$ is the unique number such that $f(x+d) = f(x) + md$ for all $d \in \Delta$.

The intuitive meaning of this last fact is that every function is differentiable at each point. Bell calls this the *Principle of Microstraightness*.

We will actually *define* the derivative of a function $f$ at point $x$ to be the unique $m$ that we proved above. It will be denoted via the usual notation, $f'(x)$.

