---
title: Elementary smooth infinitesimal analysis, part 2
tags: math, SIA
---

A part $A$ of $R$ is **microstable** if for all $a \in A$ and $\epsilon in \Delta$, then $a + \epsilon \in A$.

**Microcancellation:** If $a, b \in R$, ($\forall d \in \Delta ad = bd$) implies $a = b$.

*Proof:* The function $f: \Delta \to R$ defined by $f(d) = ad$ can, by hypothesis, also be defined by $f(d) = bd$. The Kock-Lawvere axiom implies that $a = b$. $\Box$


The KL axiom intuitively says that any function defined on a very small neighborhood of $0$ (that is to say, $\Delta$) is actually a "linear" function (linear in the sense of *resembling a straight line*, not in the since of homomorphisms of vector spaces). We can obtain a stronger result from the KL axiom: for any $f: R \to R$ and any $x \in R$, define a function $g_x: \Delta \to R$ by $g_x(d) = f(x+d)$. Then by KL, for all $d \in \Delta$ there's a unique $m \in R$ with $g_x(d) = g_x(0) + md$. This implies that $m$ is the unique number such that $f(x+d) = f(x) + md$ for all $d \in \Delta$.

The intuitive meaning of this last fact is that every function is differentiable at each point. Bell calls this the *Principle of Microstraightness*.

We will actually *define* the derivative of a function $f$ at point $x$ to be the unique $m$ that we proved above. It will be denoted via the usual notation, $f'(x)$.

