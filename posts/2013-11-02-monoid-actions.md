---
title: Monoid actions
tags: math, algebra
---

First, if $(M, \cdot)$ is a monoid, then the **opposite monoid** $(M^{op}, \Box)$ is formed by using the same set but flipping the monoid product, so that $M^{op} = M$ and for all $a, b \in M^{op}$, $a \Box b := b \cdot a$.

An important example is the monoid $(X^X, ;)$ of all set functions on a set $X$ with composition written on the right, i.e. the function that first applies $f$ and then applies $g$ is written $fg$. We denote this composition operator by the semicolon. The opposite monoid $(X^X)^{op}$ would be formed by writing composition in the standard way: $g \circ f$ denotes the function $f$ followed by $g$. Because I'm weird like that, when referring to the monoid of all set functions I will always mean the first monoid.

A **right monoid action** is a monoid homomorphism from a monoid $M$ to $X^X$. We get a **left monoid action** by considering a monoid homomorphism from $M$ to $(X^X)^{op}$. A useful fact is that if $M$ and $N$ are monoids and $f: M \to N^{op}$ is a monoid homomorphism, then $f^{op} : M^{op} \to N$ defined by $xf^{op} = xf$ is also a monoid homomorphism. So we could have equivalently defined a left monoid action to be a **right monoid action* from $M^{op}$ to $X^X$.

I've taken to calling each transformation $m \phi$ of a monoid action a *hand*, but that is perfectly non-standard terminology.

A monoid action is also called a **representation** of $M$. We will often denote a monoid action by a triple $(X, G, \phi)$, where $\phi : G \to X^X$.

An equivalent definition of monoid action is any subset of $X^X$ indexed by the elements of a monoid $M$ such that for any $a, b \in M$, $f_e = id_M$ and $f_a f_b = f_{ab}$. 

Yet another equivalent definition is an operation $\circ: X \times M \to X$ such that for all $x \in X$, $x \circ e = x$ and $(x \circ a) \circ b = x \circ (ab)$.

I find the "indexed family of set functions" view helpful, espectially for making sense of the next few definitions. Since the same monoid can be represented by set functions on many different sets, we would like some way of comparing representations and some way of mapping one representation into another. An **$M$-subset** of an action $(X, M, \phi)$ is an action $(A, M, \psi)$ where $A \subset X$ and for all $m \in M$, $\psi(m) = $\phi(m) |_A$. In words, an $M$-subset of a monoid action $A$ is an action obtained by restricting each hand of $A$ to a subset of $A$.

A necessary and sufficient condition for $X$ to be an $M$-subset of $A$ is that every hand of $A$ is closed under $X$. For another characterization, we introduce a notion that will become important later: the **orbit** of $a \in A$ is the subset of $A$ that action hands "move" $a$ to. In symbols: $\{a (m \phi) : m \in M\}$. So equivalently, $X$ is an $M$-subset if each orbit of $x$ is contained in $X$.

Since monoid actions are algebraic objects, of course we're going to define homomorphisms between them. Specifically, and $M$-homomorphism $(A, M, \phi) \to (B, M, \psi)$ is any function $f: A \to B$ such that for all $m \in M$, (m \phi) f = f (m \psi)$. Another way of looking at it is that a monoid action is a certain kind of directed graph where the edges are labelled (and where some of the edges can have more than one label), and an $M$-homomorphism is just a graph homomorphism that maps similarly-labelled edges.

To be honest, I'm not sure I fully I grok this definition of $M$-homomorphism. Maybe a homomorphism is whatever makes the first isomorphism theorem work. Under that criteria, the above definition succeeds admirably.

**First isomorphism theorem for $M$-actions**: If $(A, M, \phi)$ and $(B, M, \psi)$ are $M$-actions and $f: A \to B$ is an $M$-homomorphism, then:

 - $(A / ker f, M, \eta)$ with $\eta$ defined by $[a] \eta = a \phi$ is an $M$-action
 - $\pi: A \to A / ker f$ defined by $a \pi = [a]$ is an $M$-homomorphism
 - $Af$ is an $M$-subset of $B$
 - $F: [a] \mapsto a$ is an $M$-isomorphism $A / ker f \to Af$
 - Finally, $f = \pi F \iota$, where $\iota : Af \to B$ is an inclusion and also a $M$-homomorphism.

$\Box$

We can actually say a bit more. Define an $M$-congruence of an $M$-action $(A, M, \phi)$ to be an equivalence relation $\sim$ on $A$ such that $a \sim b$ implies that $(a \phi) \sim (b \phi)$. This condition ensures that we can define an action on the equivalence classes of $A$ via $[a] \eta = a \phi$. $M$-congruences don't just partition the set being acted upon, they do it in such a way that we could collapse each block into a single point and still canonically define an action in terms of the original action.

A big part of the first isomorphism theorem is that the kernel of an $M$-homomorphism gives us an $M$-congruence. The $M$-homomorphism condition is that $f (m \psi) = (m \phi) f$. So if $af = bf$ (i.e. $a \sim b$ under the kernel), then $a (m \phi) f = b (m \phi) f$, or $a (m \phi) \sim b (m \phi)$.

We're going places with this, so stay tuned.
