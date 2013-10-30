---
title: A higher-order function from algebra
---

I've been learning Haskell recently, so higher order functions abound in my latest programming. However, I just encountered a higher-order function in an algebra textbook I've also been reading that I'd like to share.

I assume you know what a group is.

A **right group action** is a group homomorphism from a group $G$ to the set of all functions on a set $X$ along with composition on the right (so if $f$ and $g$ are composable functions, $xfg$ denotes applying $f$ to element $x$ first, then applying $g$ to the result). A group action is also called a **representation** of $G$.

We can denote group actions as $(A, G, \phi)$ where $G$ is a group, $A$ is a set, and $\phi: G \to A^A$ is a group homomorphism.

The **right regular representation** of a group $G$ is the action which maps each $g \in G$ to a function $x \mapsto xg$. If this were haskell, this would be the result of sectioning the group product on the right, like so: $(\cdot g)$. In other words, each transformation in the group action is the result of multiplying on the right by some element.

A **$G$-homomorphism** $f: (A, G, \phi) \to (B, G, \psi)$ is a function $f: A \to B$ such that for all $g \in G$, $f \psi_g = \phi_g f$, where $\psi_g$ denotes $g \psi$ and ditto for $\phi$.

That was a lot of setup that will probably be incomprehensible to someone not already familiar with the topic! Let's proceed! If $G$ is any group and $(X, G, \phi)$ is any group action of $G$, then any $t \in X$ induces a $G$-homomorphism $L_t: (G, G, R) \to (X, G, \phi)$ defined by $g \mapsto \phi(g)(t)$. In the preceding we denote the right regular representation of $G$ by $(G, G, R)$. We can verify that this is indeed a $G$-homomorphism as follows:

$$h L_t \phi_g = t \phi_h \phi_g = t \phi_{hg} = (hg) L_t = h R_g L_t$$


Okay, so it's a legit $G$-homomorphism, but what does it mean? Well, we are taking every element $g$ of the group, looking up its corresponding transformation in the $(X, G)$ action, which is given by $\phi_g$, and then applying that transformation to $t \in X$. We could write this function in Haskell like this: 

    ($ t) . phi

