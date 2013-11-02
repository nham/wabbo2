---
title: Cayley's Theorem for Monoids and Semigroups
tags: math
---

Notational convention: $A^B$ is the set of all functions from $B \to A$.

In group theory, Cayley's theorem states that every every abstract group is isomorphic to a permutation group. We can generalize this result to monoids and even semigroups.

If $M$ is any monoid, then for any $x \in M$ we can form maps on $M$, the *left-* and *right-multiplication* maps.

$$R_x : a \mapsto a \cdot x$$

$$L_x : a \mapsto x \cdot a$$

In words, $R_x$ multiples any monoid element $a$ on the right by $x$, and $L_x$ multiplies on the left. An important fact is that, supposing $e$ is the monoid unit, $e R_x = x = e L_x$.

Note that we are writing function application on the right, so function $f$ applied to element $x$ is written $xf$. This means that if $f: A \to B$ and $g: B \to C$, the composite function $A \to C$ is written $fg$. We use the convention that the "standard" monoid of all set functions on a set $X$ is formed from writing functions on the right. We can make this more explicit by writing the  composite of $f$ and $g$ with the semicolon operator, $f;g$. Then the monoid of all set functions on $X$ is $(X^X, ;, id_X)$. We would get a different monoid on the same set by writing function composition in the usual way, $g \circ f$. This yields a monoid $(X^X, \circ, id_X)$ which is dual to the previous monoid.

So let's just go ahead and prove the theorem for monoids, and then we'll look at how to extend it for semigroups.

**Theorem:** If $(M, \cdot, e)$ is a monoid, it is isomorphic to a submonoid of $(M^M, ;, id_M)$. That is, every monoid is isomorphic to a monoid of set functions.

*Proof:* Letting $x \in M$, we form the map $R: x \mapsto R_x$. For $x \neq y$, $R_x \neq R_y$ since $e R_x = x \neq y = e R_y$, so $R$ is an injection. The question is whether the image of $R$ is a submonoid of $X^X$. Noting that $R_e = id_M$, we must simply prove that $R_x ; R_y = R_z$ for some $z \in M$. Since we're aiming not only for $img R$ being a monoid but for $R$ to be a monoid homomorphism, the obvious candidate is that $z = xy$. It's easy to see that this guess is true since $a(R_x R_y) = (a R_x) R_y = (ax) R_y = (ax)y = a(xy)$. This establishes both that $img R$ is a submonoid of $M^M$ and that $R$ is a monoid homomorphism. $\Box$

It is interesting to note that $x \mapsto L_x$ would not have worked, since $a(L_x L_y) = (yx)a$. We would need to write composition in the opposite order, that is with $\circ$ instead of $;$, in order to make that work. So $x \mapsto L_x$ gives us an isomorphism of $M$ with a submonoid of $(M^M, \circ)$ instead of with $(M^M, ;)$

Can we prove the theorem for semigroups? If so, we will need a modified argument since a key step in the above proof does not go through, namely the fact that right-multiplication maps are not necessarily distinct for distinct elements. Consider the semigroup on the set $\{a, b\}$ defined by $aa = ab = ba = bb = a$. This is an associative operation (clearly), and yet $R_a = R_b$ is the function taking constant value $a$.

Our argument goes through for any semigroup $S$ with a left unit $k$, since for $a \neq b$, $k R_a = a \neq b = k R_b$. But for semigroups without such an element, what can we do?

The trick is to *add an identity*. So if $S$ has a left unit, we are done. Else, turn $S$ into a monoid by adding an element $e \notin S$ that behaves as a two-sided unit. By the previous theorem this is isomorphic to a monoid of set functions, so $S$ is isomorphic to the collection of set functions sans the identity function. This establishes our desired fact:

**Theorem:** Any semigroup is isomorphic to a semigroup of set functions.
