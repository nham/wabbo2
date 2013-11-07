---
title: Monoid and group actions
tags: math, algebra
---

Picking up from [last time](2013-11-02-monoid-actions.html), we will soon need to exclude certain uninteresting kinds of $M$-subsets. Namely, any monoid action $(A, M, \phi)$ is clearly an $M$-subset of itself. So we shall speak of **proper $M$-subsets**, which are those formed by proper subsets of $A$. Also, we can always form the **trivial** $M$-subset by restricting each hand to the empty set (so that each hand is the empty function). We will call an $M$-subset that is both proper and non-trivial a **legit** $M$-subset, and say that any action that has no legit $M$-subsets is **irreducible.**

If $M$ is a monoid and $N$ is a submonoid of $M$, then there is a standard monoid action available to us, the **right multiplication action** $(M, N, R)$ where $m(nR) := mn$. A special case is when $N = M$, called the **right regular representation**. We can get a left monoid action via the **left multiplication action** $(M, N^{op}, L)$, defined similarly.

Recall that groups are special monoids where every element has a (unique) inverse under the product. We can then consider $G$-sets or $G$-actions where $G$ is a group, otherwise known as **group actions** or **group representations**.

Group actions have some very interesting properties. For starters, recall the property that for all $g, h \in G$, $(g \phi) (h \phi) = (gh \phi)$. In particular, $(g^{-1} \phi) = (g \phi)^{-1}$. This means that if $y \in A$ is in the orbit of $x \in A$, then not only is $x$ in $y$'s orbit (if some hand takes $x$ to $y$, then the inverse hand takes $y$ to $x$), but in fact $x$ and $y$ have the same orbit. This means that the hands of a group action are closed under any orbit, i.e. that all orbits of a group action are $G$-subsets.

The above argument can be adapted to show that distinct orbits of group actions are disjoint, which is not true in general for orbits of monoid actions.

I think we can say a bit more. Recall that one necessary and sufficient condition for $X$ to be a $G$-subset of a group action is that $X$ contains all of the orbits of its elements. So orbits are the building blocks of $G$-subsets, in the sense that orbits don't have any legit $G$-subsets (since the orbit of any element is the orbit itself). Another way of saying that is that orbits of group actions are irreducible.

By the definition, $M$-subsets of a monoid action are always a union of orbits. The difference for group actions is that the orbits are *disjoint*. We introduce a new notion to cover this possibility, the **disjoint union** of any collection of $M$-subsets. Unfortunately it is quite unwieldy to define formally, so I'm going to describe it informally. First of all, if a collection of sets are disjoint, then their union is a disjoint union. If the sets are not disjoint, then we have to do a kind of *copying procedure* that generates copies that are disjoint from one another. 

Notice that I said *a* disjoint union, not *the* disjoint union. A disjoint union is basically any set (along with certain inclusion functions) that satisfies a certain universal property. There are generally infinitely many possible disjoint unions.

We might also consider forming the disjoint union of a collection of functions $f_i: X_i \to X_i$. This could be done by first forming the disjoint union of the sets $X_i$ via the copying procedure. Then we could follow it up by "porting" each function to its corresponding subset of the disjoint union. The resulting function is basically all the original functions glued together into one big function.

From the last construction we basically have what we need for the disjoint union of $M$-subsets. Each hand of the action on the disjoint union is the disjoint union of hands from the $M$-subsets. Each subset hand is confined (by construction) to its own little part of the disjoint union, so that the resulting action hands are just the gluing-togethers of the subset hands.

I hope some of the above made sense. Now, back to orbits of group actions.

We can characterize orbits in this way: $X$ is an orbit of a group action $(A, G, \phi)$ iff it is a non-empty, irreducible $G$-subset of $A$. Proof: $(\implies)$ was proven in the above paragraph. Conversely, if $X$ is a non-empty, irreducible $G$-subset, well we know from above that any $G$-subset is a disjoint union of orbits. If, additionally, there are no legit $G$-subsets, then it must be a single orbit, otherwise we could obtain a legit $G$-subset by taking a single orbit subset.

What we've actually just established is the following:

**Proposition:** Any group action of $G$ is the disjoint union of its orbits, i.e. the disjoint union of non-empty, irreducible $G$-subsets.

I would like to now prove the orbit-stabilizer theorem (see [Gowers' post](http://gowers.wordpress.com/2011/11/09/group-actions-ii-the-orbit-stabilizer-theorem/)). This theorem states, in part, that any orbit of a group action is bijective with the collection of right cosets of any stabilizer of the action. We will dress it up a bit more than that, though.

First we need some new notation. If $(A, G, \phi)$ is a right group action, denote the collection of orbits by $A/G$. If $(B, H, \psi)$ is a left group action, we will denote its collection of orbits by $H\B$.

As we established above, if $H$ is a subgroup of $G$, we can define the *left multiplication action $(G, H, L)$. The orbits of this action are denoted by $G\H$. Note that the orbit of any $g \in G$, is the set $\{hg : h \in H\}$, which is otherwise known as the **right coset** $Hg$ of $H$, a standard construction in group theory. So this left multiplication action is the disjoint union of $G$-subsets over right cosets.

And now for something different. We can define a group action $(H\G, G, \eta)$ on the set $H\G$ itself via $(g \eta): Hx \mapsto Hxg$. Even though this is an action on the set of orbits, the action has orbits itself. We've already proved that every group action is the disjoint union of orbits, so this new action is as well. However, in this case the action is irreducible, meaning there's only one orbit: the hand $(x^{-1}y \eta)$ maps $Hx$ to $Hy$. This $G$-set defined on the right cosets of $H$ is called a **homogeneous space**.

For any group action $(A, G, \phi)$ and $t \in A$, we can form a **stabilizer of $t$** defined by $G_t = \{ g \in G : t \phi(g) = t \}$, the subset of group elements whose hands fix $t$. It's clearly a subgroup of $G$ because the composition of two $t$-fixing maps also fixes $t$, the identity fixes $t$, and if a hand fixes $t$ its inverse must as well.

**Proposition:** If $(A, G, \phi)$ is an irreducible group action on $A \neq \emptyset$, then it is isomorphic to the homogeneous space of any stabilizer $G_t$.

*Proof:* It is most convenient to introduce notation $S_t^x$ for $t, x \in A$ to be the subset of $G$ of group elements whose hands map $t \mapsto x$. Note $G_t = S_t^t$. We prove that for all $s \in S_t^x$, $G_t s = S_t^x$: for any $g \in G_t$, $t [(gs) \phi] = t (g \phi) (s \phi) = t (s \phi) = x$, so $G_t s = S_t^x$. Conversely, if $s_1 \in S_t^x$, then $s_1 s^{-1} \in G_t$ and $(s_1 s^-1}) s = s_1$, so $S_t^x \subseteq G_t s$.

Now, saying the group action is irreducible is equivalent to saying that the set being acted upon is non-empty and that group action is **transitive**, meaning for any $a, b \in A$ there's a $g \in G$ with $a (\phi g) = b$. This ensures that each $S_t^x$ is non-empty. 

We've already proven above that we can define a group action $(G_t \ G, G, \eta)$ on the right cosets. We now define a $G$-homomorphism $f: A \to G_t \G$ by $a \mapsto S_t^a$. This is well-defined because each $S_t^a$ is inhabited (cf. the previous paragraph), so that every $S_t^a$ is a right coset. This is a homomorphism because for any $g \in G, a \in A$, $a (g \phi) f = S_t^{a (g \phi)} = S_t^a (g \eta) = (af) (g \eta)$. $f$ is clearly injective since  if $S_t^a$ and $S_t^b$ are non-empty, they  must be distinct. Also surjective since every right coset is an $S_t^x$ for some $x$. This establishes the isomorphism. $\Box$
