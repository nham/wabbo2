---
title: Normal subgroups
tags: math, group theory
---

I'm going to try to present two views on normal subgroups, an algebraic view and a geometric view.

A **group congruence** is an equivalence relation on a group that enables you to define a group product on the equivalence classes. How we define this group product is by picking any two representatives from two classes, multiplying them via the original group product, and then taking the equivalence class of the result. In symbols:

$$ [x] \cdot [y] = [xy]$$

In order for this to be well-defined, we must get the same result via any two choices of representatives. So for any $z \sim x$ and $w \sim y$, $zw \sim xy$.

Now let's think about cosets. For any subgroup $H$ of a group $G$, we can form (left) coset of $g$ by defining $gH := \{gh : h \in H\}$. It is easy to prove that the left cosets of a subgroup partition the group, and hence we have an equivalence relation. Can we say anything about which subgroups give us a congruence relation instead of a mere equivalence relation?

If the left cosets of $H$ form a congruence, then for any $a, b \in G$, and for any $x \in aH$, $y \in bH$, then $xy \in (ab)H$. In particular, if $h \in H$, then $w = ghg^{-1}$ and $wg \in gH$, so  $w = (wg)g^{-1} \in (g g^{-1}) H = H$. If we allow that we can define, for any subset $S$ of a group and any element $x$ of the group, the set $xS = \{xs : s \in S\}$ and $Sx = \{sx : s \in S\}$, then we have proven that if the left cosets of $H$ form a congruence, then $gHg^{-1} \subseteq H$.

Conversely, if $gHg^{-1} \subseteq H$, then let $x \in aH$ and $y \in bH$. Then we need to prove $xy \in (ab)H$. So we must find a $z \in H$ with $xy = abz$, but by unique solvability of equations in groups the only option is $z = b^{-1} a^{-1} x y$. To prove that $z \in H$, note that $y = b b^{-1} y$, so

$$z = b^{-1} a^{-1} x b b^{-1} y$$

But $a^{-1} x$ and $b^{-1} y$ are both in $H$, and since $H$ is closed under conjugation, $b^{-1} (a^{-1} x) b$ is in $H$ as well, so $z$ is clearly in $H$.

So that solves that, right? To determine whether a group's left cosets induce a congruence relation, just check if the subgroup is closed under conjugation. This condition is both necessary and sufficient.

Well, that's nice, but we're not done because there is another perspective.
