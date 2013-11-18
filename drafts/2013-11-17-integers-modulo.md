Some brief notes on the ring $\mathbb{Z}_p$ for $p \in \mathbb{Z}$, $p > 1$. First of all, it is straightforward and boring to prove that $(\mathbb{Z}_p, +, \cdot)$ with the usual addition/multiplication defined on the residue classes $[n]$ of congruence modulo $p$ forms a commutative, unital ring. The question is when it forms a field. Note that we are only missing the property that every non-zero has a multiplicative inverse, so we are interested in necessary and sufficient conditions for this situation.

One direction is not too hard via the concept of zero divisors. A ring $(R, +, \cdot)$  has **zero divisors** $a$ and $b$ if $ab = 0$ with $a \neq 0$ and $b \neq 0$. We are interested in ruling this possibility out because it is very unlike how normal numbers behave. Indeed, zero divisors are impossible in any field: if $ab = 0$ and $a \neq 0$, then $0 a^{-1}ab = b$. So zero divisors are impossible.

This leads to a proof that if $\mathbb{Z}_p$ is a field then $p$ must be prime: if $p$ isn't prime, there exist $a, b$ both greater than $1$ such that $p = ab$. Hence $[a] \cdot [b] = [0]$, so $\mathbb{Z}_p$ has zero divisors and hence could not be a field.

The other way involves the group of units.
