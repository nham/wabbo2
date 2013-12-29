# Notes on linear systems of equations

## Prerequisites: 
 - vector spaces, subspaces
 - the span of a set of vectors
 - the Steinitz exchange lemma and the definition of finite dimensional vector spaces


## Systems of linear equations

A system of linear equations is a collection of $n$ equations in $k$ variables:

$$\begin{aligned}
\sum_1^k a_{1i} x_i & = c_1 \\
\vdots \\
\sum_1^k a_{ni} x_i & = c_n
\end{aligned}$$

If every $c_i = 0$, the system is said to be *homogeneous*.

A system has a *trivial* solution when $x_i = 0$ for all $i$ solves it. Every homogenous system has at least the trivial solution. One question is whether there are any other solutions.

**A poverty of constraints:** For any homogeneous system, if the number of variables exceeds the number of equations, then there is a non-trivial solution.

*Proof:* If you look closely, we can group the scalars attached to each variable together into column vectors so that asking whether there are non-trivial solutions to a homogeneous system is the same as asking whether there are non-trivial combinations of the columns that result in zero, i.e. whether the columns are linearly dependent. But the dimension of the columns vectors is less than the number of vectors, so they could not be linearly independent by the Steinitz exchange lemma. $\Box$

## Enter the matrix (whoa)

We introduce matrices for a more compact representation of sytems of linear equations.

$$\begin{bmatrix} a_{11} & \cdots & a_{1n} & c_1 \\
\vdots & \ddots & \vdots & \vdots \\
a_{ki} & \cdots & a_{kn} & c_k \end{bmatrix}$$
