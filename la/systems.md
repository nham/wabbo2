# Notes on linear systems of equations

## Prerequisites: 
 - vector spaces, subspaces
 - the span of a set of vectors
 - linear independence of vectors
 - the Steinitz exchange lemma and the definition of finite dimensional vector spaces
 - matrix multiplication


## Systems of linear equations

A system of linear equations is a collection of $n$ equations in $k$ variables:

$$\begin{aligned}
\sum_1^k a_{1i} x_i & = c_1 \\
\vdots \\
\sum_1^k a_{ni} x_i & = c_n
\end{aligned}$$

The set of all tuples $(x_1, \ldots, x_k)$ that make each equation hold is called the **solution set** of the system. If every $c_i = 0$, the system is said to be *homogeneous*.

A system has a *trivial* solution when $x_i = 0$ for all $i$ solves it. Every homogenous system has at least the trivial solution. One question is whether there are any other solutions.

**A poverty of constraints:** For any homogeneous system, if the number of variables exceeds the number of equations, then there is a non-trivial solution.

*Proof:* If you look closely, we can group the scalars attached to each variable $x_i$ together into column vectors. You should be able to see that asking whether there are non-trivial solutions to a homogeneous system is the same as asking whether there are non-trivial combinations of the columns that result in zero, i.e. whether the columns are linearly dependent. But the dimension of the columns vectors is less than the number of vectors, so they could not be linearly independent by the Steinitz exchange lemma. $\Box$

## Enter the matrix (whoa)

We introduce matrices for a more compact representation of sytems of linear equations.

For a system:

$$\begin{aligned}
\sum_1^k a_{1i} x_i & = c_1 \\
\vdots \\
\sum_1^k a_{ni} x_i & = c_n
\end{aligned}$$

We define these matrices:

$$A = \begin{bmatrix} a_{11} & \cdots & a_{1k} \\
\vdots & \ddots & \vdots \\
a_{n1} & \cdots & a_{nk} \end{bmatrix}$$

$$X = \begin{bmatrix} x_1 \\
\vdots \\
x_k \end{bmatrix}$$

$$C = \begin{bmatrix} c_1 \\
\vdots \\
c_n \end{bmatrix}$$

Then solution set is the set of $X$ such that $AX = C$, where the operation used in that equation is matrix multiplication.

We can also define the **augmented matrix** of the system, which adds the right-hand side of the system as an additional column in the matrix $A$:

$$A_{aug} = \begin{bmatrix} a_{11} & \cdots & a_{1k} & c_1\\
\vdots & \ddots & \vdots & \vdots \\
a_{n1} & \cdots & a_{nk} & c_n \end{bmatrix}$$

The **row space** of any $n \times k$ matrix with scalars in $\mathbb{F}$ is the subspace of $\mathbb{F}^k$ spanned by the rows of the matrix (taken as vectors in $\mathbb{F}^k$. The **row rank** is the dimension of the row space.
