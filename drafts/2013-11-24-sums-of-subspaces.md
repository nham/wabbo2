
## Complements

Since this post isn't long enough, I'll cram one more fact in about finite dimensional things. The **sum** of a collection of subspaces of a vector space is the span of their union, i.e. the smallest subspace containing all of them. Unlike the case for intersection, the union of subspaces generally isn't a subspace.

By definition, this is the collection of all linear combinations of vectors belonging to the subspaces, but we can get away with less, actually: since every vector in the union of subspaces is an element of some subspace, all the scalar multiplications of that vector are in the subspace, and hence in the union. So the union is already closed under scalar multiplication, we just need to ensure that the set is closed under sums of vectors from different subspaces. So the sum of a collection of subspaces can be alternately characterized as the collection of linear combinations of the union of subspaces such that the coefficient of each vector in each combination is $1$.
