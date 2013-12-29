A vector space is an abelian group of *vectors* combined with a field of *scalars* together which an operation of *scalar multiplication* such that the following hold:

a(u + v) = au + av
(a + b)u = au + bu
1u = u
a(bu) = (ab)u

Examples (collapsable)
 - F^n
 - mxn matrices
 - for any set X, the set of functions X \to F (generalizes the previous two)
 - set of all polynomials

A subspace is a subset of a vector space which forms a vector space under the set-theoretic restriction of the VS operations to the subset.

[Detailed derivation] - since properties like associativity, commutativity, identity automatically hold for restrictions, we are only checking that the subset is closed under vector addition and scalar multiplication.

Examples (collapsable)
 - solution set of homogeneous system
 - diagonal matrices, upper/lower triangular matrices. symmetric matrices.
 - polynomials of degree no greater than N


You can extend the "vector addition" operation to be done on any multiset of vectors since the order of vector addition does not matter.


the lattice of subspaces
 - the intersection of any collection of subspaces is a subspace. it is the greatest subspace contained in every subspace.
 - the union is not generally a subspace, but we still desire the "dual" of the intersection, the smallest subspace that contains every subspace. this is provided by the *sum* of subspaces.
    * abstract definition: the intersection of all subspaces that contain the union. this is a direct translation of our desire for a dual to the intersection

    * concrete definition: note that the union of subspaces is closed under scalar multiplication. so if it fails to be a subspace, it is because it is not closed under vector addition. for vectors in the same subspace it is automatically closed, so we must only be missing closure for addition of vectors from distinct subspaces. (this is hairy to define formally) pick any finite subcollection of subspaces and pick a vector from each subspace. the vector addition.

 - we can now form the lattice of all subspaces. it is a complete lattice since any collection of subspaces has both a sup and an inf


 - the **span** of a set of vectors is the smallest subspace containing the set.
