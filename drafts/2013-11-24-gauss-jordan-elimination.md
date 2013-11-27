---
title: Gauss-Jordan elimination for fun and profit
tags: math, linear algebra, haskell
---

There is a fundamental fact that we use but don't prove here: that if two systems of linear equations have augmented matrices with the same row space, then the systems must have the same solution set.

The idea is to apply transformations to a given augmented matrix that don't change to row space. What we end up with is an augmented matrix whose associated system has the same solution set as what we started out with. One idea behind Gauss-Jordan elimination is to apply the necessary transformations so that the system we end up with is immediately solvable.



##########


Two systems' augmented matrices having the same row space implies that they have the exact same set of solutions. Elementary row operations preserve the row space, so by applying any sequence of elementary row operations to an augmented matrix, we obtain an augmented matrix whose associated system has exactly the same set of solutions.

By careful application of elementary row operations, we can transform a system of linear equations into another system which a) has the same solution set but b) is easier to solve. This is not the whole story, because merely Gauss-eliminated matrices are already fairly easy to solve, so why go through the trouble of doing the complete Gauss-Jordan elimination?
