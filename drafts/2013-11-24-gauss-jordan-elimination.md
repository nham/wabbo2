---
title: Gauss-Jordan elimination for fun and profit
tags: math, linear algebra, haskell
---

There is a fundamental fact that we use but don't prove here: that if two systems of linear equations have augmented matrices with the same row space, then the systems must have the same solution set.

The idea is to apply transformations to a given augmented matrix that don't change to row space. What we end up with is an augmented matrix whose associated system has the same solution set as what we started out with. One idea behind Gauss-Jordan elimination is to apply the necessary transformations so that the system we end up with is immediately solvable.
