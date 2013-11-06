---
title: Zippers in haskell
tags: haskell
---

The starting point is [LYAH's article on zippers](http://learnyouahaskell.com/zippers). I'm going to cut to the chase and dump some code on you.

First, a datatype for a binary tree, which should be clear:

    data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

    data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
    type Breadcrumbs a = [Crumb a]

    goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
    goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs) 

    goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
    goRight (Node x l r, bs) = (r, RightCrumb x l:bs)  

    goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
    goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)  
    goUp (t, RightCrumb x l:bs) = (Node x l t, bs)  


<Description goes here>

A Crumb records 1) which road we took, 2) the value of the node and 3) a copy of the road not taken

We can generalize the binary trees above to trees of arbitrary numbers of children. The implementation actually becomes simpler:


    data Tree a = Node a [Tree a] deriving (Show)

    data Crumb a = Crumb a [Tree a] [Tree a] deriving (Show)
    type Breadcrumbs a = [Crumb a]

    goDown :: Int -> (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
    goDown n (Node v ch, bs) = (ch !! n, Crumb v (take (n-1) ch) (drop n ch) : bs) 

    goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
    goUp (t, Crumb v t1 t2:bs) = (Node x t1 ++ [t] ++ t2 , bs)  


This `(Tree a, Breadcrumbs a)` concept seems important, we should probably define a datatype for it

    type Zipper a = (Tree a, Breadcrumbs a)

The following function applies a function to the root of the tree currently being focused by a Zipper.

    modify :: (a -> a) -> Zipper a -> Zipper a
    modify f (Node v ch, bs) = (Node (f v) ch, bs)

    attach :: Tree a -> Zipper a -> Zipper a


List zipper.


Maybe turn this into a "toy XMonad" implementation post instead.
