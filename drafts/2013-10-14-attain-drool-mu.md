---
title: Attain Drool Mu: A Monad Tutorial
---



This is not an introduction to monads in Haskell. As such, I assume that you have familiarity with Haskell syntax, including pattern matching, currying, basic types, algebraic data types, builtin typeclasses like Eq, Ord, Show, etc.  Also, you should know how to define your own type classes.

There are no pictures or flimsy metaphors in this tutorial. We introduce the Functors, Applicatives  and Monads, describe the methods involved for each and all the laws that types should meet. The proper way to build intuition about something abstract is to look at lots of examples, so that is our aim here.

The reason, I think, that monads seem difficult is that they are abstract, and many programmers are not used to working at such a level of abstraction. Other monad tutorials attempt to route around this by invoking fanciful metaphors, but this is a waste of time.

We must bounce back and forth between the general and the specific, seeing how the specific adheres to the general and how the general abstracts away from the details of the specific.


List monad example

let z n = replicate n (if even n then "cat" else "dog")
map z [1,2,3,4]
