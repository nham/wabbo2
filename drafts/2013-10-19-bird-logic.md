---
title: Of talking birds, combinators, applicative functors, and parsing
---

The prerequisites for this post are basic knowledge of Haskell, including pattern matching, higher order functions, algebraic data types and type classes. The first 8 chapters of Learn You a Haskell should suffice. You do *not* need to understand monads. Many introductions to applicative functors assume that you already know about monads, but I'm taking the reverse approach and trying to understand monads by first understanding applicative functors.

There are two resources that don't suppose you've understood monads: Typeclassopedia and LYAH's chapter on Applicatives. However, after reading both of them, I still don't feel like I "get" Applicatives. So I decided to write this post instead in order to teach myself.

# Bird calls

In this section I -steal- borrow Raymond Smullyan's presentation of combinatory logic in terms of birds. You should probably skip this and go read *To Mock a Mockingbird* instead.

There's a forest that's full of talking birds. Each of the birds has a name, and when you say some bird's name to another bird, it responds with the yet another bird's name. For example, if "Steve" and "Emily" are the names of talking birds, and you see Emily in front of you, then saying "Steve" to Emily will cause Emily to respond with the name of some other bird in the forest, for example "Jeffrey".

A key point is that the response is fixed. Every time I say "Steve", Emily responds with "Jeffrey" and only "Jeffrey". It could not be the case that three days from now I say "Steve" to Emily and she responds with "Janet" or "Bartholomew" or any other name besides "Jeffrey".

We also stipulate that *each of the birds has a cell phone*, that *birds only give responses to names said over the phone*, and that *there are no conference calls*. This is needed to avoid difficulties that would arise if we were just saying names out loud and there were multiple birds nearby. It's also funny to think about.

In order to intimidate you, I'm going to introduce some mathematical notation. If $A$ and $B$ are (symbols that represent) birds, then we notate $A$'s response to hearing $B$'s name by $AB$. Note that $AB$ is distinct from $BA$. If $A = Emily$ and $B = Steve$, then $AB$ is Emily's response to hearing "Steve" and $BA$ is Steve's response to hearing "Emily". Now, these might be the same, but they don't *have* to be. In general, for any birds $A$ and $B$, these will be different.

We also note that we can use parentheses to group bird calls. $A(BC)$ denotes $A$'s response to $B$'s response to $C$'s name, while $(AB)C$ denotes the response of the bird $AB$ to hearing $C$'s name. (In other words, $A$ responds with the name of some bird $X$ when it hears $B$'s name, so $(AB)C$ is $X$'s response to hearing $C$'s name). If you think about this carefully you should see that, as above, these two expressions will, in general, be different.

# Mockingbirds and composition

We've already stipulated that we can call up any bird, say any bird name, and get a name back in response. So there is nothing strange about, for example, calling up Emily and saying "Emily". Emily will respond with some name.

A **mockingbird** is a bird $M$ such that, for any other bird $x$, $Mx = xx$. That is, for every bird $x$, $M$'s response to hearing $x$'s name is the same as $x$'s response to hearing its own name.

Bird $A$ is **fond** of bird $B$ if $AB = B$. In other words, if $A$'s response to hearing $B$'s name is $B$. $A$ is **narcissistic** if it is fond of itself, i.e. $AA = A$.
