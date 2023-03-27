# "Intro" to Haskell

My attempts to work through the course exercises from and mini-projects
inspired by the University of Chicago's introduction to programming course
sequence.  I put "Intro" in quotes because this course would be
impenetrable to anyone without some previous exposure to functional
programming.  Working through the book [Learn You A Haskell for Great Good][1]
should be sufficient.  The posted lectures and assigned exercises for this
course seem excellent.  College honors courses tend to be just shallow
accelerated versions of the regular course for people who already know the
material.  This one, I think, actually is successful at being deeper.

  [1]: http://learnyouahaskell.com "Learn You A Haskell for Great Good"

## CMSC course whose online Lecture notes I am following

* Honors Introduction to Programming, I
  * Autumn Quarter, 2017
  * Univ. of Chicago
  * Profs. Stuart A. Kurtz & Ravi Chugh
  * [CMSC-16100](http://cmsc-16100.cs.uchicago.edu/2017/)

### Modules written following CMSC-16100

* Lecture I
  * [Triangles](modules/Triangles.hs)
* Lecture II
  * [ListFunctions](modules/ListFunctions.hs)

### Additional Lectures for CMSC-16100

* [Peano Arithmrtic](PeanoArithmetic/)

## Additional play

* [Examples](examples/README.md), basically a place for code snippets.

## Original invitation to community on Haskell-Cafe

```text
   Message: 2
   Date: Sun, 24 Sep 2017 10:52:03 -0500
   From: "Stuart A. Kurtz" <stuart@cs.uchicago.edu>
   To: erwig <erwig@oregonstate.edu>
   Cc: haskell-cafe@haskell.org
   Subject: Re: [Haskell-cafe] EduHaskell
   Message-ID: <E83334CC-2FD1-4BCB-8BC9-3CEFC14D4F59@cs.uchicago.edu>
   Content-Type: text/plain; charset=us-ascii

   Dear Martin,

   I also teach Haskell to college freshmen, albeit in a self-selected "honors"
   class. My typical student has some programming background, and a high level
   of mathematical aptitude.

   The first few weeks were a bit easier in the days before burning bridges,
   but my sense is the things are starting to improve again. Certainly, I think
   the benefits of the improved Prelude far outweigh the pedagogical costs
   imposed on the proud few, who like us, are crazy enough to teach Haskell to
   18 year olds.

   1. The error messages in ghc 8.2.1 strike me as clearer and more concise.
   2. I'm changing the way I teach.

   My experience in teaching Haskell is that have to introduce types early, and
   the only real question is "how early." We're experimenting with introducing
   types (including simple parametric types and typeclasses) in Lecture 1
   (tomorrow) this year, in what my colleague Ravi Chugh and I refer to as
   the "types earliest" approach.

   What we're aiming for in Lecture 1 is an overview, not a deep understanding
   of Haskell's type system. Still, we *are* introducing typeclasses (albeit
   without talking about how they're implemented), and we are sketching out how
   type inference works, so students can see how conflicts arise.

   I'd send a URL, but it won't be stable until tomorrow.

   Peace,

   Stu

   ---------------

   Stuart A. Kurtz
   Professor, Department of Computer Science and the College
   Director of Undergraduate Studies for Computer Science
   The University of Chicago
```

```text
   Message: 3
   Date: Sun, 24 Sep 2017 19:19:24 -0500
   From: "Stuart A. Kurtz" <stuart@cs.uchicago.edu>
   To: erwig <erwig@oregonstate.edu>
   Cc: haskell-cafe@haskell.org
   Subject: Re: [Haskell-cafe] EduHaskell
   Message-ID: <D5B7AB4A-A144-4DAB-87A5-23C22213BE28@cs.uchicago.edu>
   Content-Type: text/plain; charset=us-ascii

   Dear Martin,

   Here's the URL for our first lecture:

      http://cmsc-16100.cs.uchicago.edu/2017/Lectures/01/intro.php

   It is idiosyncratic, but then, so am I ;-). My experience is that Chicago
   students love little bits of history that help contextualize what they're
   learning, and these notes reflect that.

   We'll be putting up additional lectures as we go. It's pretty easy to find
   the 2016 version of the class, but it looks like we'll be reworking much of
   the material.

   Peace,

   Stu

   ---------------

   Stuart A. Kurtz
   Professor, Department of Computer Science and the College
   Director of Undergraduate Studies for Computer Science
   The University of Chicago
```
