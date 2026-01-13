---
title: Five Point Haskell
categories: Haskell
tags: functional programming, type safety
create-time: 2025/07/21 10:51:22
identifier: five-point-haskell
slug: five-point-haskell
---

<!-- Um this is a post idea i have to compare the principles of five-point calvinism -->
<!-- with Haskell. -->

<!-- here is what i workshopped with chat gpt -->

<!-- ### **1️⃣ TOTAL DEPRAVITY** -->

<!-- **(a) Theological Nuance** -->
<!-- Human nature is not as evil as possible, but *every faculty* is affected such that salvation cannot arise from human ability alone. -->

<!-- **(b) Haskell Interpretation** -->
<!-- Human reasoning alone cannot reliably maintain correctness as systems grow; intuition, discipline, and testing cannot scale to match system complexity. -->

<!-- **(c) Haskell Nuance** -->
<!-- The failure is epistemic, not moral — informal reasoning grows linearly while system complexity grows superlinearly, making correctness impossible without externalized guarantees (types, proofs, algebraic structure). -->

<!-- **(d) Opposed Programming Heresy** -->
<!-- **The Heresy of Human Heroism:** the belief that correctness can be sustained by expertise, care, or testing *without* formal guarantees. -->

<!-- --- -->

<!-- ### **2️⃣ UNCONDITIONAL ELECTION** -->

<!-- **(a) Theological Nuance** -->
<!-- Acceptance does not depend on foreseen effort, merit, or performance; it is decreed by criteria outside human accomplishment. -->

<!-- **(b) Haskell Interpretation** -->
<!-- A term’s legitimacy is determined *solely* by whether it inhabits a valid type derivation before execution, not by runtime success, demonstrated behavior, or developer intention. -->

<!-- **(c) Haskell Nuance** -->
<!-- Stronger typing (e.g. dependent types, refinements, GADTs) extends the domain of election but preserves the principle: **nothing becomes acceptable at runtime**. -->

<!-- **(d) Opposed Programming Heresy** -->
<!-- **The Heresy of Runtime Vindication:** the belief that a program deserves acceptance because “it works,” rather than because it typechecks. -->

<!-- --- -->

<!-- ### **3️⃣ LIMITED ATONEMENT** -->

<!-- **(a) Theological Nuance** -->
<!-- Atonement is perfectly effective for its intended recipients, but not universally applied to everyone everywhere. -->

<!-- **(b) Haskell Interpretation** -->
<!-- Purity is perfectly reliable *within* its intended realm, but Haskell recognizes a necessary and permanent boundary between pure expressions and effectful interaction. -->

<!-- **(c) Haskell Nuance** -->
<!-- Effects must be *bounded, typed, named, and disciplined* (IO, ST, STM, capabilities), not hand-waved away; the goal is not universal purification but correct partitioning. -->

<!-- **(d) Opposed Programming Heresy** -->
<!-- **The Heresy of Universal Purification:** the belief that *all* software concerns can or should be made pure, and that effect boundaries are optional or embarrassing. -->

<!-- --- -->

<!-- ### **4️⃣ IRRESISTIBLE GRACE** -->

<!-- **(a) Theological Nuance** -->
<!-- Saving grace is not merely offered; it is effectual — what it intends, it accomplishes. -->

<!-- **(b) Haskell Interpretation** -->
<!-- When the right abstraction is properly formulated and enforced, **correctness becomes inevitable by construction**, not by vigilance. -->

<!-- **(c) Haskell Nuance** -->
<!-- Correctness is achieved by eliminating invalid states from the representable space (parametricity, state-transition types, linearity, capability-aware APIs, algebraic interfaces), not by detecting errors *after* writing code. -->

<!-- **(d) Opposed Programming Heresy** -->
<!-- **The Heresy of Heroic Correctness:** the belief that correctness is earned through testing, review, or discipline instead of *designed into* the representable domain. -->

<!-- --- -->

<!-- ### **5️⃣ PERSEVERANCE OF THE SAINTS** -->

<!-- **(a) Theological Nuance** -->
<!-- Those who are effectually called are preserved; they endure to the end according to their intended purpose. -->

<!-- **(b) Haskell Interpretation** -->
<!-- Once an invariant is encoded in types or interfaces, it survives scaling, refactoring, optimization, team turnover, and time. -->

<!-- **(c) Haskell Nuance** -->
<!-- Typed resource lifetimes, phantom types, session types, newtypes, and algebraic contracts preserve correctness **regardless of code evolution or human memory**. -->

<!-- **(d) Opposed Programming Heresy** -->
<!-- **The Heresy of Folklore Invariants:** the belief that safety can live in comments, convention, or institutional memory rather than in types that cannot be violated. -->


Uhh I'm going to use this for planning. To make sure we don't accidentally use
the same example for all of them and we have enough to go around.

Total Depravity
---------------

Idea: Any mistake/bug that could be made will eventually be made, don't trust
your mental modeling abilities

Theme: "Postmortems" of real world accidents, programming gore.

*   ID mixups (2022 Atlassian Outage)
*   Phantoms for environments (2017 Digital Ocean Outage)
*   Units (Mars Orbiter Failure)
*   Billion dollar problem --- sigil values
*   Use-after-free --- continuations, Acquire, ResourceT
*   Shotgun validation/parser inside database, accidentally save unvalidated
    data
*   Boolean blindness

Unconditional Election
----------------------

<!-- Idea: The choice of the type's structure will fully determine the values -->
<!-- allowed. Bad states are unrepresentable. -->

<!-- Theme: Type-Driven Development, Prase Don't Validate -->

<!-- *   Boolean blindness/multiple Maybe issues -->
<!-- *   NonEmpty lists -->
<!-- *   State machine requires each step (GADT enforced?) -->
<!-- *   Authorization payloads -->
<!-- *   Higher-kinded data -->
<!-- *   Sized vectors -->

Idea: Parametrically polymorphic code will pre-destine what functions are
possible. Take advantage of it to enforce invariants and what you can write,
instead of using ie refinement types or postconditions.

Theme: Guessing game, what functions are allowed?

Be careful to not frame the programmer as the person who is electing the terms.
Frame it as the type system electing the valid terms

Give away your free will -- just be elective! constraint is liberation.

Oh that's the key: "Constraint is liberation"! it is not by works or foreseen
faith! the more you surrender....the safer you are! principle of least
strength! also can maybe combine with purity in multi-threaded. restrictions
are better.

Principle of Least Strength

*   Parametric polymorphism --- guess the implementation
    *   `[Int] -> [Int]` vs `[a] -> [a]`
    *   Compare with refinement types
*   higher-kinded data, parametric over functors
*   Subtyping via parametric polymorphism
*   Phantoms + parametric polymoirphism, restrictions, ST trick
*   Princple of least strength, Monad vs Applicative
    *   typeclass-based limitation of functions

Limited Atonement
-----------------

Idea: Effects must be bounded, typed, named, disciplined (IO, ST, STM, StateT,
capabilities), the goal is not universal purity but rather correct
partitioning

Theme: Extensible Effects, free monads, etc.

*   StateT instead of IO
*   ST to do mutation in vectors without full IO
*   STM does not allow IO
*   Free monads to describe exactly what actions you want
*   Free monads for mocking
*   Extensible effects to allow you selectively eliminate handlers until you
    are done
*   ReaderT vs global vars
*   Bracket lets you bound effects
*   Error monads vs IO exceptions

Irresistible Grace
------------------

Idea: When you set up your type-safety correctly, the compiler forces you to
handle things appropriately

Distinction from Unconditional election: happens when writing functions, not
when writing types. I guess?

Theme: Sum type branches, GADTs and witnsses, handler based programming,
church encodings?

*   Sum type --- properly require every handler, or else the compiler complains
*   Lists mean you have to check for null
*   GADTs --- the type of the GADT can tell you what you need to handle.
    Message pattern, Expr pattern
*   Church encodings --- each continuation must be addressed
*   Typed holes to help program
*   Instance resolution to auto-derive programs, Deriving Via, etc.

Perseverance of the Saints
---------------------------

Idea: Benefits of Immutability

*   Immutability
*   Sharing --- all new data structures
*   Multi-threaded
*   Locks, STM

<!-- Idea: Well-typed code will survive big refactoring -->

<!-- Theme: Hands-on refactoring, seeing how changes propagate. Build on chained -->
<!-- successive refactors -->

<!-- *   String -> Sum Type/Enum -->
<!-- *   Add new constructor -->
<!-- *   Add new field to record -->
<!-- *   `ToJSON`/`Persist` change -->
<!-- *   Refactor "reason" to be owned by the sum type -->
<!-- *   Property tests to help it all out -->


<!-- ### Squished Pipeline -->

<!-- Along the same lines, there is often the temptation to squish multiple stages -->
<!-- along a pipeline into the same type. -->

<!-- For example, your "checkout" workflow might incrementally set `Maybe` fields: -->

<!-- ```haskell -->
<!-- data Checkout = Checkout -->
<!--     { items :: [Item] -->
<!--     , address :: Maybe Address -->
<!--     , payment :: Maybe Token -->
<!--     } -->
<!-- ``` -->

<!-- You start with an empty `Checkout` state...then you add `[Item]`s...then you -->
<!-- add `Maybe Address`...then you add `Maybe Token` for payment. However, payment -->
<!-- requires an address: -->

<!-- ```haskell -->
<!-- pay :: Checkout -> IO Checkout -->
<!-- pay c = case address c of -->
<!--   Just addr -> do -->
<!--     tok <- processPayment (items c) addr -->
<!--     pure $ c { payment = Just tok } -->
<!--   Nothing -> -- uh.... -->
<!-- ``` -->

<!-- `pay` doesn't _really_ take a `Maybe Address`, it requires an actual `Address`! -->
<!-- Its input type is too "big". This is a subtle manifestation of the same -->
<!-- problems as shotgun parsing: there is no indication in the type about the -->
<!-- actual stage it is in and what operations can legally be done. -->

<!-- To fix this, we can just...not keep them all as the same type. -->

<!-- ```haskell -->
<!-- data PreCheckout = PreCheckout [Item] -->
<!-- data PrePayment = PrePayment [Item] Address -->
<!-- data PostPayment = PostPayment [Item] Address Token -->

<!-- pay :: PrePayment -> IO PostPayment -->
<!-- pay (PrePayment items addr) = do -->
<!--     tok <- processPayment items addr -->
<!--     pure $ PostPayment items addr tok -->
<!-- ``` -->

