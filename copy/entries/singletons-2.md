---
title: "Introduction to Singletons (Part 2)"
categories: Haskell
series: Practical Dependent Types in Haskell
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2017/12/22 10:33:03
date: never
identifier: singletons-2
slug: introduction-to-singletons-2
---

<!-- Ditching the Phantom -->
<!-- -------------------- -->

<!-- Now, sometimes we don't actually care about the state of the door in our type, -->
<!-- and we don't *want* the state of the door in its type.  Our `lockAnyDoor` -->
<!-- function earlier was an example. -->

<!-- We have a couple of options here --- first, we can create a new type -->
<!-- `SomeDoor`, that doesn't have the opened/closed status in its type, but rather -->
<!-- as a runtime value: -->

<!-- ```haskell -->
<!-- data SomeDoor = MkSomeDoor -->
<!--     { someDoorState    :: DoorState -->
<!--     , someDoorMaterial :: String -->
<!--     } -->

<!-- -- or, in GADT syntax -->
<!-- data SomeDoor :: Type where -->
<!--     MkSomeDoor :: -->
<!--       { someDoorState    :: DoorState -->
<!--       , someDoorMaterial :: String -->
<!--       } -> SomeDoor -->
<!-- ``` -->

<!-- We could have actually been using this type the entire time, if we didn't care -->
<!-- about type safety.  In the real world and in real applications, we might have -->
<!-- actually written `SomeDoor` *before* we ever thought about `Door` with a -->
<!-- phantom type.  It's definitely the more typical "standard" Haskell thing. -->

<!-- It's possible to "construct" this from our original typed `Door`, using a smart -->
<!-- constructor/conversion function: -->

<!-- ```haskell -->
<!-- fromDoor :: SingDS s -> Door s -> SomeDoor -->
<!-- fromDoor SOpened (UnsafeMkDoor m) = MkSomeDoor Opened m -->
<!-- formDoor SClosed (UnsafeMkDoor m) = MkSomeDoor Closed m -->
<!-- formDoor SLocked (UnsafeMkDoor m) = MkSomeDoor Locked m -->
<!-- ``` -->

<!-- ### SomeDoor to Door -->

<!-- Now, `SomeDoor` is great.  But because it's a completely different type, we -->
<!-- potentially have to write the same function for both `Door` and `SomeDoor`, -->
<!-- because they have different implementations.  For example: -->

<!-- ```haskell -->
<!-- closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor -->
<!-- closeSomeOpenedDoor (MkSomeDoor Opened m) = Just (MkSomeDoor Closed m) -->
<!-- closeSomeOpenedDoor (MkSomeDoor Closed m) = Nothing -->
<!-- closeSomeOpenedDoor (MkSomeDoor Locked m) = Nothing -->
<!-- ``` -->

<!-- Wouldn't it be nice if we can *re-use* our original `closeDoor`?  This is a toy -->
<!-- example, and in real life, closing a door might have some complicated runtime -->
<!-- logic, and it'd be annoying to have to *re-implement* it for both `SomeDoor` -->
<!-- and `Door`. -->

<!-- #### Converting into an existential -->

<!-- One thing we can do is write a function to convert a `SomeDoor` into a `Door`, -->
<!-- so we can re-use our original `closeDoor`.  We'd convert our `SomeDoor` into a -->
<!-- `Door` to re-use our `closeDoor :: Door 'Opened -> Door 'Closed` on it if -->
<!-- possible! -->

<!-- However, going from `SomeDoor` to `Door s` is slightly trickier in Haskell than -->
<!-- going the other way around.  One trick we often use is a CPS-style existential -->
<!-- type. -->

<!-- The essential concept is that normal Haskell type variables are universally -->
<!-- qualified, meaning that the *caller* can pick how to instantiate `s`.  However, -->
<!-- we want a function where the *function* can pick the `s`, and the caller must -->
<!-- handle whatever `s` is given by the function: -->

<!-- ```haskell -->
<!-- withSomeDoor :: SomeDoor -> (forall s. SingDS s -> Door s -> r) -> r -->
<!-- withSomeDoor (MkSomeDoor Opened m) f = f SOpened (UnsafeMkDoor m) -->
<!-- withSomeDoor (MkSomeDoor Closed m) f = f SClosed (UnsafeMkDoor m) -->
<!-- withSomeDoor (MkSomeDoor Locked m) f = f SLocked (UnsafeMkDoor m) -->
<!-- ``` -->

<!-- Notice the funky CPS-like type signature of `withSomeDoor`.  To use -->
<!-- `withSomeDoor` and access the `Door`, you have to pass in a function to handle -->
<!-- *any possible `s`*.  And, as you can see, the function passed in might be given -->
<!-- an `SOpened`, an `SClosed`, or an `SLocked`.  It has to be able to handle all -->
<!-- three! -->

<!-- Here, we call `s` *existentially quantified*.  The `withSomeDoor` function gets -->
<!-- to pick which `s` to give `f`.  So, the `s` type variable is directly chosen by -->
<!-- the *function*, and not by the caller. -->

<!-- So we can implement `closeSomeOpenedDoor` (and even a `lockAnySomeDoor`) using this -->
<!-- conversion function: -->

<!-- ```haskell -->
<!-- closeSomeOpenedDoor :: SomeDoor -> Maybe (Door 'Closed) -->
<!-- closeSomeOpenedDoor sd = withSomeDoor sd $ \case -->
<!--     SOpened -> \d -> Just (closeDoor d) -->
<!--     SClosed -> \_ -> Nothing -->
<!--     SLocked -> \_ -> Nothing -->

<!-- lockAnySomeDoor :: SomeDoor -> Door 'Locked -->
<!-- lockAnySomeDoor sd = withSomeDoor sd $ \s d -> -->
<!--     lockAnyDoor s d -->
<!-- ``` -->

<!-- #### The Existential Datatype -->

<!-- However, there's another path we can take.  With the power of singletons, we -->
<!-- can actually implement `SomeDoor` *in terms of* `Door`, using an **existential -->
<!-- data type**: -->

<!-- ```haskell -->
<!-- -- using existential constructor syntax -->
<!-- data SomeDoor = forall s. MkSomeDoor (SingDS s) (Door s) -->

<!-- -- or, using GADT syntax (preferred) -->
<!-- !!!singletons/Door.hs "data SomeDoor ::" -->
<!-- ``` -->

<!-- `MkSomeDoor` is a constructor for an existential data type, meaning that the -->
<!-- data type "hides" a type variable `s`. -->

<!-- Hopefully you can see the similarities between our original `SomeDoor` and this -->
<!-- one. -->

<!-- ```haskell -->
<!-- -- Original type -->
<!-- data SomeDoor where -->
<!--     MkSomeDoor :: DoorState -> String -> SomeDoor -->
<!-- -- New existential type -->
<!-- data SomeDoor where -->
<!--     MkSomeDoor :: SingDS s  -> Door s -> SomeDoor -->
<!-- ``` -->

<!-- The key differences are: -->

<!-- *   Our first `SomeDoor` contains a `DoorState`, and this new `SomeDoor` -->
<!--     contains a `SingDS` (a *singleton* for the `DoorState`): -->
<!-- *   Our first `SomeDoor` contains essentially a re-implementation of the `Door` -->
<!--     type, but the new `SomeDoor` contains an actual `Door`, so we can re-use -->
<!--     functions on `Door`s. -->

<!-- In Haskell, existential data types are pretty nice, syntactically, to work -->
<!-- with.  For a comparison, let's re-implement our previous functions with our new -->
<!-- data type: -->

<!-- ```haskell -->
<!-- !!!singletons/Door.hs "closeSomeOpenedDoor ::" "lockAnySomeDoor ::" -->
<!-- ``` -->

<!-- Much more convenient, because *we already have a `Door`!*  And we don't have to -->
<!-- re-implement one like we did for our original `SomeDoor` -- all of our original -->
<!-- code works directly! -->

<!-- It's important to remember that our original separate-implementation `SomeDoor` -->
<!-- is, functionally, identical to the new code-reusing `Door`.  The reason why -->
<!-- they are the same is that *having an existentially quantified singleton is the -->
<!-- same as having a value of the corresponding type.*  Having an existentially -->
<!-- quantified `SingDS s` is *the same as* having a value of type `DoorState`. -->

<!-- If they're identical, why use a `SingDS` or the new `SomeDoor` at all?  One -->
<!-- main reason (besides allowing code-reuse) is that *using the singleton lets us -->
<!-- recover the type*.  Essentially, a `SingDS s` not only contains whether it is -->
<!-- Opened/Closed/Locked...it contains it in a way that GHC can use to *bring it -->
<!-- all back* to the type level. -->

<!-- Basically, `SingDS` allows us to re-use our original `Door s` implementation, -->
<!-- because we store both the `Door`...*and* the `s` at the type level.  You should -->
<!-- read it as storing `s` and `Door s`, together, at runtime.  It also lets GHC -->
<!-- *check* our implementations, to help ensure that they are correct, because you -->
<!-- maintain the `s` at the type level. -->

<!-- #### Some Lingo -->

<!-- In the language of dependently typed programming, we call `SomeDoor` a -->
<!-- **dependent sum**, because you can imagine it basically as: -->

<!-- ```haskell -->
<!-- data SomeDoor = SDOpened (Door 'Opened) -->
<!--               | SDClosed (Door 'Closed) -->
<!--               | SDLocked (Door 'Locked) -->
<!-- ``` -->

<!-- A three-way sum between a `Door 'Opened`, a `Door 'Closed`, and a `Door -->
<!-- 'Locked`, essentially.  If you have a `SomeDoor`, it's *either* an opened door, -->
<!-- a closed door, or a locked door.  Try looking at this new `SomeDoor` until you -->
<!-- realize that this type is the same type as the previous `SomeDoor`! -->

<!-- You might also see `SomeDoor` called a **dependent pair**, because it's -->
<!-- basically an existentially quantified tuple of the type (the `s`, witnessed by -->
<!-- the `SingDS s`) with a value (the `Door s`). -->

<!-- ### Types at Runtime -->

<!-- With this last tool, we finally have enough to build a function to "make" a -->
<!-- door with the status unknown until runtime: -->

<!-- ```haskell -->
<!-- !!!singletons/Door.hs "mkSomeDoor ::" -->
<!-- ``` -->

<!-- ```haskell -->
<!-- ghci> let mySomeDoor = mkSomeDoor Opened "Birch" -->
<!-- ghci> :t mySomeDoor -->
<!-- SomeDoor -->
<!-- ghci> putStrLn $ case mySomeDoor of -->
<!--         MkSomeDoor SOpened _ -> "mySomeDoor was opened!" -->
<!--         MkSomeDoor SClosed _ -> "mySomeDoor was closed!" -->
<!--         MkSomeDoor SLocked _ -> "mySomeDoor was locked!" -->
<!-- mySomeDoor was opened! -->
<!-- ``` -->

<!-- Using `mkSomeDoor`, we can truly pass in a `DoorState` that we generate at -->
<!-- runtime (from IO, or a user prompt, or a configuration file, maybe), and create -->
<!-- a `Door` based on it. -->

<!-- Take *that*, type erasure! :D -->

<!-- We could even directly return a `Door` with an existentially quantified door -->
<!-- status in CPS style: -->

<!-- ```haskell -->
<!-- withDoor :: DoorState -> String -> (forall s. SingDS s -> Door s -> r) -> r -->
<!-- withDoor s m f = case s of -->
<!--     Opened -> f SOpened (UnsafeMkDoor m) -->
<!--     Closed -> f SClosed (UnsafeMkDoor m) -->
<!--     Locked -> f SLocked (UnsafeMkDoor m) -->
<!-- ``` -->

<!-- ```haskell -->
<!-- ghci> withDoor Opened "Birch" $ \s d -> case s of -->
<!--          SOpened -> "Opened door!" -->
<!--          SClosed -> "Closed door!" -->
<!--          SLocked -> "Locked door!" -->
<!-- Opened door! -->
<!-- ``` -->

<!-- This allows us to *truly* directly generate a `Door s` with an `s` that can -->
<!-- vary at runtime. -->

<!-- #### Reification -->

<!-- The general pattern we are exploiting here is called **reification** -- we're -->
<!-- taking a dynamic run-time value, and lifting it to the type level as a type -->
<!-- (here, the type variable `s`).  You can think of reification as the opposite of -->
<!-- reflection, and imagine the two as being the "gateway" between the type-safe -->
<!-- and unsafe world.  In the dynamic world of a `DoorState` value, you have no -->
<!-- type safety.  You live in the world of `SomeDoor`, `closeSomeOpenedDoor`, -->
<!-- `lockAnySomeDoor`, etc.  But, you can *reify* your `DoorState` value to a *type*, and -->
<!-- enter the type-safe world of `Door s`, `closeDoor`, `lockDoor`, and -->
<!-- `lockAnyDoor`. -->

<!-- It might be more meaningful then to write a direct reification function for our -->
<!-- `DoorState`, in CPS style.  Then, we can actually write our `withDoor` in terms -->
<!-- of it! -->

<!-- ```haskell -->
<!-- !!!singletons/Door.hs "withDoorState ::" "withDoor ::" -->
<!-- ``` -->

<!-- ## Sing -->

<!-- *   `toSing :: DoorState -> SomeSing DoorState` takes us from values to their -->
<!--     (existentially quantified) singletons -->

<!--     ```haskell -->
<!--     ghci> let s = toSing Opened -->
<!--     ghci> :t s -->
<!--     s :: SomeSing DoorState -->
<!--     ghci> putStrLn $ case s of -->
<!--             SomeSing SOpened -> "Opened." -->
<!--             SomeSing SClosed -> "SClosed." -->
<!--             SomeSing SLocked -> "SLocked." -->
<!--     "Opened." -->
<!--     ``` -->

<!--     `SomeSing` is like `SomeDoor` in that it is an existentially quantified -->
<!--     singleton: -->

<!--     ```haskell -->
<!--     data SomeSing DoorState :: Type where -->
<!--         SomeSing :: Sing s -> SomeSing DoorState -->

<!--     -- or, more accurately, since `SomeSing` is polykinded -->
<!--     data SomeSing :: k -> Type where -->
<!--         SomeSing :: Sing (a :: k) -> SomeSing k -->
<!--     ``` -->


<!-- 3.  Implement `withSomeDoor` for the existentially quantified `SomeDoor` type. -->

<!--     ```haskell -->
<!--     !!!singletons/DoorSingletons.hs "data SomeDoor" "withSomeDoor ::"1 -->
<!--     ``` -->

<!-- 4.  Implement `openAnySomeDoor`, which should work like `lockAnySomeDoor`, just -->
<!--     wrapping an application of `openAnyDoor` inside a `SomeDoor`. -->

<!--     ```haskell -->
<!--     !!!singletons/DoorSingletons.hs "openAnySomeDoor ::"1 -->
<!--     ``` -->

<!--     You **shouild not** use `UnsafeMkDoor` directly. -->

<!--     Note that because we wrote `openAnyDoor` in "implicit style", we might have -->
<!--     to convert between `SingI s =>` and `Sing s ->` style, using `withSingI`. -->

<!-- However, full expressively with phantom types is still out of our reach.  If we -->
<!-- want to express more complicated relationships and to be able to treat phantom -->
<!-- types (and *types*, in general) as first-class values, and delve into the -->
<!-- frighteningly beautiful world of "type-level programming", we are going to have -->
<!-- to dig a bit deeper.  Come back for the next post to see how!  Singletons will -->
<!-- be our tool, and we'll also see how the singletons library is a very clean -->
<!-- unification of a lot of concepts. -->

