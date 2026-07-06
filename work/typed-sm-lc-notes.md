# typed-sm-lc notes (scratch)

- Post file: copy/entries/typed-sm-lc-1.md
  - Title: "Extreme Haskell: Typed State Machines with Typed Lambda Calculus"
  - Identifier: typed-sm-lc
  - Slug: extreme-haskell-typed-state-machines-with-typed-lambda-calculus
  - Code samples referenced via !!! in code-samples/typed-sm-lc

- Code samples folder: code-samples/typed-sm-lc
  - Files: ExprStage1.hs, ExprStage2.hs, ExprStage3.hs, ExprStage4.hs
  - All run via runghc and print the "fifteen" result
  - Nix flake uses GHC 9.12.3 (haskell.packages.ghc9123)
  - Flake.lock updated via nix flake update

- RequiredTypeArguments:
  - Not usable directly in ELambda constructor (needs DependentTypes which is unsupported).
  - Workaround in ExprStage4: eLambda uses RequiredTypeArguments + TypeAbstractions.
    eLambda (type n) x = ELambda @n x

- Records across stages:
  - ExprStage1: ERecord uses Map String Expr for symmetry with env; EAccess via Map lookup.
  - ExprStage2: no records or sums; first type-indexed Expr/eval layer.
  - ExprStage3: James-style Rec records and typed sums.
  - ExprStage4: James-style Rec records.
    - Ty includes TRecord [(Symbol, Ty)]
    - ERecord :: Rec (ExprField vs) as -> Expr vs (TRecord as)
    - EAccess :: KnownSymbol l => Expr vs (TRecord as) -> Index as (l ::: a) -> Expr vs a
    - EVRecord :: Rec EValueField as
    - ExprField/EValueField + mapRec + indexRec

- Narrative notes:
  - Introduce typed state machine graph + typed lambda calculus.
  - Discussion about trigger vs event split: button example uses Click/Bump triggers; IncreaseCount/Enstuck/PryOpen events.
  - Mention graph integration later; typed DSL progression first.
  - TypeData: drop leading tick on TInt/TBool/TString in text and code.

- Renames done:
  - typed-lc-1.md -> typed-sm-lc.md
  - code-samples/typed-lc -> code-samples/typed-sm-lc
  - ExprStage* filenames and module names

- Build/run status:
  - runghc ExprStage1..4 under nix develop works; outputs Just (EPrim (PInt 15)) for Stage1 and 15 for later stages.

```
codex resume 019c3976-0b35-73f3-93f0-f0222646eb62
```
