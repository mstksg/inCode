let Level = < Inactive | Support | Amazing >

let mkPatron =
      λ(name : Text) →
      λ(twitter : Optional Text) →
      λ(level : Level) →
        { name, info = { twitter, level } }

in  [ mkPatron "Josh Vera" (None Text) Level.Amazing
    , mkPatron "Josh Miller" (None Text) Level.Support
    , mkPatron "Chris Penner" (Some "opticsbyexample") Level.Support
    , mkPatron "Jan Hrček" (None Text) Level.Support
    , mkPatron "Sam Raker" (None Text) Level.Support
    , mkPatron "Julie Moronuki" (None Text) Level.Support
    , mkPatron "Amir Saeid" (Some "gluegadget") Level.Support
    , mkPatron "Sam Stites" (None Text) Level.Support
    ]
