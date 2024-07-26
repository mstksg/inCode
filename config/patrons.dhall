let Level = < Inactive | Support | Amazing >

let mkPatron =
      λ(name : Text) →
      λ(twitter : Optional Text) →
      λ(level : Level) →
        { name, info = { twitter, level } }

in  [ mkPatron "Josh Vera" (Some "joshvera") Level.Amazing
    , mkPatron "Austin Huang" (Some "austinvhuang") Level.Support
    , mkPatron "Jon" (None Text) Level.Support
    , mkPatron "Josh Miller" (None Text) Level.Support
    , mkPatron "Andrew Handley-Marsh" (None Text) Level.Support
    , mkPatron "Domen Kožar" (Some "domenkozar") Level.Support
    , mkPatron "Fintan Halpenny" (None Text) Level.Support
    , mkPatron "Shae Erisson" (Some "shapr") Level.Support
    , mkPatron "Amir Saeid" (Some "gluegadget") Level.Support
    , mkPatron "Julie Moronuki" (Some "argumatronic") Level.Support
    , mkPatron "Sam Stites" (Some "Sam Stites") Level.Support
    , mkPatron "Chris Penner" (Some "opticsbyexample") Level.Inactive
    , mkPatron "Jan Hrček" (Some "_honza") Level.Inactive
    , mkPatron "Sam Raker" (None Text) Level.Inactive
    ]
