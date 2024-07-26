let Level = < Inactive | Support | Amazing >

let mkPatron =
      λ(name : Text) →
      λ(twitter : Optional Text) →
      λ(level : Level) →
        { name, info = { twitter, level } }

in  [ mkPatron "Josh Vera" (None Text) Level.Amazing
    , mkPatron "Austin Huang" (None Text) Level.Support
    , mkPatron "Jon" (None Text) Level.Support
    , mkPatron "Josh Miller" (None Text) Level.Support
    , mkPatron "Andrew Handley-Marsh" (None Text) Level.Support
    , mkPatron "Domen Kožar" (None Text) Level.Support
    , mkPatron "Fintan Halpenny" (None Text) Level.Support
    , mkPatron "Shae Erisson" (None Text) Level.Support
    , mkPatron "Amir Saeid" (Some "gluegadget") Level.Support
    , mkPatron "Julie Moronuki" (None Text) Level.Support
    , mkPatron "Sam Stites" (None Text) Level.Support
    , mkPatron "Chris Penner" (Some "opticsbyexample") Level.Inactive
    , mkPatron "Jan Hrček" (None Text) Level.Inactive
    , mkPatron "Sam Raker" (None Text) Level.Inactive
    ]
