    let Level  = < Inactive : {} | Support : {} | Amazing : {} >
in  let mkPatron = \(name    : Text                         ) ->
                   \(twitter : Optional Text                ) ->
                   \(level   : Level                        ) ->
                      { name = name
                      , info = { twitter = twitter, level = level }
                      }
in  [ mkPatron "Domen Kožar"
               (Some "iElectric")
               (Level.Inactive {=})
    , mkPatron "Sam Stites"
               (None Text)
               (Level.Amazing {=})
    , mkPatron "Finpan Halpenny"
               (None Text)
               (Level.Support {=})
    , mkPatron "Josh Miller"
               (None Text)
               (Level.Support {=})
    , mkPatron "Josh Vera"
               (None Text)
               (Level.Amazing {=})
    , mkPatron "Shae Erisson"
               (None Text)
               (Level.Support {=})
    , mkPatron "Heneli Kailahi"
               (None Text)
               (Level.Support {=})
    ]
