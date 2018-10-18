    let Level  = constructors < Support : {} | Amazing : {} >
in  let Patron = { name : Text
                 , info :
                     { twitter: Optional Text
                     , level: < Support : {} | Amazing : {} >
                     }
                 }
in  let mkPatron = \(name    : Text                         ) ->
                   \(twitter : Optional Text                ) ->
                   \(level   : <Support : {} | Amazing : {}>) ->
                      { name = name
                      , info = { twitter = twitter, level = level }
                      } : Patron
in  [ mkPatron "Domen Kožar"
               (Some "iElectric")
               (Level.Support {=})
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
    ] : List Patron
