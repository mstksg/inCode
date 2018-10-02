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
               (["iElectric"] : Optional Text)
               (Level.Support {=})
    , mkPatron "Sam Stites"
               ([] : Optional Text)
               (Level.Amazing {=})
    , mkPatron "Finpan Halpenny"
               ([] : Optional Text)
               (Level.Support {=})
    , mkPatron "Josh Miller"
               ([] : Optional Text)
               (Level.Support {=})
    , mkPatron "Josh Vera"
               ([] : Optional Text)
               (Level.Amazing {=})
    , mkPatron "Shae Erisson"
               ([] : Optional Text)
               (Level.Support {=})
    ] : List Patron
