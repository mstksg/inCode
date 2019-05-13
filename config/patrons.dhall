let Level = < Inactive : {} | Support : {} | Amazing : {} >

let mkPatron =
        λ(name : Text)
      → λ(twitter : Optional Text)
      → λ(level : Level)
      → { name = name, info = { twitter = twitter, level = level } }

in  [ mkPatron "Amir Saeid" (None Text) (Level.Support {=})
    , mkPatron "Julie Moronuki" (Some "argumatronic") (Level.Support {=})
    , mkPatron "Sam Stites" (None Text) (Level.Support {=})
    , mkPatron "Finpan Halpenny" (None Text) (Level.Support {=})
    , mkPatron "Heneli Kailahi" (None Text) (Level.Support {=})
    , mkPatron "Josh Miller" (None Text) (Level.Support {=})
    , mkPatron "Josh Vera" (None Text) (Level.Amazing {=})
    ]
