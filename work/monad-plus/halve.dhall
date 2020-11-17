let OddEven = < Odd : Natural | Even : Natural >

let incr =
      λ(x : OddEven) →
        merge
          { Odd = λ(k : Natural) → OddEven.Even (k + 1)
          , Even = λ(k : Natural) → OddEven.Odd k
          }
          x

let halve = λ(x : Natural) → Natural/fold x OddEven incr (OddEven.Even 0)

in  λ(a : Type) →
    λ(f : Natural → a) →
    λ(g : Natural → a) →
    λ(x : Natural) →
      merge { Odd = f, Even = g } (halve x)
