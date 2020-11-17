let List/concat =
      https://prelude.dhall-lang.org/v17.1.0/List/concat.dhall sha256:54e43278be13276e03bd1afa89e562e94a0a006377ebea7db14c7562b0de292b

let List/map =
      https://prelude.dhall-lang.org/v17.1.0/List/map.dhall sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Optional/map =
      https://prelude.dhall-lang.org/v17.1.0/Optional/map.dhall sha256:501534192d988218d43261c299cc1d1e0b13d25df388937add784778ab0054fa

let List/concatMap =
      https://prelude.dhall-lang.org/v17.1.0/List/concatMap.dhall sha256:3b2167061d11fda1e4f6de0522cbe83e0d5ac4ef5ddf6bb0b2064470c5d3fb64

let indexed =
      https://prelude.dhall-lang.org/v17.1.0/List/indexed.dhall sha256:58bb44457fa81adf26f5123c1b2e8bef0c5aa22dac5fa5ebdfb7da84563b027f

let Map/keyText =
      https://prelude.dhall-lang.org/v17.1.0/Map/keyText.dhall sha256:f7b6c802ca5764d03d5e9a6e48d9cb167c01392f775d9c2c87b83cdaa60ea0cc

let types =
      https://raw.githubusercontent.com/Gabriel439/dhall-dot/master/types.dhall sha256:a6c26efdefa8c54ed2ffc88d7a75c507fe8939aee628b55c345ac26baac95af6

let render =
      https://raw.githubusercontent.com/Gabriel439/dhall-dot/master/render.dhall sha256:2e4d5f0b250aea399cff7d044f56ce4e7b34e329109c64c9a35d3a5feefd03de

let halve = ./halve.dhall

let simpleNode =
      λ(t : Text) →
      λ(lab : Text) →
        { nodeID = { id = t, port = None types.Port }
        , attributes = toMap { label = lab }
        }

let simpleEdge =
      λ(t1 : Text) →
      λ(t2 : Text) →
        { vertices =
          [ types.vertex.nodeID { id = t1, port = None types.Port }
          , types.vertex.nodeID { id = t2, port = None types.Port }
          ]
        , attributes = [] : List types.Attribute
        }

let Node = { nodeID : types.NodeID, attributes : List types.Attribute }

let NodeVal = λ(a : Type) → { val : a, origin : Natural }

let Cluster = λ(a : Type) → List (NodeVal a)

let bindCluster =
      λ(a : Type) →
      λ(b : Type) →
      λ(f : a → List b) →
      λ(c0 : Cluster a) →
        List/concatMap
          { index : Natural, value : NodeVal a }
          (NodeVal b)
          ( λ(iv : { index : Natural, value : NodeVal a }) →
              List/map
                b
                (NodeVal b)
                (λ(nv : b) → { val = nv, origin = iv.index })
                (f iv.value.val)
          )
          (indexed (NodeVal a) c0)

let mapCluster =
      λ(a : Type) →
      λ(b : Type) →
      λ(f : a → b) →
      λ(c0 : Cluster a) →
        List/map
          { index : Natural, value : NodeVal a }
          (NodeVal b)
          ( λ(iv : { index : Natural, value : NodeVal a }) →
              { val = f iv.value.val, origin = iv.index }
          )
          (indexed (NodeVal a) c0)

let subgraph =
      λ(t : Text) →
      λ(nodes : List Node) →
      λ(attrs : List types.Attribute) →
        types.statement.subgraph
          ( types.subgraph
              { id = Some t
              , statements =
                    List/map Node types.Statement types.statement.node nodes
                  # List/map
                      types.Attribute
                      types.Statement
                      types.statement.attribute
                      attrs
              }
          )

let {- fundamental problem here is we need to color the arrow based on origin -}
    renderCluster =
      λ(a : Type) →
      λ(name : Text) →
      λ(originPrefix : Text) →
      λ(prefix : Text) →
      λ(render : a → Text) →
      λ(attrs : List types.Attribute) →
      λ(c : Cluster a) →
        let mapIxC =
              λ(b : Type) →
              λ(f : Natural → NodeVal a → b) →
                List/map
                  { index : Natural, value : NodeVal a }
                  b
                  ( λ(nvi : { index : Natural, value : NodeVal a }) →
                      f nvi.index nvi.value
                  )
                  (indexed (NodeVal a) c)

        in    [ subgraph
                  name
                  ( mapIxC
                      Node
                      ( λ(ix : Natural) →
                        λ(x : NodeVal a) →
                          simpleNode (prefix ++ Natural/show ix) (render x.val)
                      )
                  )
                  attrs
              ]
            # mapIxC
                types.Statement
                ( λ(ix : Natural) →
                  λ(x : NodeVal a) →
                    types.statement.edges
                      ( simpleEdge
                          (originPrefix ++ Natural/show x.origin)
                          (prefix ++ Natural/show ix)
                      )
                )

let halveDoubleFuncs =
      [ halve
          (Optional Natural)
          (λ(_ : Natural) → None Natural)
          (λ(n : Natural) → Some n)
      , λ(x : Natural) → Some (x * 2)
      ]

let cluster0
    : Cluster Natural
    = [ { val = 6, origin = 0 } ]

let cluster1
    : Cluster (Optional Natural)
    = bindCluster
        Natural
        (Optional Natural)
        ( λ(x : Natural) →
            List/map
              (Natural → Optional Natural)
              (Optional Natural)
              (λ(f : Natural → Optional Natural) → f x)
              halveDoubleFuncs
        )
        cluster0

let cluster2
    : Cluster (Optional { xx : Natural, yy : Natural })
    = bindCluster
        (Optional Natural)
        (Optional { xx : Natural, yy : Natural })
        ( λ(x : Optional Natural) →
            List/map
              (Natural → Optional Natural)
              (Optional { xx : Natural, yy : Natural })
              ( λ(f : Natural → Optional Natural) →
                  merge
                    { Some =
                        λ(oldx : Natural) →
                          Optional/map
                            Natural
                            { xx : Natural, yy : Natural }
                            (λ(y : Natural) → { xx = oldx, yy = y })
                            (f oldx)
                    , None = None { xx : Natural, yy : Natural }
                    }
                    x
              )
              halveDoubleFuncs
        )
        cluster1

let cluster3
    : Cluster (Optional Natural)
    = mapCluster
        (Optional { xx : Natural, yy : Natural })
        (Optional Natural)
        ( λ(x : Optional { xx : Natural, yy : Natural }) →
            Optional/map
              { xx : Natural, yy : Natural }
              Natural
              (λ(xy : { xx : Natural, yy : Natural }) → xy.xx)
              x
        )
        cluster2

let halvedouble =
        { strict = False
        , directionality = types.Directionality.digraph
        , id = None types.ID
        , statements =
          [ types.statement.attributes
              { type = types.AttributeType.node
              , attributes = toMap
                  { shape = "none", color = "white", fontname = "palatino" }
              }
          , types.statement.attributes
              { type = types.AttributeType.graph
              , attributes = toMap { fontname = "palatino" }
              }
          , types.statement.attribute (Map/keyText "labeljust" "l")
          , types.statement.attribute (Map/keyText "constraint" "false")
          , subgraph
              "cluster0"
              [ simpleNode "n" "n = 6" ]
              ([] : List types.Attribute)
          , subgraph
              "cluster1"
              [ simpleNode "x2" "Just 12", simpleNode "x1" "Just 3" ]
              ( toMap
                  { label = "x ←"
                  , fontcolor = "blue"
                  , style = "filled"
                  , color = "linen"
                  }
              )
          , subgraph
              "cluster2"
              [ simpleNode "y4" "Just 24"
              , simpleNode "y3" "Just 6"
              , simpleNode "y2" "Just 6"
              ,   simpleNode "y1" "Nothing"
                ⫽ { attributes = toMap
                      { label = "Nothing", fontcolor = "slategray" }
                  }
              ]
              (toMap { fontcolor = "blue", style = "filled", color = "linen" })
          , subgraph
              "cluster3"
              [ simpleNode "r4" "Just 13"
              , simpleNode "r3" "Just 13"
              , simpleNode "r2" "Just 4"
              ,   simpleNode "r1" "Nothing"
                ⫽ { attributes = toMap
                      { label = "Nothing", fontcolor = "slategray" }
                  }
              ]
              ( toMap
                  { label = "return (x + 1)"
                  , labelloc = "b"
                  , fontcolor = "blue"
                  , style = "filled"
                  , color = "linen"
                  }
              )
          , types.statement.edges (simpleEdge "n" "x1")
          , types.statement.edges (simpleEdge "n" "x2")
          , types.statement.edges (simpleEdge "x1" "y1")
          , types.statement.edges (simpleEdge "x1" "y2")
          , types.statement.edges (simpleEdge "x2" "y3")
          , types.statement.edges (simpleEdge "x2" "y4")
          , types.statement.edges
              (   simpleEdge "y1" "r1"
                ⫽ { attributes = toMap { color = "slategray" } }
              )
          , types.statement.edges (simpleEdge "y2" "r2")
          , types.statement.edges (simpleEdge "y3" "r3")
          , types.statement.edges (simpleEdge "y4" "r4")
          ]
        }
      : types.Graph

in  render halvedouble
