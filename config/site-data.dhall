{ title = "in Code"
, desc = "Weblog of Justin Le, covering his various adventures in programming "
      ++ "and explorations in the vast worlds of computation physics, and knowledge."
, authorInfo =
    { name     = "Justin Le"
    , email    = "justin@jle.im"
    , rel      = "https://plus.google.com/107705320197444500140"
    , twitter  = "mstk"
    , gPlus    = "+JustinLe"
    , github   = "mstksg"
    , linkedIn = "lejustin"
    , keybase  = "mstksg"
    , coinbase = "mstksg"
    , bitcoin  = "3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU"
    , patreon  = "justinle"
    , twitch   = "justin_l"
    }
, copyright      = "2018 Justin Le"
, license        = "CC-BY-NC-ND 3.0"
, licenseLink    = "https://creativecommons.org/licenses/by-nc-nd/3.0/"
, feed           = "http://feeds.feedburner.com/incodeblog"
, blobs          =
    let Blobs = { tree : Text
                , sourceBranch : Optional Text
                , renderBranch : Optional Text
                }
    in  [{ tree         = "https://github.com/mstksg/inCode/tree"
         , sourceBranch = (["master"]   : Optional Text)
         , renderBranch = (["gh-pages"] : Optional Text)
         }] : Optional Blobs
, codeSamples    = ["code-samples"] : Optional Text
, interactive    = ["https://www.fpcomplete.com/user/jle/"] : Optional Text
, hostInfo       =
    { secure = True
    , base   = "blog.jle.im"
    , port   = [] : Optional Natural
    , root   = [] : Optional Text
    }
, developerAPIs  =
    { analytics  =
        { _1 = "UA-443711-8"
        , _2 = "jle.im"
        }
    , disqus     = "incode"
    , facebook   = "641852699171929"
    , addThis    = "ra-5234d67a6b68dcd4"
    , feedburner = "incodeblog"
    , flattr     = "3p9jqr"
    }
, blogPrefs      =
    { slugLength     = 8
    , homeEntries    = 8
    , ledeMax        = 6
    , feedEntries    = 10
    , sidebarEntries = 5
    }
, envType        =
    let EnvType = constructors < Development : {} | Production : {} >
    in  EnvType.Production {=}
}

