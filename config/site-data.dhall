{ title          = "in Code"
, desc = "Weblog of Justin Le, covering various adventures in programming and "
      ++ "explorations in the vast worlds of computation physics, and knowledge."
, authorInfo     = ./site-data/author-info.dhall
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
, developerAPIs  = ./site-data/developer-apis.dhall
, blogPrefs      = ./site-data/blog-prefs.dhall
, envType        =
    let EnvType = constructors < Development : {} | Production : {} >
    in  EnvType.Production {=}
}
