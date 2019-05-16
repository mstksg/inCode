{ title =
    "in Code"
, desc =
    ''
    Weblog of Justin Le, covering various adventures in programming and explorations in the worlds of computation physics, and knowledge.
    ''
, authorInfo =
    ./site-data/author-info.dhall
, copyright =
    "2018 Justin Le"
, license =
    "CC-BY-NC-ND 3.0"
, licenseLink =
    "https://creativecommons.org/licenses/by-nc-nd/3.0/"
, feed =
    "http://feeds.feedburner.com/incodeblog"
, blobs =
    Some
    { tree =
        "https://github.com/mstksg/inCode/tree"
    , sourceBranch =
        Some "master"
    , renderBranch =
        Some "gh-pages"
    }
, codeSamples =
    Some "code-samples"
, interactive =
    Some "https://www.fpcomplete.com/user/jle/"
, hostInfo =
    { secure =
        True
    , base =
        "blog.jle.im"
    , port =
        None Natural
    , root =
        None Text
    }
, developerAPIs =
    ./site-data/developer-apis.dhall
, blogPrefs =
    ./site-data/blog-prefs.dhall
, envType =
    let EnvType = < Development | Production >

    in  EnvType.Production
}
