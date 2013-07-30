module Web.Blog.Routes (routes) where

import Web.Scotty

routes :: ScottyM ()
routes = do
  get "/" $ do
    html "Hello World!"
  get "/entry/:entryId" $ do
    html "Hello Entry!"
