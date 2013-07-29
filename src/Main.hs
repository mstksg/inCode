import Web.Scotty
import Web.Blog.Routes
import Web.Blog.Models

main :: IO ()
main = scotty 4268 $ do
  routes
