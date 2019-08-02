{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Proxy
import           Data.Text (Text)
import           GHC.Generics
import           Servant.API

data Task = Task
    { taskStatus :: Bool
    , taskDesc   :: Text
    }
  deriving (Show, Generic)
instance ToJSON   Task
instance FromJSON Task

type TodoApi =
      "list"   :> QueryFlag "filtered"                      :> Get  '[JSON] (IntMap Task)
 :<|> "add"    :> QueryParam' '[Required] "desc" Text       :> Post '[JSON] Int
 :<|> "set"    :> Capture "id" Int :> Capture "status" Bool :> Post '[JSON] ()
 :<|> "delete" :> Capture "id" Int                          :> Post '[JSON] ()
 :<|> "prune"                                               :> Post '[JSON] IntSet

todoApi :: Proxy TodoApi
todoApi = Proxy
