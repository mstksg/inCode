{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

-- | Chatbot source code, accompanying
-- http://blog.jle.im/entry/auto-building-a-declarative-chatbot-with-implicit-serialization
--
-- dependencies: auto (0.2.0.0+), simpleirc (0.3.0), transformers
--
-- You can run globally:
--
-- $ cabal install auto simpleirc
-- $ runghc chatbot.hs
--
-- Or you can run in a sandbox (recommended)
--
-- $ cabal sandbox init
-- $ cabal install auto simpleirc
-- $ cabal exec runghc chatbot.hs
--
-- Remember to edit `channels` and `conf` with the channels and server/nick
-- you would like to join.

module Main where

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Run         (runOnChanM)
import Control.Auto.Serialize   (serializing')
import Control.Auto.Switch      (resetOn)
import Control.Concurrent       (Chan, newChan, writeChan, forkIO, threadDelay)
import Control.Monad            (void, forever)
import Control.Monad.IO.Class
import Data.Foldable            (forM_)
import Data.Map                 (Map)
import Data.Serialize
import Data.Text hiding         (words, unwords, map)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time
import Network.SimpleIRC
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import qualified Data.Map       as M

channels :: [Channel]
channels = ["#testchan1", "#testchan2"]

conf :: IrcConfig
conf = (mkDefaultConfig "myserver" "mynick") { cChannels = channels }

type Nick    = String
type Channel = String
type Message = String

data InMessage = InMessage { _inMessageNick   :: Nick
                           , _inMessageBody   :: Message
                           , _inMessageSource :: Channel
                           , _inMessageTime   :: UTCTime
                           } deriving Show

newtype OutMessages = OutMessages (Map Channel [Message]) deriving Show

instance Monoid OutMessages where
    mempty  = OutMessages M.empty
    mappend (OutMessages m1) (OutMessages m2)
            = OutMessages (M.unionWith (++) m1 m2)

type ChatBot m = Auto m InMessage OutMessages
type RoomBot m = Auto m InMessage (Blip [Message])

perRoom :: Monad m => RoomBot m -> ChatBot m
perRoom rb = proc inp -> do
    messages <- fromBlips [] . rb -< inp
    id -< OutMessages $ M.singleton (_inMessageSource inp) messages

chatBot :: MonadIO m => ChatBot m
chatBot = serializing' "chatbot.dat"
        . mconcat $ [ perRoom seenBot
                    , perRoom repBot
                    , announceBot channels
                    ]

chatBot' :: MonadIO m => ChatBot m
chatBot' = mconcat [ perRoom . serializing' "seens.dat" $ seenBot
                   , perRoom . serializing' "reps.dat"  $ repBot
                   ,           serializing' "anns.dat"  $ announceBot channels
                   ]

seenBot :: Monad m => RoomBot m
seenBot = proc (InMessage nick msg _ time) -> do
    seens  <- trackSeens -< (nick, time)

    queryB <- queryBlips -< msg

    let respond :: Nick -> [Message]
        respond qry = case M.lookup qry seens of
                        Just t  -> [qry ++ " last seen at " ++ show t ++ "."]
                        Nothing -> ["No record of " ++ qry ++ "."]

    id -< respond <$> queryB
  where
    trackSeens :: Monad m => Auto m (Nick, UTCTime) (Map Nick UTCTime)
    trackSeens = accum (\mp (nick, time) -> M.insert nick time mp) M.empty
    queryBlips :: Auto m Message (Blip Nick)
    queryBlips = emitJusts (getRequest . words)
      where
        getRequest ("@seen":nick:_) = Just nick
        getRequest _                = Nothing

repBot :: Monad m => RoomBot m
repBot = proc (InMessage nick msg _ _) -> do
    updateB <- updateBlips -< (nick, msg)

    reps    <- trackReps   -< updateB

    queryB  <- queryBlips  -< msg

    let lookupRep :: Nick -> [Message]
        lookupRep nick = [nick ++ " has a reputation of " ++ show rep ++ "."]
          where
            rep = M.findWithDefault 0 nick reps

    id -< lookupRep <$> queryB
  where
    updateBlips :: Auto m (Nick, Message) (Blip (Nick, Int))
    updateBlips = emitJusts getUpdateCommand
      where
        -- updater is the person triggering the update blip
        getUpdateCommand (updater, msg) =
          case words msg of
            "@addRep":nick:_ | nick /= updater -> Just (nick, 1)
            "@subRep":nick:_                   -> Just (nick, -1)
            _                                  -> Nothing
    trackReps :: Monad m => Auto m (Blip (Nick, Int)) (Map Nick Int)
    trackReps = scanB (\mp (nick, change) -> M.insertWith (+) nick change mp) M.empty
    queryBlips :: Auto m Message (Blip Nick)
    queryBlips = emitJusts (getRequest . words)
      where
        getRequest ("@rep":nick:_) = Just nick
        getRequest _                = Nothing

announceBot :: Monad m => [Channel] -> ChatBot m
announceBot chans = proc (InMessage nick msg src time) -> do
    announceB <- announceBlips     -< (nick, msg)

    newDayB   <- newDayBlips       -< utctDay time

    annCounts <- resetOn trackAnns -< (nick <$ announceB, newDayB)

    let hasFlooded  = M.findWithDefault 0 nick annCounts > 3

        targetChans :: [Channel]
        targetChans | hasFlooded = [src]
                    | otherwise  = chans

        outB        :: Blip [Message]
        outB        | hasFlooded = [nick ++ ": No flooding!"] <$ announceB
                    | otherwise  = announceB

        outMsgsB    :: Blip OutMessages
        outMsgsB    = (\out -> OutMessages (M.fromList (map (,out) targetChans)))
                  <$> outB

    fromBlips mempty -< outMsgsB
  where
    announceBlips :: Monad m => Auto m (Nick, Message) (Blip [Message])
    announceBlips = emitJusts getAnnounces
      where
        getAnnounces (nick, msg) =
          case words msg of
            "@ann":ann -> Just [nick ++ " says \"" ++ unwords ann ++ "\"."]
            _          -> Nothing
    newDayBlips :: Monad m => Auto m Day (Blip Day)
    newDayBlips = onChange
    trackAnns :: Monad m => Auto m (Blip Nick) (Map Nick Int)
    trackAnns = scanB (\mp nick -> M.insertWith (+) nick 1 mp) M.empty

main :: IO ()
main = do
    withIrcConf conf chatBot
    forever (threadDelay 1000000000)

withIrcConf :: IrcConfig -> ChatBot IO -> IO ()
withIrcConf ircconf chatbot = do

    -- chan to receive `InMessage`s
    inputChan <- newChan :: IO (Chan InMessage)

    -- configuring IRC
    let events   = cEvents ircconf ++ [ Privmsg (onMessage inputChan) ]
        ircconf' = ircconf { cEvents = events }

    -- connect; simplified for demonstration purposes
    Right server <- connect ircconf' True True

    -- run `chatbot` on `inputChan`
    void . forkIO . void $
        runOnChanM id (processOutput server) inputChan chatbot

  where
    -- what to do when `chatBot` outputs
    processOutput :: MIrc -> OutMessages -> IO Bool
    processOutput server (OutMessages outs) = do
      print outs
      _ <- flip M.traverseWithKey outs $ \channel messages -> do
        let channel' = encodeUtf8 . pack $ channel
        forM_ messages $ \message -> do
          let message' = encodeUtf8 . pack $ message
          sendMsg server channel' message'
      return True       -- "yes, continue on"

    -- what to do when you get a new message
    onMessage :: Chan InMessage -> EventFunc
    onMessage inputChan = \_ message -> do
      case (mNick message, mOrigin message) of
        (Just nick, Just src) -> do
          time <- getCurrentTime
          writeChan inputChan $ InMessage (unpack (decodeUtf8 nick))
                                          (unpack (decodeUtf8 (mMsg message)))
                                          (unpack (decodeUtf8 src))
                                          time

instance Serialize UTCTime where
    get = read <$> get      -- haha don't do this in real life.
    put = put . show

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay
