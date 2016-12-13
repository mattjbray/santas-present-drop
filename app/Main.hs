{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           Control.Monad.Trans    (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BS
import           Data.Maybe             (fromMaybe, listToMaybe)
import           Data.Monoid
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text.IO           as T
import           GHC.Generics
import           Network.Socket         (withSocketsDo)
import qualified Network.WebSockets     as WS

import           Player                 (Player (Player))
import qualified Player
import           Position               (Position (..))



ms :: Num a => a -> a
ms i = i * 1000


teamName :: Text
teamName = "haskmas"


teamColor :: Text
teamColor = "haskmas"


targetScore :: Int
targetScore = 50


main :: IO ()
main = do
    putStrLn "Connecting..."
    withSocketsDo $ WS.runClient "game.clearercode.com" 80 "/" app


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    state <- newTVarIO initState
    -- Fork a thread that listens for broadcasts of the new state of the world.
    _ <- forkIO $ forever $ listenForNewState conn state
    let sendCommand command = do
            WS.sendTextData conn (encode command)
            threadDelay (ms 110)
    sequence_ (sendCommand <$> [SetName teamName, SetColor teamColor])
    -- Loop forever, chasing the unicorn.
    let loop strategy = do
            s <- readTVarIO state
            let target =
                    case strategy of
                        ScorePoints -> findTarget . gpss . stateWorld $ s
                        VictoryDance satNumber ->
                            Main.position . (!! satNumber) . gpss . stateWorld $
                            s
                us = ourPlayer s
                ourPosition = Player.position us
                step =
                    (Position
                     { x = (x target) - (x ourPosition)
                     , y = (y target) - (y ourPosition)
                     })
                newStrategy =
                    case strategy of
                        ScorePoints ->
                            if Player.score us >= targetScore
                                then VictoryDance 0
                                else ScorePoints
                        VictoryDance satNumber ->
                            if x step == 0 && y step == 0 then
                              VictoryDance $ (satNumber + 1) `mod` 3
                            else
                              VictoryDance satNumber
            sendCommand (Move step)
            loop newStrategy
    -- Start the loop
    loop ScorePoints


listenForNewState :: WS.Connection -> TVar State -> IO ()
listenForNewState conn state = do
    msg <- WS.receiveData conn
    let mWorld = decode (BS.fromStrict (encodeUtf8 msg))
    case mWorld of
        Just world -> do
            let self = findPlayer teamName (players world)
            liftIO $
                atomically $
                modifyTVar
                    state
                    (\s ->
                          (s
                           { stateWorld = world
                           , ourPlayer = fromMaybe (ourPlayer s) self
                           }))
        Nothing -> liftIO $ T.putStrLn $ "Failed to decode: " <> msg


data Strategy
  = ScorePoints
  | VictoryDance Int
  deriving (Eq, Show)


data State = State
    { ourPlayer  :: Player
    , stateWorld :: World
    } deriving ((Show))


data World = World
    { gpss    :: [Satellite]
    , players :: [Player]
    } deriving (Show,Generic,FromJSON)


data Satellite = Satellite
    { distance :: Double
    , position :: Position
    } deriving (Show,Generic,FromJSON)


initState :: State
initState =
    State
    { ourPlayer = Player
      { Player.position = Position
        { x = 0
        , y = 0
        }
      , Player.score = 0
      , Player.name = ""
      }
    , stateWorld = World
      { gpss = []
      , players = []
      }
    }


data Command
    = SetName Text
    | SetColor Text
    | Move Position
    deriving (Show)


instance ToJSON Command where
    toJSON (SetName name) =
        object ["tag" .= ("SetName" :: Text), "contents" .= name]
    toJSON (SetColor color) =
        object ["tag" .= ("SetColor" :: Text), "contents" .= color]
    toJSON (Move pos) =
        object ["tag" .= ("Move" :: Text), "contents" .= toJSON pos]


findPlayer :: Text -> [Player] -> Maybe Player
findPlayer name = listToMaybe . filter ((name ==) . Player.name)


pyt :: Position -> Position -> Double
pyt p1 p2 = sqrt $ (y p2 - y p1) ** 2 + (x p2 - x p1) ** 2


f2 :: Satellite -> Satellite -> [Position]
f2 s1 s2 =
    [ Position
      { x = x4
      , y = y4
      }
    , Position
      { x = x5
      , y = y5
      }]
  where
    d = pyt (position s1) (position s2)
    a = ((distance s1) ** 2 - (distance s2) ** 2 + d ** 2) / (2 * d)
    (x3,y3) =
        ( x (position s1) + a * (x (position s2) - x (position s1)) / d
        , y (position s1) + a * (y (position s2) - y (position s1)) / d)
    h = sqrt $ distance s1 ** 2 - a ** 2
    (x4,y4) =
        ( (x3 + h * (y (position s2) - y (position s1)) / d)
        , (y3 - h * (x (position s2) - x (position s1)) / d))
    (x5,y5) =
        ( (x3 - h * (y (position s2) - y (position s1)) / d)
        , (y3 + h * (x (position s2) - x (position s1)) / d))


findTarget :: [Satellite] -> Position
findTarget satellites =
    let [a,b,c] = satellites
        ps =
            (\l m ->
                  (pyt l m, l)) <$>
            f2 a b <*>
            f2 a c
    in snd $ minimum ps
