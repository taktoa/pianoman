{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | FIXME: doc
module Utility.PianoMan (module Utility.PianoMan) where

import           Pipes            ((>->))
import qualified Pipes            as P
import qualified Pipes.Prelude    as P

import qualified Pipes.ByteString as PB
import qualified Pipes.Safe       as PS

import           Data.Text        (Text)
import qualified Data.Text        as Text

import           Data.Sequence    (Seq)
import qualified Data.Sequence    as Seq

import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as BS

import qualified Data.Foldable    as Foldable
import           Data.Monoid

import qualified System.ZMQ4      as Z

import           Control.Lens     hiding (Context)

import           Control.Monad

import           Pipes.ZMQ4

type Address = Text

type Subscription = ByteString

zmqProducer :: (MonadSafe m, Base m ~ IO)
               => Address
               -> Seq Subscription
               -> Producer ByteString m ()
zmqProducer addr subs = (liftIO Z.context >>= producer) >-> P.concat
  where
    producer ctx = setupProducer ctx Z.Sub start
    start s = mapM_ (Z.subscribe s) subs >> Z.connect s (Text.unpack addr)

zmqAddress :: Text
zmqAddress = "ipc://teamspeak.ipc"

stdoutLn :: MonadIO m => Consumer ByteString m ()
stdoutLn = P.map (<> "\n") >-> PB.stdout

teamspeak :: (MonadSafe m, Base m ~ IO) => Producer ByteString m ()
teamspeak = zmqProducer zmqAddress [""]

-- | FIXME: doc
main :: IO ()
main = runSafeT $ runEffect (teamspeak >-> stdoutLn)

