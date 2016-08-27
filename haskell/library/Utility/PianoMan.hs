{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | FIXME: doc
module Utility.PianoMan (module Utility.PianoMan) where

import           Pipes                  ((>->))
import qualified Pipes                  as P
import qualified Pipes.Prelude          as P

import qualified Pipes.ByteString       as PB
import qualified Pipes.Safe             as PS

import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS

import qualified Data.ByteString.Char8  as BSC8 (pack, unpack)

import qualified Data.Foldable          as Foldable
import           Data.Monoid

import qualified System.ZMQ4            as Z

import           Control.Lens           hiding (Context)

import qualified Data.Aeson             as Aeson

import           Control.Monad

import           Pipes.ZMQ4

import           Utility.PianoMan.Types

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
zmqAddress = "ipc://teamspeak.pub"

stdoutLn :: MonadIO m => Consumer ByteString m ()
stdoutLn = P.map (<> "\n") >-> PB.stdout

showLn :: (Show s, MonadIO m) => Consumer s m ()
showLn = P.map (BSC8.pack . show) >-> stdoutLn

data DeserializationError = DeserializationError Text
                          deriving (Eq, Show, Read)
instance Exception DeserializationError

throwDeserializationError :: MonadThrow m => String -> m a
throwDeserializationError = throwM . DeserializationError . Text.pack

teamspeak :: (MonadSafe m, Base m ~ IO) => Producer ByteString m ()
teamspeak = zmqProducer zmqAddress [""]

deserialize :: MonadThrow m => Pipe ByteString RawEvent m ()
deserialize = forever $ do
  input <- await
  case Aeson.eitherDecodeStrict' input of
    Left msg    -> throwDeserializationError msg
    Right event -> yield event

-- | FIXME: doc
main :: IO ()
main = runSafeT $ runEffect (teamspeak >-> deserialize >-> showLn)

