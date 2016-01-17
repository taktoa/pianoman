{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | FIXME: doc
module Utility.PianoMan (module Utility.PianoMan) where

import           Pipes
import qualified Pipes.ByteString as PB
import           Pipes.Safe

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
               -> Producer [ByteString] m ()
zmqProducer addr subs = liftIO Z.context >>= mkProducer
  where
    mkProducer ctx = setupProducer ctx Z.Sub start
    start s = do
      mapM_ (Z.subscribe s) subs
      Z.connect s $ Text.unpack addr

toPipe :: Monad m => (a -> b) -> Pipe a b m ()
toPipe f = forever (await >>= return . f >>= yield)

unlinesBS :: [ByteString] -> ByteString
unlinesBS = Foldable.fold . map (<> "\n")

unlinesBSPipe :: Monad m => Pipe [ByteString] ByteString m ()
unlinesBSPipe = toPipe unlinesBS

-- | FIXME: doc
main :: IO ()
main = runSafeT $ runEffect (producer >-> unlinesBSPipe >-> PB.stdout)
  where
    producer = zmqProducer "tcp://localhost:5555" ["10002"]
