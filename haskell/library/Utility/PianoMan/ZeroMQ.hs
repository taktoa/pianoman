{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | FIXME: doc
module Utility.PianoMan.ZeroMQ () where

import           Pipes
import           Pipes.ByteString
import           Pipes.Safe

import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS

import           System.ZMQ4.Monadic

import           Control.Lens

{-

-- | The kind of transport used by the address.
data Transport = TransportTCP
                 -- ^ A unicast TCP transport.
               | TransportIPC
                 -- ^ A local inter-process communication transport.
               | TransportInProc
                 -- ^ A local intra-process communication transport.
               | TransportPGM
                 -- ^ Reliable, multicast transport over IP with PGM.
               | TransportEPGM
                 -- ^ A transport using PGM encapsulated within UDP datagrams.
               deriving (Eq, Show, Read)

-- | FIXME: doc
data SocketAddress = SocketAddress { transport :: Transport
                                   , address   :: () }
                   deriving (Eq, Show, Read)

-- | FIXME: doc
newtype Subscription = Subscription { _prefix :: ByteString }
                deriving (Eq, Show, Read)
makeLenses ''Subscription

data ZMQClient = ZMQClient { _address       :: Address
                           , _subscriptions :: Seq Subscription
                           }

makeLenses ''ZMQClient

-- | FIXME: doc
toProducer :: (MonadIO m, Base m ~ IO, SocketType sock, Receiver sock)
              => ZMQClient
              -> Producer (Seq ByteString) m ()
toProducer client =
  runZMQ $ do
    s <- socket
    return ()
-- Consumer
-- withCOnt

-}
