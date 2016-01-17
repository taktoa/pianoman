{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | FIXME:doc
module Utility.PianoMan.Types where

import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq

import           Data.Int
import           Data.Word

import           Data.ByteString
import           Data.Text

type AnyID = Word16
type Path = Text

data Event = ConnectStatusChange { connectstatuschange_schandlerID :: Word64
                                 , connectstatuschange_newStatus   :: Int
                                 , connectstatuschange_errorNumber :: Word64 }
           | NewChannel { newchannel_schandlerID     :: Word64
                        , newchannel_channelID       :: Word64
                        , newchannel_channelParentID :: Word64 }
           | NewChannelCreated { newchannelcreated_schandlerID  :: Word64
                               , newchannelcreated_channelID    :: Word64
                               , newchannelcreated_cparentID    :: Word64
                               , newchannelcreated_invokerID    :: AnyID
                               , newchannelcreated_invokerName  :: Text
                               , newchannelcreated_iuidentifier :: Text }
           | DelChannel { delchannel_schandlerID  :: Word64
                        , delchannel_channelID    :: Word64
                        , delchannel_invokerID    :: AnyID
                        , delchannel_invokerName  :: Text
                        , delchannel_iuidentifier :: Text }
           | ChannelMove { channelmove_schandlerID  :: Word64
                         , channelmove_channelID    :: Word64
                         , channelmove_ncparentID   :: Word64
                         , channelmove_invokerID    :: AnyID
                         , channelmove_invokerName  :: Text
                         , channelmove_iuidentifier :: Text }
           | UpdateChannel { updatechannel_schandlerID :: Word64
                           , updatechannel_channelID   :: Word64 }
           | UpdateChannelEdited { updatechanneledited_schandlerID  :: Word64
                                 , updatechanneledited_channelID    :: Word64
                                 , updatechanneledited_invokerID    :: AnyID
                                 , updatechanneledited_invokerName  :: Text
                                 , updatechanneledited_iuidentifier :: Text }
           | UpdateClient { updateclient_schandlerID  :: Word64
                          , updateclient_clientID     :: AnyID
                          , updateclient_invokerID    :: AnyID
                          , updateclient_invokerName  :: Text
                          , updateclient_iuidentifier :: Text }
           | ClientMove { clientmove_schandlerID  :: Word64
                        , clientmove_clientID     :: AnyID
                        , clientmove_oldChannelID :: Word64
                        , clientmove_newChannelID :: Word64
                        , clientmove_visibility   :: Int
                        , clientmove_moveMessage  :: Text }
           | ClientMoveSubscription { clientmovesub_schandler :: Word64
                                    , clientmove_clientID     :: AnyID
                                    , clientmove_oldChannelID :: Word64
                                    , clientmove_newChannelID :: Word64
                                    , clientmove_visibility   :: Int }
           | ClientMoveTimeout { clientmoveto_schandler      :: Word64
                               , clientmoveto_clientID       :: AnyID
                               , clientmoveto_oldChannelID   :: Word64
                               , clientmoveto_newChannelID   :: Word64
                               , clientmoveto_visibility     :: Int
                               , clientmoveto_timeoutMessage :: Text }
           | ClientMoveMoved { clientmovemoved_schandler    :: Word64
                             , clientmovemoved_clientID     :: AnyID
                             , clientmovemoved_oldChannelID :: Word64
                             , clientmovemoved_newChannelID :: Word64
                             , clientmovemoved_visibility   :: Int
                             , clientmovemoved_moverID      :: AnyID
                             , clientmovemoved_moverName    :: Text
                             , clientmovemoved_moveruident  :: Text
                             , clientmovemoved_moveMessage  :: Text }
           | ClientKickFromChannel { clientkfc_schandler    :: Word64
                                   , clientkfc_clientID     :: AnyID
                                   , clientkfc_oldChannelID :: Word64
                                   , clientkfc_newChannelID :: Word64
                                   , clientkfc_visibility   :: Int
                                   , clientkfc_kickerID     :: AnyID
                                   , clientkfc_kickerName   :: Text
                                   , clientkfc_kickeruident :: Text
                                   , clientkfc_kickMessage  :: Text }
           | ClientKickFromServer { clientkfs_schandler    :: Word64
                                  , clientkfs_clientid     :: AnyID
                                  , clientkfs_oldChannelID :: Word64
                                  , clientkfs_newChannelID :: Word64
                                  , clientkfs_visibility   :: Int
                                  , clientkfs_kickerID     :: AnyID
                                  , clientkfs_kickerName   :: Text
                                  , clientkfs_kickeruident :: Text
                                  , clientkfs_kickMessage  :: Text }
           | ClientIDs { clientids_schandler         :: Word64
                       , clientids_uniqueClientIdent :: Text
                       , clientids_clientID          :: AnyID
                       , clientids_clientName        :: Text }
           | ClientIDsFinished { clientidsfinished_schandler :: Word64 }
           | ServerEdited { serveredited_schandler    :: Word64
                          , serveredited_editerID     :: AnyID
                          , serveredited_editerName   :: Text
                          , serveredited_editeruident :: Text }
           | ServerUpdated { serverupdated_schandler :: Word64 }
           | ServerError { servererror_schandler    :: Word64
                         , servererror_errorMessage :: Text
                         , servererror_error        :: Word64
                         , servererror_returnCode   :: Text
                         , servererror_extraMessage :: Text }
           | ServerStop { serverstop_schandler       :: Word64
                        , serverstop_shutdownMessage :: Text }
           | TextMessage { textmessage_schandler  :: Word64
                         , textmessage_targetMode :: AnyID
                         , textmessage_toID       :: AnyID
                         , textmessage_fromID     :: AnyID
                         , textmessage_fromName   :: Text
                         , textmessage_fromuident :: Text
                         , textmessage_message    :: Text
                         , textmessage_ffIgnored  :: Int }
           | TalkStatusChange { talkstatuschange_schandler         :: Word64
                              , talkstatuschange_status            :: Int
                              , talkstatuschange_isReceivedWhisper :: Int
                              , talkstatuschange_clientID          :: AnyID }
           | ConnectionInfo { connectioninfo_schandler :: Word64
                            , connectioninfo_clientID  :: AnyID }
           | ServerConnectionInfo { sconnectioninfo_schandler :: Word64 }
           | ChannelSubscribe { channelsubscribe_schandler :: Word64
                              , channelsubscribe_channelID :: Word64 }
           | ChannelSubscribeFinished { channelsubscribefinished_schandler :: Word64 }
           | ChannelUnsubscribe { channelunsubscribe_schandler :: Word64
                                , channelunsubscribe_channelID :: Word64 }
           | ChannelUnsubscribeFinished { channelunsubscribefinished_schandler :: Word64 }
           | ChannelDescriptionUpdate { channeldescriptionupdate_schandler :: Word64
                                      , channeldescriptionupdate_channelID :: Word64 }
           | ChannelPasswordChanged { channelpasswordchanged_schandler :: Word64
                                    , channelpasswordchanged_channelID :: Word64 }
           | PlaybackShutdownComplete { playbackshutdowncomplete_schandler :: Word64 }
           | SoundDeviceListChanged { sounddevicelistchanged_modeID    :: Text
                                    , sounddevicelistchanged_playOrCap :: Int }
           | UserLoggingMessage { ulogmessage_logMessage        :: Text
                                , ulogmessage_logLevel          :: Int
                                , ulogmessage_logChannel        :: Text
                                , ulogmessage_logID             :: Word64
                                , ulogmessage_logTime           :: Text
                                , ulogmessage_completeLogString :: Text }
           | ClientBanFromServer { clientbanfromserver_schanlder :: Word64,
                                   clientbanfromserver_clientID :: AnyID,
                                   clientbanfromserver_oldChannelID :: Word64,
                                   clientbanfromserver_newChannelID :: Word64,
                                   clientbanfromserver_visibility :: Int,
                                   clientbanfromserver_kickerID :: AnyID,
                                   clientbanfromserver_kickerName :: Text,
                                   clientbanfromserver_kickeruident :: Text,
                                   clientbanfromserver_time :: Word64,
                                   clientbanfromserver_kickMessage :: Text }
           | ClientPoke { clientpoke_schandler :: Word64,
                          clientpoke_fromClientID :: AnyID,
                          clientpoke_pokerName :: Text,
                          clientpoke_pokeruident :: Text,
                          clientpoke_message :: Text,
                          clientpoke_ffIgnored :: Int }
           | ClientSelfVariableUpdate { csvariableupdate_schandler :: Word64,
                                        csvariableupdate_flag :: Int,
                                        csvariableupdate_oldValue :: Text,
                                        csvariableupdate_newValue :: Text, }
           | FileList { filelist_schandler :: Word64,
                        filelist_channelID :: Word64,
                        filelist_path :: Path,
                        filelist_name :: Text,
                        filelist_size :: Word64,
                        filelist_datetime :: Word64,
                        filelist_type :: Int,
                        filelist_incompletesize :: Word64,
                        filelist_returnCode :: Text }
           | FileListFinished { filelistfinished_schandler :: Word64,
                                filelistfinished_channelID :: Word64,
                                filelistfinished_path :: Path }
           | FileInfo { fileinfo_schandler :: Word64,
                        fileinfo_channelID :: Word64,
                        fileinfo_name :: Text,
                        fileinfo_size :: Word64,
                        fileinfo_datetime:: Word64 }
           | ServerGroupList { sglist_schandler :: Word64,
                               sglist_sgID :: Word64,
                               sglist_name :: Text,
                               sglist_type :: Int,
                               sglist_iconID :: Int,
                               sglist_saveDB :: Int }
           | ServerGroupListFinished { sglistfinished_schandler :: Word64 }
           | ServerGroupByClientID { sgbyclient_schandler :: Word64,
                                     sgbyclient_name :: Text,
                                     sgbyclient_clientDBID :: Word64 }
           | ServerGroupPermList { sgpermlist_schandler :: Word64,
                                   sgpermlist_sgID :: Word64,
                                   sgpermlist_permissionID :: Word64,
                                   sgpermlist_permissionValue :: Int,
                                   sgpermlist_permissionNegated :: Int,
                                   sgpermlist_permissionSkip :: Int }
           | ServerGroupPermListFinished { sgpermlistfinished_schandler :: Word64,
                                           sgpermlistfinished_sgID :: Word64 }
           | ServerGroupCLientList { sgclientlist_schandler :: Word64,
                                     sgclientlist_sgID :: Word64,
                                     sgclientlist_clientDBID :: Word64,
                                     sgclientlist_clientNameIdent :: Text,
                                     sgclientlist_clientuID :: Text }
           | ChannelGroupList { cglist_schandler :: Word64,
                                cglist_channelGroupID :: Word64,
                                cglist_name :: Text,
                                cglist_type :: Int,
                                cglist_iconID :: Int,
                                cglist_saveDB :: Int }
           | ChannelGroupListFinished  { cglistfinished_schandler :: Word64 }
