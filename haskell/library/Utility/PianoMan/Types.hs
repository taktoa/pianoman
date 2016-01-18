{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | FIXME:doc
module Utility.PianoMan.Types where

import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq

import           Data.Int
import           Data.Word

import           Data.ByteString
import           Data.Text

import           Data.Aeson

import           GHC.Generics

type AnyID = Word16
type Path = Text

data Event = ConnectStatusChange
             { _ConnectStatusChange_schandlerID :: !Word64
             , _ConnectStatusChange_newStatus   :: !Int
             , _ConnectStatusChange_errorNumber :: !Word64 }
           | NewChannel
             { _NewChannel_schandlerID     :: !Word64
             , _NewChannel_channelID       :: !Word64
             , _NewChannel_channelParentID :: !Word64 }
           | NewChannelCreated
             { _NewChannelCreated_schandlerID :: !Word64
             , _NewChannelCreated_channelID   :: !Word64
             , _NewChannelCreated_cparentID   :: !Word64
             , _NewChannelCreated_invokerID   :: !AnyID
             , _NewChannelCreated_invokerName :: !Text
             , _NewChannelCreated_invokerUID  :: !Text }
           | DelChannel
             { _DelChannel_schandlerID :: !Word64
             , _DelChannel_channelID   :: !Word64
             , _DelChannel_invokerID   :: !AnyID
             , _DelChannel_invokerName :: !Text
             , _DelChannel_invokerUID  :: !Text }
           | ChannelMove
             { _ChannelMove_schandlerID :: !Word64
             , _ChannelMove_channelID   :: !Word64
             , _ChannelMove_ncparentID  :: !Word64
             , _ChannelMove_invokerID   :: !AnyID
             , _ChannelMove_invokerName :: !Text
             , _ChannelMove_invokerUID  :: !Text }
           | UpdateChannel
             { _UpdateChannel_schandlerID :: !Word64
             , _UpdateChannel_channelID   :: !Word64 }
           | UpdateChannelEdited
             { _UpdateChannelEdited_schandlerID :: !Word64
             , _UpdateChannelEdited_channelID   :: !Word64
             , _UpdateChannelEdited_invokerID   :: !AnyID
             , _UpdateChannelEdited_invokerName :: !Text
             , _UpdateChannelEdited_invokerUID  :: !Text }
           | UpdateClient
             { _UpdateClient_schandlerID :: !Word64
             , _UpdateClient_clientID    :: !AnyID
             , _UpdateClient_invokerID   :: !AnyID
             , _UpdateClient_invokerName :: !Text
             , _UpdateClient_invokerUID  :: !Text }
           | ClientMove
             { _ClientMove_schandlerID  :: !Word64
             , _ClientMove_clientID     :: !AnyID
             , _ClientMove_oldChannelID :: !Word64
             , _ClientMove_newChannelID :: !Word64
             , _ClientMove_visibility   :: !Int32
             , _ClientMove_moveMessage  :: !Text }
           | ClientMoveSubscription
             { _ClientMoveSubscription_schandlerID  :: !Word64
             , _ClientMoveSubscription_clientID     :: !AnyID
             , _ClientMoveSubscription_oldChannelID :: !Word64
             , _ClientMoveSubscription_newChannelID :: !Word64
             , _ClientMoveSubscription_visibility   :: !Int32 }
           | ClientMoveTimeout
             { _ClientMoveTimeout_schandlerID    :: !Word64
             , _ClientMoveTimeout_clientID       :: !AnyID
             , _ClientMoveTimeout_oldChannelID   :: !Word64
             , _ClientMoveTimeout_newChannelID   :: !Word64
             , _ClientMoveTimeout_visibility     :: !Int32
             , _ClientMoveTimeout_timeoutMessage :: !Text }
           | ClientMoveMoved
             { _ClientMoveMoved_schandlerID  :: !Word64
             , _ClientMoveMoved_clientID     :: !AnyID
             , _ClientMoveMoved_oldChannelID :: !Word64
             , _ClientMoveMoved_newChannelID :: !Word64
             , _ClientMoveMoved_visibility   :: !Int32
             , _ClientMoveMoved_moverID      :: !AnyID
             , _ClientMoveMoved_moverName    :: !Text
             , _ClientMoveMoved_moverUID     :: !Text
             , _ClientMoveMoved_moveMessage  :: !Text }
           | ClientKickFromChannel
             { _ClientKickFromChannel_schandlerID  :: !Word64
             , _ClientKickFromChannel_clientID     :: !AnyID
             , _ClientKickFromChannel_oldChannelID :: !Word64
             , _ClientKickFromChannel_newChannelID :: !Word64
             , _ClientKickFromChannel_visibility   :: !Int32
             , _ClientKickFromChannel_kickerID     :: !AnyID
             , _ClientKickFromChannel_kickerName   :: !Text
             , _ClientKickFromChannel_kickerUID    :: !Text
             , _ClientKickFromChannel_kickMessage  :: !Text }
           | ClientKickFromServer
             { _ClientKickFromServer_schandlerID  :: !Word64
             , _ClientKickFromServer_clientID     :: !AnyID
             , _ClientKickFromServer_oldChannelID :: !Word64
             , _ClientKickFromServer_newChannelID :: !Word64
             , _ClientKickFromServer_visibility   :: !Int32
             , _ClientKickFromServer_kickerID     :: !AnyID
             , _ClientKickFromServer_kickerName   :: !Text
             , _ClientKickFromServer_kickerUID    :: !Text
             , _ClientKickFromServer_kickMessage  :: !Text }
           | ClientIDs
             { _ClientIDs_schandlerID    :: !Word64
             , _ClientIDs_uniqueClientID :: !Text
             , _ClientIDs_clientID       :: !AnyID
             , _ClientIDs_clientName     :: !Text }
           | ClientIDsFinished
             { _ClientIDsFinished_schandlerID :: !Word64 }
           | ServerEdited
             { _ServerEdited_schandlerID :: !Word64
             , _ServerEdited_editerID    :: !AnyID
             , _ServerEdited_editerName  :: !Text
             , _ServerEdited_editerUID   :: !Text }
           | ServerUpdated
             { _ServerUpdated_schandlerID :: !Word64 }
           | ServerError
             { _ServerError_schandlerID  :: !Word64
             , _ServerError_errorMessage :: !Text
             , _ServerError_error        :: !Word64
             , _ServerError_returnCode   :: !Text
             , _ServerError_extraMessage :: !Text }
           | ServerStop
             { _ServerStop_schandlerID     :: !Word64
             , _ServerStop_shutdownMessage :: !Text }
           | TextMessage
             { _TextMessage_schandlerID :: !Word64
             , _TextMessage_targetMode  :: !AnyID
             , _TextMessage_toID        :: !AnyID
             , _TextMessage_fromID      :: !AnyID
             , _TextMessage_fromName    :: !Text
             , _TextMessage_fromUID     :: !Text
             , _TextMessage_message     :: !Text
             , _TextMessage_ffIgnored   :: !Int32 }
           | TalkStatusChange
             { _TalkStatusChange_schandlerID       :: !Word64
             , _TalkStatusChange_status            :: !Int32
             , _TalkStatusChange_isReceivedWhisper :: !Int32
             , _TalkStatusChange_clientID          :: !AnyID }
           | ConnectionInfo
             { _ConnectionInfo_schandlerID :: !Word64
             , _ConnectionInfo_clientID    :: !AnyID }
           | ServerConnectionInfo
             { _ServerConnectionInfo_schandlerID :: !Word64 }
           | ChannelSubscribe
             { _ChannelSubscribe_schandlerID :: !Word64
             , _ChannelSubscribe_channelID   :: !Word64 }
           | ChannelSubscribeFinished
             { _ChannelSubscribeFinished_schandlerID :: !Word64 }
           | ChannelUnsubscribe
             { _ChannelUnsubscribe_schandlerID :: !Word64
             , _ChannelUnsubscribe_channelID   :: !Word64 }
           | ChannelUnsubscribeFinished
             { _ChannelUnsubscribeFinished_schandlerID :: !Word64 }
           | ChannelDescriptionUpdate
             { _ChannelDescriptionUpdate_schandlerID :: !Word64
             , _ChannelDescriptionUpdate_channelID   :: !Word64 }
           | ChannelPasswordChanged
             { _ChannelPasswordChanged_schandlerID :: !Word64
             , _ChannelPasswordChanged_channelID   :: !Word64 }
           | PlaybackShutdownComplete
             { _PlaybackShutdownComplete_schandlerID :: !Word64 }
           | SoundDeviceListChanged
             { _SoundDeviceListChanged_modeID    :: !Text
             , _SoundDeviceListChanged_playOrCap :: !Int32 }
           | UserLoggingMessage
             { _UserLoggingMessage_logMessage        :: !Text
             , _UserLoggingMessage_logLevel          :: !Int32
             , _UserLoggingMessage_logChannel        :: !Text
             , _UserLoggingMessage_logID             :: !Word64
             , _UserLoggingMessage_logTime           :: !Text
             , _UserLoggingMessage_completeLogString :: !Text }
           | ClientBanFromServer
             { _ClientBanFromServer_schandlerID  :: !Word64
             , _ClientBanFromServer_clientID     :: !AnyID
             , _ClientBanFromServer_oldChannelID :: !Word64
             , _ClientBanFromServer_newChannelID :: !Word64
             , _ClientBanFromServer_visibility   :: !Int32
             , _ClientBanFromServer_kickerID     :: !AnyID
             , _ClientBanFromServer_kickerName   :: !Text
             , _ClientBanFromServer_kickerUID    :: !Text
             , _ClientBanFromServer_time         :: !Word64
             , _ClientBanFromServer_kickMessage  :: !Text }
           | ClientPoke
             { _ClientPoke_schandlerID  :: !Word64
             , _ClientPoke_fromClientID :: !AnyID
             , _ClientPoke_pokerName    :: !Text
             , _ClientPoke_pokerUID     :: !Text
             , _ClientPoke_message      :: !Text
             , _ClientPoke_ffIgnored    :: !Int32 }
           | ClientSelfVariableUpdate
             { _ClientSelfVariableUpdate_schandlerID :: !Word64
             , _ClientSelfVariableUpdate_flag        :: !Int32
             , _ClientSelfVariableUpdate_oldValue    :: !Text
             , _ClientSelfVariableUpdate_newValue    :: !Text }
           | FileList
             { _FileList_schandlerID    :: !Word64
             , _FileList_channelID      :: !Word64
             , _FileList_path           :: !Path
             , _FileList_name           :: !Text
             , _FileList_size           :: !Word64
             , _FileList_datetime       :: !Word64
             , _FileList_type           :: !Int32
             , _FileList_incompletesize :: !Word64
             , _FileList_returnCode     :: !Text }
           | FileListFinished
             { _FileListFinished_schandlerID :: !Word64
             , _FileListFinished_channelID   :: !Word64
             , _FileListFinished_path        :: !Path }
           | FileInfo
             { _FileInfo_schandlerID :: !Word64
             , _FileInfo_channelID   :: !Word64
             , _FileInfo_name        :: !Text
             , _FileInfo_size        :: !Word64
             , _FileInfo_datetime    :: !Word64 }
           | ServerGroupList
             { _ServerGroupList_schandlerID :: !Word64
             , _ServerGroupList_sgID        :: !Word64
             , _ServerGroupList_name        :: !Text
             , _ServerGroupList_type        :: !Int32
             , _ServerGroupList_iconID      :: !Int32
             , _ServerGroupList_saveDB      :: !Int32 }
           | ServerGroupListFinished
             { _ServerGroupListFinished_schandlerID :: !Word64 }
           | ServerGroupByClientID
             { _ServerGroupByClientID_schandlerID :: !Word64
             , _ServerGroupByClientID_name        :: !Text
             , _ServerGroupByClientID_clientDBID  :: !Word64 }
           | ServerGroupPermList
             { _ServerGroupPermList_schandlerID       :: !Word64
             , _ServerGroupPermList_sgID              :: !Word64
             , _ServerGroupPermList_permissionID      :: !Word64
             , _ServerGroupPermList_permissionValue   :: !Int32
             , _ServerGroupPermList_permissionNegated :: !Int32
             , _ServerGroupPermList_permissionSkip    :: !Int32 }
           | ServerGroupPermListFinished
             { _ServerGroupPermListFinished_schandlerID :: !Word64
             , _ServerGroupPermListFinished_sgID        :: !Word64 }
           | ServerGroupClientList
             { _ServerGroupClientList_schandlerID  :: !Word64
             , _ServerGroupClientList_sgID         :: !Word64
             , _ServerGroupClientList_clientDBID   :: !Word64
             , _ServerGroupClientList_clientNameID :: !Text
             , _ServerGroupClientList_clientUID    :: !Text }
           | ChannelGroupList
             { _ChannelGroupList_schandlerID    :: !Word64
             , _ChannelGroupList_channelGroupID :: !Word64
             , _ChannelGroupList_name           :: !Text
             , _ChannelGroupList_type           :: !Int32
             , _ChannelGroupList_iconID         :: !Int32
             , _ChannelGroupList_saveDB         :: !Int32 }
           | ChannelGroupListFinished
             { _ChannelGroupListFinished_schandlerID :: !Word64 }
           deriving (Eq, Generic, Show, Read)

instance FromJSON Event

instance ToJSON Event
